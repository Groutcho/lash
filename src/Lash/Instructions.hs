{-# LANGUAGE OverloadedStrings #-}
module Lash.Instructions where

import           Lash
import           Lash.AST

import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad                (when)
import           Data.List                    (intercalate)
import           Data.Maybe                   (isNothing, fromJust, isJust)
import           Data.Monoid                  ((<>))
import           Data.Conduit
import           Data.ByteString              (ByteString)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Process as CP
import           System.Directory             (findExecutable)
import           System.Exit                  (ExitCode)
import           System.IO
import           System.Process
import           System.Posix.Env             (setEnv)
import           Text.Printf                  (printf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Prelude hiding (Word, putStrLn)

assignX = Assign (Name "x") (Unquoted "valueX")
assignY = Assign (Name "y") (Unquoted "valueY")
cat f = Command (Unquoted "cat") [Unquoted f] []
grep x = Command (Unquoted "grep") [Unquoted x] []

pg0 = [ ClosedStdin
      , PushHandle stdout
      , PushHandle stderr
      -- , cat "lash.cabal"
      , cmd' "ls" ["-l"]
      , RunPendingActions
      ]

pg1 = [ Assign (Name "x") (Unquoted "valueX")
      , Export (Name "x")
      , ClosedStdin
      , Pipe Output
      , PushHandle stderr
      , cat "lash.cabal"
      , Pipe Output
      , PushHandle stderr
      , grep "a"
      , PushHandle stdout
      , PushHandle stderr
      , Command (Unquoted "wc") [Unquoted "-l"] [ IOFile Nothing IOTo (Unquoted "wc-write.txt")
                                                , IOFile Nothing IOAppend (Unquoted "wc-append.txt")
                                                ]
      ]

class Compilable a where
  compile :: a -> [Instruction]

instance Compilable Assignment where
  compile (Assignment n w) = [Assign n w]

instance Compilable Command where
  compile (SimpleCommand name args ios) = [Command name args ios]

data Instruction = Assign Name Word
                 | ClosedStdin
                 | Export Name
                 | Command Word [Word] [IOFile]
                 | PushHandle Handle
                 | Pipe ConduitTarget
                 | RunPendingActions
                 deriving (Show)

data ConduitTarget = Input | Output | Error deriving (Show, Eq, Ord)

cmd :: T.Text -> Instruction
cmd c = Command (Unquoted c) [] []

cmd' :: T.Text -> [T.Text] -> Instruction
cmd' c a = Command (Unquoted c) (Unquoted <$> a) []

getReadable :: Instruction -> T.Text
getReadable (Assign (Name n) w)  = "Assign    " <> n <> " := " <> (getValue w)
getReadable (Export (Name n))    = "Export    " <> n
getReadable (Command w args ios)     = "StartCmd  " <> (getValue w) <> " " <> (T.intercalate " " $ getValue <$> args)
getReadable (PushHandle h)
  | h == stdin  = "PushHndl  stdin"
  | h == stdout = "PushHndl  stdout"
  | h == stderr = "PushHndl  stderr"
getReadable (Pipe tgt)           = "Pipe      " <> (T.pack $ show tgt)
getReadable RunPendingActions    = "RunCondt"
getReadable ClosedStdin          = "CloseStdin"

getListing :: [Instruction] -> T.Text
getListing instr = T.pack $ intercalate "\n" $ values where
    values = f 0 $ map getReadable instr
    f i (x:xs) = (printf "0x%04X  %s" (i :: Int) x : f (i+1) xs)
    f _ [] = []

printProgram :: [Instruction] -> IO ()
printProgram p = TIO.putStrLn $ getListing p

isBuiltin :: T.Text -> Bool
isBuiltin s =
  case s of
    "export" -> True
    _ -> False

executeBuiltin :: T.Text -> [T.Text] -> Shell -> IO Shell
executeBuiltin "export" (x:_) s = execute (Export (Name x)) s

executeAll' :: [Instruction] -> IO Shell
executeAll' i = executeAll i mkShell

executeWithStandardStreams :: [Instruction] -> IO Shell
executeWithStandardStreams i = let program = [ ClosedStdin
                                             , PushHandle stdout
                                             , PushHandle stderr ] ++ i
                               in executeAll' program

executeAll :: [Instruction] -> Shell -> IO Shell
executeAll [] s = return s
executeAll (x:xs) s = do
  s' <- execute x s
  executeAll xs s'

getHandle :: IOFile -> IO Handle
getHandle (IOFile _ mode f) = do
  let m = case mode of
            IOClobber -> WriteMode
            IOFrom -> ReadMode
            IOFromBoth -> ReadMode
            IOTo -> WriteMode
            IOToBoth -> WriteMode
            IOReadWrite -> ReadWriteMode
            IOAppend -> AppendMode
  h <- openFile (getValueS f) m
  return h

getOutputConduit :: [Handle] -> IO [Handle]
getOutputConduit [x] = return [x]
getOutputConduit hs = do
  isTty <- hIsTerminalDevice (last hs)
  let handles = case isTty of
                  True -> init hs
                  False -> hs

  return handles

-- | Variant of hClose that doesn't close standard streams.
hClose' :: Handle -> IO ()
hClose' h
  | h == stdin = return ()
  | h == stdout = return ()
  | h == stderr = return ()
  | otherwise = hClose h

execute :: Instruction -> Shell -> IO Shell

execute (Assign name@(Name var) w) shell = do
    w' <- substitute w
    return (setVar name w' shell)

execute (Export name) s = do
  let result = getVar name s
  case result of
    Just (Variable (Name n) value) -> setEnv (T.unpack n) (getValueS value) True
    Nothing -> return ()
  return s

execute RunPendingActions shell = do
  sequence (pendingActions shell)
  return shell { pendingActions = [] }

execute ClosedStdin shell = do
  (r, w) <- createPipe
  hClose w
  let cleanup = hClose r
  let shell1 = pushInputStream r shell
  let shell2 = pushAction cleanup shell1
  return shell2

execute (PushHandle h) shell
  | h == stdin  = return $ pushInputStream stdin shell
  | h == stdout = do
                  h <- openFile "/dev/tty" WriteMode
                  return $ pushOutputStream h shell
  | h == stderr = return $ pushErrorStream stderr shell

execute (Pipe t) shell = do
  (r, w) <- createPipe

  let tgt = case t of
              Output -> pushOutputStream
              Error -> pushErrorStream

  let shell1 = pushInputStream r $
               tgt w shell

  return shell1

execute (Command cmd args outIo) shell = do
  (cmdS:argsS) <- mapM substitute (cmd:args)

  let cmdName = getValue cmdS
  let cmdArgs = getValue <$> argsS

  bin <- findExecutable $ getValueS cmdS
  if isNothing bin
    then do
      TIO.hPutStrLn stderr ("lash: command not found: " <> (getValue cmdS))
      return shell
    else do
      let cp = (proc (getValueS cmdS) (getValueS <$> argsS))

      let (pIn, shell1) = popInputStream shell
      let (pOut, shell2) = popOutputStream shell1
      let (pErr, shell3) = popErrorStream shell2

      outHandles <- mapM getHandle outIo
      isTtyStdout <- hIsTerminalDevice pOut
      isTtyStderr <- hIsTerminalDevice pErr
      if (isTtyStdout && isTtyStderr && (null outHandles))
        then do
          (_, _, _, procHnd) <- createProcess cp
          exit <- waitForProcess procHnd
          return shell3 { lastExitCode = exit }
        else do
          os <- getOutputConduit (outHandles ++ [pOut])

          let output = case os of
                        [x] -> CB.sinkHandle x
                        (x:y:[]) -> CB.conduitHandle x .| CB.sinkHandle y
                        xs -> let (i,m,e) = (head xs, init $ tail xs, last xs) in
                               foldl fuse (CB.conduitHandle i) (map CB.conduitHandle m)
                               .| CB.sinkHandle e

          (exit, _, _) <- CP.sourceProcessWithStreams cp
                                                      (CB.sourceHandle pIn)
                                                      output
                                                      (CB.sinkHandle pErr)

          mapM_ hClose' [pIn, pOut, pErr]
          mapM_ hClose outHandles

          return shell3 { lastExitCode = exit }
