{-# LANGUAGE OverloadedStrings #-}
module Lash.Instructions where

import           Lash
import           Lash.AST
import           Lash.Expansion
import           Lash.Builtin

import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad                (when)
import           Data.List                    (intercalate)
import           Data.Maybe                   (isNothing, fromJust, isJust)
import           Data.Monoid                  ((<>))
import           Data.Conduit
import           Data.Vector                  ((!), Vector, fromList)
import           Data.ByteString              (ByteString)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Process as CP
import           System.Directory             (findExecutable)
import           System.Exit
import           System.IO
import           System.Process
import           System.Posix.Env             (setEnv)
import           Text.Printf                  (printf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Prelude hiding (Word, putStrLn)

type Program = Vector Instruction

cond = [ Assign (Name "0") (Unquoted "0")
       , Assign (Name "3") (Unquoted "3")
       , If (Condition (\s -> ((length . variables) s) == 1)) 0x0001 0x0002
       , Assign (Name "1") (Unquoted "1")
       , Assign (Name "2") (Unquoted "2")
       ]

pg0 = [ ClosedStdin
      , PushHandle stdout
      , PushHandle stderr
      , cmd' "ls" ["-l", "--color=tty"]
      , RunPendingActions
      ]

pg1 = [ Assign (Name "x") (Unquoted "valueX")
      , ClosedStdin
      , Pipe Output
      , PushHandle stderr
      , cmd' "cat" ["lash.cabal"]
      , Pipe Output
      , PushHandle stderr
      , cmd' "grep" ["a"]
      , PushHandle stdout
      , PushHandle stderr
      , Command (Just (Unquoted "wc")) [Unquoted "-l"] [ IOFile Nothing IOTo (Unquoted "wc-write.txt")
                                                       , IOFile Nothing IOAppend (Unquoted "wc-append.txt")
                                                       ]
      ]

class Compilable a where
  compile :: a -> [Instruction]

instance Compilable Assignment where
  compile (Assignment n w) = [Assign n w]

instance Compilable Command where
  compile (SimpleCommand assgn name args ios) =
    (concatMap compile assgn) ++ [ Command name args ios ]

instance Compilable Pipeline where
  compile (Pipeline _ []) = error "empty command list"
  compile (Pipeline reverseExit [cmd]) = [ ClosedStdin
                                         , PushHandle stdout
                                         , PushHandle stderr
                                         , head $ compile cmd
                                         , RunPendingActions
                                         ]
  compile (Pipeline reverseExit cmds) =
    let ccmds = map ( compile) cmds in
    let prologue = [ ClosedStdin, Pipe Output, PushHandle stderr ] in
    let pipe = intercalate [ Pipe Output
                           , PushHandle stderr
                           ] (init ccmds) in
    let epilogue = [ PushHandle stdout
                   , PushHandle stderr
                   , (head $ last ccmds)
                   , RunPendingActions
                   ] in
    let exit = case reverseExit of
                 True -> [ReverseLastExitCode]
                 False -> [] in
    prologue ++ pipe ++ epilogue ++ exit

instance Compilable Sequence where
  compile (CondEnd p) = compile p
  compile (CondSeq p op t) = compile p ++ compile t

data Instruction = Assign Name Word
                 | ClosedStdin
                 | Command (Maybe Word) [Word] [IOFile]
                 | PushHandle Handle
                 | Pipe ConduitTarget
                 | RunPendingActions
                 | ReverseLastExitCode
                 | If Condition Int Int
                 deriving (Show)

newtype Condition = Condition (Shell -> Bool)

instance Show Condition where
  show cond = "{condition}"

data ConduitTarget = Input | Output | Error deriving (Show, Eq, Ord)

cmd :: T.Text -> Instruction
cmd c = Command (Just (Unquoted c)) [] []

cmd' :: T.Text -> [T.Text] -> Instruction
cmd' c a = Command (Just (Unquoted c)) (Unquoted <$> a) []

getReadable :: Instruction -> T.Text
getReadable (Assign (Name n) w) = "Assign    " <> n <> " := " <> (getValue w)
getReadable (Command w args ios) =
  "StartCmd  " <> (maybe "{nothing}" getValue w) <> " " <> (T.intercalate " " $ getValue <$> args)
getReadable (PushHandle h)
  | h == stdin = "PushHndl  stdin"
  | h == stdout = "PushHndl  stdout"
  | h == stderr = "PushHndl  stderr"
getReadable (Pipe tgt) = "Pipe      " <> (T.pack $ show tgt)
getReadable RunPendingActions = "RunCondt"
getReadable ClosedStdin = "CloseStdin"
getReadable ReverseLastExitCode = "ReverseLastExitCode"
getReadable (If cond then_ else_) = "If"

getListing :: [Instruction] -> T.Text
getListing instr = T.pack $ intercalate "\n" $ values where
    values = f 0 $ map getReadable instr
    f i (x:xs) = (printf "0x%04X  %s" (i :: Int) x : f (i+1) xs)
    f _ [] = []

printProgram :: [Instruction] -> IO ()
printProgram p = TIO.putStrLn $ getListing p

executeAll' :: [Instruction] -> IO Shell
executeAll' i = executeAll i mkShell

run :: Vector Instruction -> Shell -> IO Shell
run program shell = do
  let ip = (currentInstruction shell)
  -- putStrLn (printf "0x%04X" ip)
  if ip >= (length program)
    then return shell
    else do
      shell2 <- execute (program ! ip) shell
      let shell3 = case (incrementPointer shell2) of
                     True -> shell2 { currentInstruction = ip + 1}
                     False -> shell2
      run program (shell3 { incrementPointer = True })

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
getOutputConduit [] = error "empty list"
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
    (w', shell') <- expand w shell
    return (setVar name w' shell')

-- execute (Export name) s = do
--   let result = getVar name s
--   case result of
--     Just (Variable (Name n) value) -> setEnv (T.unpack n) (getValueS value) True
--     Nothing -> return ()
--   return s

execute (If (Condition cond) ipThen ipElse) shell = do
  let ip = currentInstruction shell
  let shell2 = if cond shell
                then shell { currentInstruction = ip + ipThen }
                else shell { currentInstruction = ip + ipElse }

  return shell2 { incrementPointer = False }


execute ReverseLastExitCode shell = do
  let x = lastExitCode shell
  case x of
    ExitSuccess -> return shell { lastExitCode = (ExitFailure 1) }
    _ -> return shell { lastExitCode = ExitSuccess }

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

execute (Command Nothing _ _) shell0 = return shell0

execute (Command (Just cmd) args outIo) shell0 = do
  ((cmdS:argsS), shell1) <- expand' (cmd:args) shell0

  let cmdName = getValueS cmdS
  let cmdArgs = getValueS <$> argsS

  let (pIn, shell2) = popInputStream shell1
  let (pOut, shell3) = popOutputStream shell2
  let (pErr, shell4) = popErrorStream shell3

  outHandles <- mapM getHandle outIo
  isTtyStdout <- hIsTerminalDevice pOut
  isTtyStderr <- hIsTerminalDevice pErr

  let cleanup = mapM_ hClose' (pIn:pOut:pErr:outHandles)

  let useStream = not $ isTtyStdout && isTtyStderr && null outHandles

  let cp = (proc cmdName cmdArgs)

  let builtin = getBuiltin cmdName
  executable <- findExecutable cmdName

  finalShell <- if useStream
    then do
      os <- getOutputConduit (outHandles ++ [pOut])

      let output = case os of
                    [] -> error "empty output handles"
                    [x] -> CB.sinkHandle x
                    (x:y:[]) -> CB.conduitHandle x .| CB.sinkHandle y
                    xs -> let (i,m,e) = (head xs, init $ tail xs, last xs) in
                           foldl fuse (CB.conduitHandle i) (map CB.conduitHandle m)
                           .| CB.sinkHandle e

      case builtin of
        Nothing -> do
          (exit, _, _) <- CP.sourceProcessWithStreams cp
                                                      (CB.sourceHandle pIn)
                                                      output
                                                      (CB.sinkHandle pErr)
          return shell4 { lastExitCode = exit }
        Just b -> do
          (r, w) <- createPipe
          s <- b shell4 cmdArgs (pIn, w, pErr)
          hClose w
          runConduit $ CB.sourceHandle r .| output
          return s
    else
      case builtin of
        Nothing -> do
          bin <- findExecutable cmdName
          if isNothing bin
            then hPutStrLn stderr ("lash: command not found: " ++ cmdName) >> return shell4
            else do
              (_, _, _, procHnd) <- createProcess (cp { std_in = UseHandle pIn
                                                      , std_out = UseHandle pOut
                                                      , std_err = UseHandle pErr })
              exit <- waitForProcess procHnd
              return shell4 { lastExitCode = exit }
        Just b -> b shell4 cmdArgs (pIn, pOut, pErr)

  cleanup
  return finalShell
