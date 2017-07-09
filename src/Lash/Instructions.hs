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
import qualified Data.Streaming.Process as SP
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
cat f = Command (Unquoted "cat") [Unquoted f] [] []

class Compilable a where
  compile :: a -> [Instruction]

instance Compilable Assignment where
    compile (Assignment n w) = [Assign n w]

data Instruction = Assign Name Word
                 | WaitRunningActions
                 | Export Name
                 | Command Word [Word] [IOFile] [IOFile]
                 deriving (Show)

getReadable :: Instruction -> T.Text
getReadable (Assign (Name n) w)  = "ASSIGN      " <> n <> " := " <> (getValue w)
getReadable (Export (Name n))    = "EXPORT      " <> n
getReadable WaitRunningActions   = "WAIT        "
getReadable (Command w args _ _) = "COMMAND     " <> (getValue w)

getListing :: [Instruction] -> T.Text
getListing instr = T.pack $ intercalate "\n" $ values where
    values = f 0 $ map getReadable instr
    f i (x:xs) = (printf "%04d  %s" (i :: Int) x : f (i+1) xs)
    f _ [] = []

isBuiltin :: T.Text -> Bool
isBuiltin s =
  case s of
    "export" -> True
    _ -> False

executeBuiltin :: T.Text -> [T.Text] -> Shell -> IO Shell
executeBuiltin "export" (x:_) s = execute (Export (Name x)) s

executeAll' :: [Instruction] -> IO Shell
executeAll' i = mkShell >>= \s -> executeAll i s

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

execute :: Instruction -> Shell -> IO Shell

execute (Assign name@(Name var) w) shell = do
    w' <- substitute w
    return (setVar name w' shell)

execute (Export name) s = do
    let result = getVar name s
    case result of
      Just (Variable (Name n) value) -> setEnv (T.unpack n) (getValueS value) True
      Nothing -> putStrLn "not variable"
    return s

execute WaitRunningActions shell = do
    sequence (shConduits shell)
    (exit:_) <- sequence (shWaiters shell)
    let shell' = (clearConduits $ clearWaiters shell)
    return shell' { shLastExitCode = exit }

execute (Command cmd args ins outs) shell = do
  (cmdS:argsS) <- mapM substitute (cmd:args)

  let cmdName = getValue cmdS
  let cmdArgs = map getValue argsS

  if isBuiltin cmdName
    then executeBuiltin cmdName cmdArgs shell
    else do
      bin <- findExecutable (T.unpack cmdName)
      if isNothing bin
        then do
          TIO.hPutStrLn stderr ("xsh: command not found: " <> cmdName)
          return shell
        else do
          let cp = createProc (T.unpack cmdName) (map T.unpack cmdArgs) shell
          (pIn, pOut, SP.Inherited, procHandle) <-
            SP.streamingProcess cp

          hOuts <- mapM getHandle outs
          hIns <- mapM getHandle ins

          let inputs = case hIns of
                         [] -> Nothing
                         (i:is) -> Just $ foldl fuse (CB.sourceHandle i) xs
                                   where xs = map CB.conduitHandle is :: [Conduit ByteString IO ByteString]

          let outputs = case hOuts of
                          [] -> Nothing
                          _ -> Just $ foldl fuse x xs
                               where (x:xs) = map CB.conduitHandle hOuts :: [Conduit ByteString IO ByteString]

          let conduit = do
                        when (isJust inputs) (runConduit $ (fromJust inputs) .| CB.sinkHandle pIn)
                        mapM_ hClose (pIn:hIns)

                        let conduitOuts = if isNothing outputs
                                            then o1 .| o2
                                            else o1 .| (fromJust outputs) .| o2
                                            where o1 = CB.sourceHandle pOut
                                                  o2 = CB.sinkHandle (shStdout shell)

                        runConduit conduitOuts
                        mapM_ hClose (pOut:hOuts)
                        return ()

          let shell' = addConduit conduit shell
          let wait = SP.waitForStreamingProcess procHandle
          let shell'' = addWaiter wait shell'

          return shell''

createProc :: FilePath -> [String] -> Shell -> CreateProcess
createProc cmd arg shell = CreateProcess { cmdspec = RawCommand cmd arg
                                         , cwd = Nothing
                                         , env = Nothing
                                         , std_in = UseHandle (shStdin shell)
                                         , std_out = UseHandle (shStdout shell)
                                         , std_err = Inherit
                                         , close_fds = True
                                         , create_group = False
                                         , delegate_ctlc = False
                                         , detach_console = False
                                         , create_new_console = False
                                         , new_session = False
                                         , child_group = Nothing
                                         , child_user = Nothing }
