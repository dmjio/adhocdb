module Main where

import System.Directory (doesDirectoryExist, createDirectory, getDirectoryContents)
import System.Environment
import System.IO
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import qualified Data.Map as M 
import System.FilePath (takeExtension)
import Control.Monad (forever, when, liftM)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.State
import Data.List.Split (splitOn)
import Data.List
import Data.IORef
import Welcome (welcomeMsg)
import Eval
import CSV
import Data
import SQL

-- | Welcome message
welcome :: IO ()
welcome = putStrLn welcomeMsg

-- | Network file retrieval
networkReq :: IO String
networkReq = do
  url <- getLine {- TODO: error handling here, transformer -}
  rsp <- simpleHTTP $ getRequest url
  body <- getResponseBody rsp
  return body

-- | Initial State
type Stater = State (M.Map Name Table) (M.Map Name Table)

-- | Function to add a table
addDB :: Name -> Table -> Stater
addDB name table = do
  map <- get
  put $ M.insert name table map 
  return map

-- | File importer
import' :: AdHocState 
import' = do 
  liftIO $ putStrLn "(c) - csv"
  char <- liftIO getLine
  case char of
    "c" -> do
      liftIO $ putStrLn "Is this from (w) - web or (f) - file?"
      result <- liftIO getLine
      case result of
        "f" -> do
          liftIO $ putStrLn "Please enter the file path on your desktop i.e. ~/Desktop/example.csv"
          path <- liftIO getLine
          file <- liftIO $ readFile path
          liftIO $ print file
          let csv = parseCSV path file
          liftIO $ print csv
          return ()
        otherwise -> liftIO $ putStrLn "unrecognized" >> return ()

-- |DB Display
show' :: AdHocState 
show' = do
  dbs <- get
  liftIO $ putStrLn $ show dbs
  
-- |Static File Location
dir :: String
dir = "AdHocDBs"

-- |Helper Function for checking custom file name
isADB :: String -> Bool
isADB = (==".adb") . takeExtension

initDB :: AdHocState
initDB = do 
  exists <- liftIO $ doesDirectoryExist dir
  if exists
     then do files <- liftIO $ getDirectoryContents dir
             let filtered = filter isADB files
             if null filtered {- If null then db's empty -}
               then do return ()
                  else do
                    liftIO $ putStrLn "\tTODO: Fill" {- otherwise files exist, suck them up, TODO : Implement -}
                    liftIO $ print filtered
                    map <- get
                    liftIO $ print map
     else do liftIO $ createDirectory dir

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

until_ pred prompt action = do 
  result <- prompt
  if pred result 
     then return ()
     else action result >> until_ pred prompt action

type Env = IORef [DB]
type AdHocState = StateT (M.Map String String) IO ()

evalAndPrint :: IORef [DB] -> String -> IO ()
evalAndPrint ref expr = do
  dbs <- readIORef ref
  let (result, modified) = eval dbs (parseSQL expr)
  liftIO $ putStrLn result
  writeIORef ref modified

runRepl :: IORef [DB] -> IO ()
runRepl ref = until_ (== "quit") (readPrompt "λ: ") (evalAndPrint ref)

main :: IO ()
main = do
  let map = M.empty :: M.Map String String
  runStateT initDB map >> welcome
  args <- getArgs
  ref <- newIORef []
  case length args of
    0 -> runRepl ref 
    1 -> evalAndPrint ref (args !! 0)
    otherwise -> putStrLn "Program takes only 0 or 1 argument"


