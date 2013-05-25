module Main where

import System.Directory (doesDirectoryExist, createDirectory, getDirectoryContents)
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import qualified Data.Map as M 
import System.FilePath (takeExtension)
import Control.Monad (forever, when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.State
import Data.List.Split (splitOn)
import Data.List
import Welcome (welcome)
import CSV
import Data

type AdHocState = StateT (M.Map String String) IO ()

-- | Welcome message
welcome' :: IO ()
welcome' = putStrLn welcome

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
--          liftIO $ print csv
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
     then do liftIO $ putStrLn "\tFolder exists"
             files <- liftIO $ getDirectoryContents dir
             let filtered = filter isADB files
             if null filtered {- If null then db's empty -}
               then do liftIO $ putStrLn "\tDB's Empty"
                  else do
                    liftIO $ putStrLn "\tTODO: Fill" {- otherwise files exist, suck them up, TODO : Implement -}
                    liftIO $ print filtered
                    map <- get
                    liftIO $ print map
     else do liftIO $ putStrLn "\tFolder doesn't exist"
             liftIO $ createDirectory dir

main :: IO ()
main = do
  let map =  M.empty :: M.Map String String
  runStateT initDB map >> welcome'
  forever $ do
  char <- getLine
  if char == "q" 
     then return ()
     else when (not . null $ char) $ do
            case char of
--              "i" -> runStateT import' map
              _ -> error "unrecognized sequence"
                




