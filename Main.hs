{-# LANGUAGE OverloadedStrings, TypeFamilies, DeriveDataTypeable, TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module Main where

import Data.Aeson
import Data.Acid
import Data.SafeCopy
import Control.Concurrent (forkIO)
import Control.Monad.Trans (liftIO)
import qualified Web.Scotty as W
import Network.Wai.Middleware.Static
import Control.Monad.State (put)
import Control.Monad.Reader (ask)
import System.IO
import Welcome (welcomeMsg)
import Eval
import Data
import SQL
import Data.Data

newtype Database = Database [DB] deriving (Show, Eq, Ord, Typeable)

-- | Acid state transaction methods
updateDB :: [DB] -> Update Database ()
updateDB dbs = put $ Database dbs

getDB :: Query Database [DB]
getDB = do Database dbs <- ask
           return dbs

-- | Safe Copy instances
$(deriveSafeCopy 0 'base ''Database)
$(deriveSafeCopy 0 'base ''DB)
$(deriveSafeCopy 0 'base ''Table)
$(deriveSafeCopy 0 'base ''AValue)
$(deriveSafeCopy 0 'base ''Type)

-- | Make data types serializeable
$(makeAcidic ''Database ['updateDB, 'getDB])

-- | JSON Instances
instance ToJSON DB
instance ToJSON Table
instance ToJSON AValue
instance ToJSON Type

-- | Welcome message
welcome :: IO ()
welcome = putStrLn welcomeMsg

-- | Flush buffer output
flushStr :: String -> IO ()
flushStr str = hSetBuffering stdout LineBuffering >>
               putStr str >> hFlush stdout

-- | Read from stdin
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- | Custom control.monad helper
until_ pred prompt action = do 
  result <- prompt
  if pred result 
     then return ()
     else action result >> until_ pred prompt action

-- | Eval loop
evalAndPrint db expr = do
  dbs <- query db GetDB
  let (result, dbs') = eval dbs (parseSQL expr)
  update db (UpdateDB dbs')
  putStrLn result
  
-- | REPL
runRepl ref = until_ (== "quit") (readPrompt "λ: ") (evalAndPrint ref)

-- | Main method
main :: IO ()
main = do
  welcome 
  db <- openLocalStateFrom "dbs/" (Database [])
  dbs <- query db GetDB
  forkIO $ runRepl db      
  W.scotty 3000 $ do
    W.middleware $ staticPolicy (noDots >-> addBase "static")
    W.get "/" $
      W.file "index.html"
    W.get "/query" $ do
      dbs <- liftIO $ query db GetDB
      W.json $ dbs




    

