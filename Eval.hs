module Eval where

import Data
import CSV
import SQL
import Error
import Data.List
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Error

findTableByName :: String -> [DB] -> Table
findTableByName tableName dbs@(x:xs) = let ts = tables x
                                           table = filter ((==tableName) . name) ts
                                       in if not $ null table 
                                          then head table
                                          else findTableByName tableName xs

createDatabase :: String -> [DB] -> (String, [DB])
createDatabase name dbs = let newDBs = (DB name [] False):dbs
                          in if (null $ filter ((==name) . dbName) dbs) 
                             then (showDBs newDBs, newDBs)
                             else ("Already exists", dbs)

eval :: [DB] -> Command -> (String, [DB])
eval dbs (SelectAll tableName)     = let table = findTableByName tableName dbs in (show table, dbs)
eval dbs (CreateDatabase name)     = createDatabase name dbs
eval dbs ShowDBs                   = (showDBs dbs, dbs)
eval dbs ShowTables                = let db = filter ((==True) . isActive) dbs
                                     in if not $ null db
                                        then (show db, dbs)
                                        else ("Empty", dbs)
eval dbs (ImportTable typ name)    = 
  case typ of
    ".csv" -> let table = unsafePerformIO $ readFile name >>= return . parseCSV name 
                  db = filter ((==True) . isActive) dbs
                  ts = tables (head db)
                  showResults :: [DB] -> [Table] -> (String, [DB])
                  showResults [] _  = ("You must first select a database", dbs)
                  showResults [d] tables = 
                      let updatedDb = d { tables = table : tables }
                      in (show table, updatedDb : (delete d dbs))
              in showResults db ts
              
eval dbs (CreateTable tname headers) = let db = filter ((==True) . isActive) dbs
                                           ts = tables (head db)
                                           table = filter ((==tname) . name) ts
                                           showResults :: [DB] -> [Table] -> (String, [DB])
                                           showResults [] _  = ("You must first select a database", dbs)
                                           showResults _ [t] = ("Table already exists", dbs)
                                           showResults [d] [] = 
                                             let newTable = Table { name   = tname,
                                                                    header = headers, 
                                                                    body   = [] }
                                                 updatedDB  = d { tables = newTable:ts }
                                                 updatedDBs = updatedDB:(delete d dbs)
                                             in (show updatedDB, updatedDBs)
                                       in showResults db table
                                        
eval dbs (UseDatabase name)    = let db = filter (\x -> dbName x == name) dbs 
                                 in if not $ null db 
                                    then ("using " ++ (dbName . head) db, ((head db) { isActive = True } : (delete (head db) dbs)))
                                    else ("Could not find db", dbs)
eval dbs InsertInto name header values = 
eval dbs InvalidCommand = ("Unrecognized Command", dbs)