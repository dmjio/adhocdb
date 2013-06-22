module Eval where

import Data
import SQL
import Data.List
import Control.Monad.Reader

tableDoesNotExist = "Table does not exist"

getActiveDB :: Reader [DB] (Either String DB)
getActiveDB = do 
  dbs <- ask
  case filter isActive dbs of
    [db]      -> return $ Right db
    otherwise -> return $ Left "You must select a database to activate"

findDBByName :: Name -> Reader [DB] (Either String DB)
findDBByName name = do
  dbs <- ask
  case (filter ((==name) . dbName) dbs) of
    [db]       -> return $ Right db
    otherwise  -> return $ Left "Couldn't find db!"

findTableByName :: String -> Reader [DB] (Either (String, DB) (Table, DB))
findTableByName tableName = do
  dbs <- ask
  let activeDB  = runReader getActiveDB dbs
  case activeDB of
    Left failMsg -> return $ Left (failMsg, head dbs)
    Right db -> do
      case (filter ((==tableName) . name) (tables db)) of
        [table]   -> return $ Right (table, db)
        otherwise -> return $ Left (tableDoesNotExist, db)

createDatabase :: String -> [DB] -> (String, [DB])
createDatabase name dbs = let newDBs = (DB name [] False):dbs
                          in if (null $ filter ((==name) . dbName) dbs) 
                             then (showDBs newDBs, newDBs)
                             else ("Already exists", dbs)

eval :: [DB] -> Command -> (String, [DB])
eval dbs (SelectAll tableName)  = 
  let result = runReader (findTableByName tableName) dbs
  in case result of
    Left (fail, db)   -> (fail, dbs)
    Right (table, db) -> (show table, dbs)

eval dbs (DropDatabase name)     =
  case runReader (findDBByName name)  dbs of
    Left fail -> (fail, dbs)
    Right db -> ("dropped " ++ dbName db, delete db dbs)
  
eval dbs (CreateDatabase name)     = createDatabase name dbs
eval dbs ShowDBs                   = (showDBs dbs, dbs)
eval dbs ShowTables                = case runReader getActiveDB dbs of
                                        Left fail -> (fail, dbs)
                                        Right db -> (show db, dbs)

eval dbs (CreateTable tname headers) =
  let result = runReader (findTableByName tname) dbs
  in case result of
    Right table -> ("table already exists", dbs)
    Left (exists, db) ->
      if exists == tableDoesNotExist
      then (show updatedDB, updatedDBs) 
      else (exists, dbs) where
        newTable   = Table { name = tname, header = headers, body = [] }
        updatedDB  = db { tables = newTable:(tables db) }
        updatedDBs = updatedDB:(delete db dbs)

eval dbs (UseDatabase name)    = let db = filter (\x -> dbName x == name) dbs
                                 in if not $ null db
                                    then ("using " ++ (dbName . head) db,
                                          ((head db) { isActive = True } : (delete (head db) dbs)))
                                    else ("Could not find db", dbs)


eval dbs InvalidCommand = ("Unrecognized Command", dbs)
eval dbs (InsertInto name vals) =
  let result = runReader (findTableByName name) dbs
  in case result of
    Right (table, db) ->  
      case (checkMatch (header table) vals) of
        True -> let (newTable, newDbs) =
                      insertIntoTable dbs db (tables db) table vals
                in (show newTable, newDbs)
        False -> ("Incorrect number of arguments, or type error", dbs)
    Left (fail, db) -> (fail, dbs)



exportCSV :: [Table] -> String -> IO String
exportCSV tables name = undefined
  
insertIntoTable :: [DB] -> DB -> [Table] -> Table -> [AValue] -> (Table, [DB])
insertIntoTable dbs db tables tabl vals =
  let newTable  = tabl { body = (body tabl) ++ [vals] }
      newTables = [newTable] ++ (delete tabl tables) 
      newDB     = db { tables = newTables }
      newDbs    = [newDB] ++ (delete db dbs) 
  in (newTable, newDbs)
      
trans :: AValue -> Type
trans (AString _) = STRING
trans (AInt _)    = INT
trans (ADouble _) = DOUBLE

checkMatch :: Header -> [AValue] -> Bool
checkMatch head vals
  | length vals /= length head = False
  | (map snd head) == (map trans vals) = True
  | otherwise = False

com      = parseSQL "insert into Cd (3,3)"
testCom  = UseDatabase "P" 
testData = [ AInt 3 ]
testDBs  =
  [
    DB { dbName = "P",
         tables = [ Table { name = "C",
                            header = [("m", INT)],
                            body = [] } ],
         isActive = True }
  ]





