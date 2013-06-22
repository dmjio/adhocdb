{-# LANGUAGE RecordWildCards #-}
module SQL where

import Data
import Text.ParserCombinators.Parsec hiding (State, spaces)
import Control.Monad (liftM)
import Control.Applicative ((<*>), (<*), (*>), (<$>), pure)
import System.FilePath (takeExtension)

type Values = [String]
type CreateCols = [(AValue, Name)]

data Command = SelectAll Name 
             | SelectFrom Header Name
             | SelectFromWhere Header Name
             | InsertInto Name [AValue] 
             | CreateTable Name Header
             | CreateDatabase Name
             | DropTable Name
             | DropDatabase Name
             | UpdateTable Name
             | ShowDBs
             | ShowTables
             | UseDatabase Name
             | ShowDB Name
             | InvalidCommand
             | ExportCommand Name 
             | ImportTable String Name
             | UpdateSet Name Name String
             | UpdateSetWhere Name Name Name (Name, String)
             deriving Show

getAValue :: Parser AValue
getAValue =  try (AString <$> ((char '"') *> (many $ noneOf ['"']) <* (char '"'))) <|>
             (AInt . rd) <$> (many digit) where
               rd :: String -> Integer
               rd = read

dropDB :: Parser Command
dropDB = DropDatabase <$> (queryShell "drop" "db" >> skipMany space >> word)

exportTable :: Parser Command
exportTable = do
  string "export"
  spaces
  string "table"
  spaces
  name <- many letter
  return $ ExportCommand name

showTables :: Parser Command
showTables = string "show" >> spaces >>
             string "tables" >> return ShowTables

importTable :: Parser Command
importTable = do
  string "import"
  spaces 
  string "table"
  spaces
  fileName <- many (noneOf ",\n")
  return $ ImportTable (takeExtension fileName) fileName

getHeader :: Parser [AValue]
getHeader = do
  char '('   
  cols <- sepBy
          (skipMany space >> getAValue)
          (char ',')
  char ')' 
  return cols

queryShell x y = string x >> spaces >>
                 string y >> many (letter <|> digit)

spaces = skipMany1 space
word = many (noneOf "\n,")

useDatabase :: Parser Command
useDatabase = liftM UseDatabase $ queryShell "use" "db" >> word

insertInto :: Parser Command
insertInto = do
  table <- queryShell "insert" "into"
  spaces
  name <- many letter
  spaces
  vals <- getHeader
  return $ InsertInto name vals

createDB :: Parser Command
createDB = liftM CreateDatabase $
           queryShell "create" "db" >> word

displayDBs :: Parser Command
displayDBs = queryShell "show" "dbs" >> return ShowDBs

selectAll :: Parser Command
selectAll = do
  string "select"
  spaces
  char '*'
  spaces
  string "from"
  spaces
  table <- word
  return $ SelectAll table

valEqualsType :: Parser (Name, Type)
valEqualsType = do
  name <- many $ noneOf "\n,()="
  skipMany space
  char '='
  skipMany space
  typ <- choice $ map string ["int", "double", "string"]
  case typ of
    "int"    ->  skipMany space >> return (name, INT)
    "string" ->  skipMany space >> return (name, STRING)
    "double" ->  skipMany space >> return (name, DOUBLE)

createCols :: Parser [(Name,Type)]
createCols = sepBy valEqualsType (char ',')

createTable :: Parser Command
createTable = do
  string "create"
  spaces
  string "table"
  spaces
  table <- many letter
  skipMany space
  char '(' 
  skipMany space
  colTypes <- createCols
  skipMany space
  char ')' 
  skipMany space
  return $ CreateTable table colTypes

updateSet :: Parser Command
updateSet = do
  string "update" 
  spaces
  table <- word 
  spaces
  string "set" 
  spaces
  col <- word
  skipMany space
  char '=' 
  skipMany space
  val <- word
  skipMany space
  return $ UpdateSet table col val

valEqualsVal :: Parser (Name, String)
valEqualsVal = do
  name <- many $ noneOf "\n,="
  skipMany space
  char '='
  skipMany space
  val <- many $ noneOf "\n,="
  return (name,val)

updateSetWhere = do
  string "update"
  spaces
  table <- many $ noneOf " "
  spaces
  string "set" 
  spaces
  col <- many $ noneOf " "
  skipMany space
  char '=' 
  skipMany space
  val <- many $ noneOf "\n, " 
  skipMany space
  string "where" 
  skipMany space
  conditions <- valEqualsVal
  skipMany space
  return $ UpdateSetWhere table col val conditions

sqlParser = try createDB    <|> 
            try selectAll   <|> 
            try displayDBs  <|> 
            try useDatabase <|>
            try createTable <|>
            try importTable <|>
            try showTables  <|>
            try insertInto  <|>
            try dropDB      <|>
            try updateSet  


parseSQL :: String -> Command
parseSQL sql = case parse sqlParser "" sql of
  Left err -> InvalidCommand
  Right val -> val

