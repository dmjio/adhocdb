{-# LANGUAGE RecordWildCards #-}
module SQL where

import Data 
import Data.List
import Text.ParserCombinators.Parsec hiding (State, spaces)
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language
import Control.Monad (liftM)
import System.FilePath (takeExtension)
import Error

type Type = String
type Values = [String]

data Expr = GTE Column Int 
          | GTL Column Int
          | EqInt Column Int 
          | EqString Column String
          deriving Show  

data Command = SelectAll Name 
             | SelectFrom Header Name
             | SelectFromWhere Header Name Expr
             | InsertInto Name Header Values
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
             | ExportCommand Name Type
             | ImportTable Type Name
             deriving Show

showTables :: Parser Command
showTables = string "show" >> spaces >> string "tables" >> return ShowTables

importTable :: Parser Command
importTable = do
  string "import"
  spaces 
  string "table"
  spaces
  fileName <- many (noneOf ",\n")
  return $ ImportTable (takeExtension fileName) fileName

getHeader = do
  char '('   
  cols <- sepBy (skipMany space >> many (letter <|> digit)) (char ',')
  char ')' 
  return cols

queryShell x y = string x >> spaces >> string y >> many (letter <|> digit)
spaces = skipMany1 space
word = many (noneOf "\n,")

createTable :: Parser Command
createTable = do
  string "create"
  spaces
  string "table"
  spaces
  name <- many (letter <|> digit)
  spaces
  columns <- getHeader
  return $ CreateTable name columns
  
useDatabase :: Parser Command
useDatabase = liftM UseDatabase $ queryShell "use" "db" >> word

insertInto :: Parser Command
insertInto = do
  table <- queryShell "insert" "into"
  spaces
  cols <- getHeader 
  spaces
  string "values"
  spaces
  vals <- getHeader
  return $ InsertInto table cols vals

createDB :: Parser Command
createDB = liftM CreateDatabase $ queryShell "create" "db" >> word

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

selectFrom :: Parser Command
selectFrom = do
  string "select"
  spaces
  cols <- getHeader
  spaces
  string "from"
  spaces
  table <- many (noneOf ",\n")
  skipMany space
  return $ SelectFrom cols table

selectFromWhere :: Parser Command
selectFromWhere = do
  string "select"
  spaces
  words <- getHeader
  spaces
  string "from"
  spaces
  table <- many letter
  skipMany space
  string "where"
  return $ SelectFrom words table

sqlParser = try createDB    <|> 
            try selectAll   <|> 
            try displayDBs  <|> 
            try useDatabase <|>
            try createTable <|>
            try importTable <|>
            try showTables  <|>
            try insertInto 

parseSQL :: String -> Command
parseSQL sql = case parse sqlParser "" sql of
  Left err -> InvalidCommand
  Right val -> val

