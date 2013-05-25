
module CSV (parseCSV) where

import Data
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language
import Control.Applicative ((<*>), (<$>), (<*))
import Data.List.Split (splitOn)

lexer = makeTokenParser emptyDef
isint = integer lexer

test = do   
  file <- readFile "test.csv"
  let g = parseCSV "csvsomfosdfisdiofj" file
  putStrLn $ showDBs [g]

g = parseCSV "sample" "id,name\n2,dave\n3,stu"

parseCSV :: Name -> String -> Table
parseCSV name contents = 
  let header = splitOn "," . head . lines $ contents in
  case parse csvFile "" $ (unlines . tail . lines) contents of
       Left err -> error $ show err
       Right body -> Table name header body

intCell :: Parser AValue
intCell = do
  nums <- many isint <* isint
  (return . AInt . head) nums

{-- other --}
csvFile :: Parser [Body]
csvFile = endBy line eol       

line :: Parser [AValue]
line = sepBy (try intCell <|> try stringCell) (char ',')

stringCell :: Parser AValue
stringCell = do
  dat <- many (noneOf ",\n")
  return (AString dat)

eol :: Parser Char
eol = char '\n'


