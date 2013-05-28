module CSV (parseCSV) where

import Data
import Text.ParserCombinators.Parsec hiding (State, spaces)
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language
import Control.Applicative ((<*>), (<$>))
import Data.List.Split (splitOn)
import Control.Monad

parseCSV :: Name -> String -> Table
parseCSV name contents = 
  let header = splitOn "," . head . lines $ contents in     -- This should have its own parser
  case parse csvFile "" $ (unlines . tail . lines) contents of
       Left err -> error $ show err
       Right body -> Table name header body

parseHeader :: Parser Header
parseHeader = sepBy (spaces >> many (noneOf ",\n")) (char ',')

spaces :: Parser ()
spaces = skipMany space

intCell :: Parser AValue
intCell = liftM (AInt . read) $ many1 digit 

csvFile :: Parser Body
csvFile = endBy line eol       

line :: Parser [AValue]
line = sepBy (try (spaces >> intCell) <|> try (spaces >> stringCell)) (char ',')

stringCell :: Parser AValue
stringCell = liftM AString $ many (noneOf ",\n")

eol :: Parser Char
eol = char '\n'

g = parseCSV "sample" "id,name\n2,dave\n3,stu"
