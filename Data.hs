module Data where

import Data.List
import Data.Maybe (fromJust)
import qualified Data.Map as M

data DB     = DB {  dbName :: Name, tables :: [Table], isActive :: Bool } deriving (Eq)
data Table  = Table { name :: Name, header :: Header, body :: Body } deriving (Eq)
data AValue = AString String 
            | AInt    Integer deriving (Eq)

type Name   = String
type Body   = [[AValue]]
type Header = [String]

instance Show AValue where 
  show (AString string) = string
  show (AInt num) = show num

instance Show DB where show = showDB
instance Show Table where show = showTable

showDBs :: [DB] -> String
showDBs [DB [] [] _] = "empty"
showDBs [] = "empty"
showDBs all = let len = maximum $ map length $ map dbName all
                  lenAdh  = length nameADB
                  nameADB = " AdHocDB's "
                  total   = max len lenAdh
                  padding = 2
                  dashes  = replicate (total + padding) '-'
                  line    = "+" ++ dashes ++ "+\n"
                  header  = "|" ++ nameADB ++ replicate (total - lenAdh + padding) ' ' ++ "|\n"
                  dbs     = concatMap (\x -> "| " ++ (dbName x) ++ replicate (total - length (dbName x) + 1) ' ' ++ "|\n") all
              in line ++ header ++ line ++ dbs ++ line

showDB :: DB -> String
showDB (DB dbname tables _) = line ++ header ++ line ++ printTables ++ line
    where nameLength  = length dbname
          tableLength = maximum $ map (length . name) tables
          largest     = max nameLength tableLength
          padding     = 2
          dashes      = replicate (largest + padding) '-'
          line        = "+"  ++ dashes ++ "+\n"
          header      = "| " ++ dbname ++ replicate (largest - nameLength) ' ' ++ " |\n"
          printTables = concatMap (\(Table tname _ _) -> "| " ++ tname ++ replicate (largest - (length tname)) ' ' ++ " |\n") tables

showTable :: Table -> String
showTable (Table tableName header body)
    | header == [] = error "Tables need headers"
    | body   == [] = "Empty Table"
    | otherwise = line ++ cols ++ line ++ bod ++ line
    where headerLength = (sum $ map length header) + padding + (padding * barCount)
          bodyLength   = padding + (padding * barCount) + (sum $ getColWidthArray body)
          largest      = maximum [headerLength, bodyLength]
          barCount     = length header - 1
          padding      = 2
          line         = let fliprep = flip replicate in concatMap (("+"++) . (fliprep '-')) (map (+padding) $ getColWidthArray body) ++ "+\n"
          col x        = "| " ++ x ++ replicate ((fromJust $ lookup x (maxColMap header body)) - length x + 1) ' ' 
          cols         = concatMap col header ++ "|\n"
          bod          = concatMap (("| "++) . (++"\n") . printBody) body 
          printBody row = concatMap (\x -> show x ++ replicate ((fromJust $ lookup x (maxColMap row body)) - (length . show) x + 1) ' ' ++"| ") row
          maxColMap head body = zipWith (,) head (getColWidthArray body)
          getColWidthArray body = map maximum $ (map . map) (length . show) $ transpose  $ [header] ++ (map . map) show body

testDBs = [DB "DBTest" [] True, DB "DBTest2" [] True]
tDBs = putStrLn $ showDBs testDBs

testDB = DB "TestDB" [ Table "A Table" [] [], Table "Another Table" [] [], Table "Another Another Table" [] [] ] True
tDB = putStrLn $ showDB testDB

testTable = Table "A Table" ["Name------reallylongname","ID"] [[AString "String", AInt 09428209385901], [AString "Barb", AInt 2]]
tTable = print testTable



