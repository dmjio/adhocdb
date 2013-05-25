module Data where

import Data.List

-- sampleDB = DB "Cities" 
--            ["Rank", "City", "State", "Estimate","Change"]
--            [["1", "New York",    "New York",   "8,244,910", "0.85%"],
--             ["2", "Los Angeles", "California", "3,819,702", "0.71%"],
--             ["3", "Chicago",     "Illinois",   "2,707,120", "0.43%"]]


newtype DB = DB [Table]
data Table = Table { name   :: Name, 
                     header :: [String], 
                     body   :: [Body] }
           deriving Show

type Name   = String
type Body   = [AValue]
data AValue = AString String 
            | AInt Integer deriving Show


showDBs :: [Table] -> String
showDBs all@(x:xs) = let len     = maximum $ map length $ map name all 
                         lenAdh  = length nameADB
                         nameADB = " AdHocDB's "
                         total   = max len lenAdh
                         padding = 2
                         dashes  = replicate (total + padding) '-'
                         line    = "+" ++ dashes ++ "+\n"
                         header  = "|" ++ nameADB ++ 
                                   replicate (total - lenAdh + padding) ' ' ++ "|\n"
                         dbs     = concatMap (\x -> "| " ++ (name x) ++ 
                                   replicate (total - length (name x) + 1) ' ' 
                                   ++ "|\n") all
                     in line ++ header ++ line ++ dbs ++ line

-- showDB :: Table -> String
-- showDB (Table name headers body) = 
--   let wordLen = length name + 2
--       headerLen = maximum $ map length header
--       line = replicate (length name + 2) ' '
--       dashes = "+" ++ replicate 
--   in line 

-- instance Show DB where
--   show (DB name headers body) = 
--     "Data source: " ++ name ++ "\n" ++ 
--     (intercalate " - " headers) ++ "\n" ++
--     printBody body where
--       printBody []  = ""
--       printBody (x:xs) = (intercalate " - " (x)) ++ "\n" ++ printBody xs


-- instance Show DB where 
--   show (DB name headers body) = 
--     "Data source: " ++ name ++ "\n" ++ 
--     (intercalate " - " headers) ++ "\n" ++
--     printBody body where
--       printBody [] = ""
--       printBody (x:xs) = 
--         (intercalate " - " (lines x)) ++ "\n" ++ printBody xs
    
    

