module Error where

import Control.Monad.Error
import Text.ParserCombinators.Parsec.Error
import Data
import Data.List

-- | Todo: Implement

data SQLError = NumArgs Integer Header
              | TypeError String AValue  
              | Parser ParseError 
              | Default String
              | MalformedExpression String

showError :: SQLError -> String
showError (NumArgs num header) = "Expected: " ++ show num ++ " args"
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (TypeError expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (MalformedExpression message) = message

instance Show SQLError where show = showError

instance Error SQLError where  
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either SQLError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val




