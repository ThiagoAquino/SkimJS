module Value (Value (..)) where

import Language.ECMAScript3.Syntax

data Value = Bool Bool
    | Int Int
    | String String
    | Var String
    | Nil
    | Break
    | Continue
    | Error String
    | Function Id [Id] [Statement]
    | Return Value
    | List [Value]

    deriving (Eq)
--
-- Pretty Printer
--

instance Show Value where
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Int int) = show int
  show (String str) = "\"" ++ str ++ "\""
  show (Var name) = name
  show Nil = "undefined"
  show (Break) = "Break"
  show (Continue) = ""
  show (Function (Id name) argumentos corpo) = show argumentos
  show (List l) = show l

-- This function could be replaced by (unwords.map show). The unwords
-- function takes a list of String values and uses them to build a
-- single String where the words are separated by spaces.
showListContents :: [Value] -> String
showListContents [] = ""
showListContents [a] = show a
showListContents (a:as) = show a ++ ", " ++ (showListContents as)
