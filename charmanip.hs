import Data.Char

sentcap (l:ls) = (toUpper l):ls
sentcap "" = "" 
