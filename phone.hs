import Data.List
import Data.Char

data Button =
  Options [Char] 
  deriving (Eq, Show)

data DaPhone =
  Pad [Button] 
  deriving (Eq, Show)

getId :: Button -> Char
getId (Options k) = k !! ((length k) - 1)

eliminate :: Maybe Int -> Int 
eliminate Nothing = 0
eliminate (Just a) = a

getPresses :: Char -> Button -> Int
getPresses c (Options k) = (eliminate (elemIndex c k)) + 1 

getCharButton :: DaPhone -> Char -> Button
getCharButton (Pad btns) c = foldr (\(Options nms) b -> if (elem c nms) then (Options nms) else b) (Options ['0']) btns 

reverseTaps :: DaPhone -> Char -> [(Char, Int)]
reverseTaps phone c = go phone c 
    where btn = (getCharButton phone c)
          sbtn = (getCharButton phone (toLower c))
          go phone c 
                | (isUpper c) = [('*',1),((getId sbtn), (getPresses (toLower c) sbtn))]
                | otherwise   = [((getId btn), (getPresses c btn))]

cellPhonesDead :: DaPhone -> String -> [(Char, Int)]
cellPhonesDead _ [] = []
cellPhonesDead phone (x:xs) = (reverseTaps phone x) ++ (cellPhonesDead phone xs)          

fingerTaps :: [(Char, Int)] -> Int 
fingerTaps = foldr (\a b -> (snd a) + b) 0 

b1 = Options ['a','b','c','2']
b2 = Options ['d','e','f','3']
b3 = Options ['g','h','i','4']
b4 = Options ['j','k','l','5']
b5 = Options ['m','n','o', '6']
b6 = Options ['p','q','r','s','7']
b7 = Options ['t','u','v','8']
b8 = Options ['w','x','y','z','9']
b9 = Options ['*','^']
b10 = Options [' ','0']
b11 = Options ['.',',','#']
phone = Pad [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11]
