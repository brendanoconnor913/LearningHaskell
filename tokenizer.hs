breakString :: [Char] -> [[Char]]

breakString line = go line "" [] 
    where go line buffer eachchar
            | (take 1 line) == "" = eachchar ++ buffer:[]
            | (take 1 line) == " " = go (drop 1 line) "" (if buffer /= "" then (eachchar ++ buffer:[]) else eachchar)
            | otherwise = go (drop 1 line) (buffer ++ take 1 line) eachchar 

myWords :: [Char] -> [[Char]] 

myWords line = go line []
   where go line output
            | (take 1 line) == ""   = output
            | (take 1 line) == " "  = go (dropWhile (== ' ') line) output
            | otherwise             = go (dropWhile (/= ' ') line) ((takeWhile (/= ' ') line):[] ++ output)
