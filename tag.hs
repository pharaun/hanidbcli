import Data.List

getTag :: String -> String
getTag a
    | validTag a = (reverse . nextChar . reverse) a
    | otherwise  = take (length a) ['a','a'..]
    where
        nextChar :: [Char] -> [Char]
        nextChar (x:xs) = (getNextChar x) : (if (isRollover x) then (nextChar xs) else xs)
        nextChar [] = []

        validTag xs = all (\a -> a `elem` charOrder) xs

isRollover :: Char -> Bool
isRollover x = rollover $ idx x
    where
        len = length charOrder
        idx a = a `elemIndex` charOrder

        rollover Nothing = True
        rollover (Just b)
            | b+1 < len  = False
            | otherwise  = True


getNextChar :: Char -> Char
getNextChar x = charOrder !! (next $ idx x)
    where
        len = length charOrder
        idx a = a `elemIndex` charOrder

        next Nothing = 0
        next (Just b)
            | b+1 < len = b+1
            | otherwise = 0


charOrder :: [Char]
charOrder = ['a'..'c']
