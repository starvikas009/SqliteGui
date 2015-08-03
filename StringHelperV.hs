module StringHelperV where

-------- String trimming -----

quoteChar :: Char
quoteChar = '"'

displaySimple :: Show a => a -> String
-- point free function. first show string then trim quotes
displaySimple = show -- trimQuotes . show

trimQuotes :: String -> String
trimQuotes xs = let reversedXS1 = trimAndReverse1 xs
                in trimAndReverse1 reversedXS1

trimStart1 :: String -> String
trimStart1 (quoteChar:xs) = xs
trimStart1 xs = xs

trimAndReverse1 :: String -> String
trimAndReverse1 = (reverse . trimStart1)
