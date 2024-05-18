module Utils (trim) where

trimEnd :: String -> String
trimEnd = reverse . dropWhile (== ' ') . reverse

trimStart :: String -> String
trimStart "" = ""
trimStart (' ' : xs) = trimStart xs
trimStart xs = xs

trim :: String -> String
trim = trimEnd . trimStart
