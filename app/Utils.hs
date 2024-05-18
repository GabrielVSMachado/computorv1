module Utils (trim, trimDSTuple) where

trimEnd :: String -> String
trimEnd = reverse . dropWhile (== ' ') . reverse

trimStart :: String -> String
trimStart "" = ""
trimStart (' ' : xs) = trimStart xs
trimStart xs = xs

trim :: String -> String
trim = trimEnd . trimStart

trimDSTuple :: (String, String) -> (String, String)
trimDSTuple (xs, ys) = (trim xs, trim ys)
