module Util where

import Data.List

{--------
--Helpers
---------}

getUsedInput :: IO String
getUsedInput = do 
    xs <- getLine
    return xs


removeLivingCells :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
removeLivingCells from rem = from \\ rem