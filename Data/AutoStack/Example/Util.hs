module Data.AutoStack.Example.Util where

-- | In separate module due to GHC stage restrictions
-- [a,b,c,d] -> [a,b,c]
dropLast :: [a] -> [a]
dropLast s = take (length s - 1) s
