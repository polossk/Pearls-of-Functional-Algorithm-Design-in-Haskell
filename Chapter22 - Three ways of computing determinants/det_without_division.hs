{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}

import Data.List
import Text.Printf

det :: [[Integer]] -> Integer
det ass = head $ head bss
    where
    bss   = foldl (trimult . mut) ass' (replicate (n - 1) ass)
    ass'  = if odd n then upper ass
            else map (map negate) (upper ass)
    n     = length ass

upper = zipWith drop [0..]

dotproduct xs ys = sum $ zipWith (*) xs ys

trimult xss yss = zipWith (map . dotproduct) xss $ submats $ transpose yss

submats :: [[a]] -> [[[a]]]
submats [[x]] = [[[x]]]
submats xss   = xss : submats (map tail (tail xss))

mut xss = zipWith (:) ys $ map tail xss
  where ys = map negate $ tail $ scanr (+) 0 (map head xss)

abc = [[5, 6, 7], [0, 2, 1], [0, 0, 1]]

main = printf "%d" $ det abc
