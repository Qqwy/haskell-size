{-# OPTIONS_GHC -ddump-rule-rewrites #-}
module Main (main) where

import Size

main :: IO ()
main = do
    let x = 10 :: Size
    let y = 20 :: Size
    print (y - x)
    print (x - y)
