{-# OPTIONS_GHC -ddump-asm #-}
module Main (main) where

import Size

main :: IO ()
main = do
    x <- readLn :: IO Size
    y <- readLn :: IO Size
    -- let x = 10 :: Size
    -- let y = 20 :: Size
    print (y - x)
    print (x - y)


