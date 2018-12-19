{-# LANGUAGE OverloadedStrings #-}
module Main where

import Julia

main :: IO ()
main = do
  juliaInit
  evalJulia "print(sqrt(2.0))"
  exitJulia