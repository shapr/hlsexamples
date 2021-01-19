{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hedgehog
import Hedgehog.Main
import Examples

prop_test :: Property
prop_test = property $ do
  doExamples === "Examples"

main :: IO ()
main = defaultMain [checkParallel $$(discover)]
