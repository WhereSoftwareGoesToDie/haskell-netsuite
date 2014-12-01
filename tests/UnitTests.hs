{-# LANGUAGE OverloadedStrings #-}

module Main where

import Netsuite.Connect
import Test.Hspec

suite :: Spec
suite = do
    describe "Foo" $ do
        it "Can do something" $ do
            pass

main :: IO ()
main = hspec suite

pass :: Expectation
pass = return ()
