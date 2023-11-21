{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Rollbar.Golden
import qualified Rollbar.Item.Data.Test

main :: IO ()
main = do
    Rollbar.Golden.main
    Rollbar.Item.Data.Test.props
