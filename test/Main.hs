module Main where

import qualified Spec
import qualified System.IO                     as IO
import           Test.Hspec.Runner

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.NoBuffering
  IO.hSetBuffering IO.stderr IO.NoBuffering
  hspecWith defaultConfig Spec.spec
