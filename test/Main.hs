module Main where

import Prelude hiding (choose)
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Property as QuickCheck
import qualified Potoki.Cereal.Consume as Consume
import qualified Potoki.Cereal.Produce as Produce
import qualified Potoki.Cereal.Transform as Transform
import qualified Potoki.Consume as Consume
import qualified Potoki.Produce as Produce
import qualified Potoki.Transform as Transform
import qualified Potoki.IO as IO
import qualified System.Directory as IO


main =
  defaultMain $
  testGroup "All" $
  [
    testProperty "" $ let
      workDir = "dist/test-temp"
      file = workDir <> "/list"
      gen :: Gen [Int]
      gen = do
        length <- choose (0, 100)
        replicateM length (choose (-100, 100))
      in forAll gen $ \ list -> unsafePerformIO $ do
        IO.createDirectoryIfMissing True workDir
        raiseIOException =<< IO.produceAndConsume (Produce.list list) (Consume.encodeToFile file)
        restoredList <- raiseText =<< raiseIOException =<< IO.produceAndConsume (Produce.fileDecoded file) (right' (right' Consume.list))
        return (list === restoredList)
  ]

raiseIOException = either throwIO return
raiseText = either (fail . show) return
