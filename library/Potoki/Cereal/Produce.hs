module Potoki.Cereal.Produce
where

import Potoki.Cereal.Prelude
import Potoki.Core.Produce
import Data.Serialize
import qualified Data.ByteString.Lazy as A
import qualified Data.ByteString.Lazy.Internal as A


put :: Put -> Produce ByteString
put =
  lazyByteString . runPutLazy
