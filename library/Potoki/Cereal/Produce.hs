module Potoki.Cereal.Produce
where

import Potoki.Cereal.Prelude
import Potoki.Core.Produce
import Data.Serialize
import qualified Data.ByteString.Lazy as A
import qualified Data.ByteString.Lazy.Internal as A
import qualified Potoki.Cereal.Transform as D
import qualified Data.Serialize as C
import Potoki.Core.Transform


put :: Put -> Produce ByteString
put =
  lazyByteString . runPutLazy

fileDecoded :: C.Serialize a => FilePath -> Produce (Either IOException (Either Text a))
fileDecoded = transform (right D.decode) . fileBytes

directoryDecoded :: C.Serialize a => FilePath -> Produce (Either IOException (Either Text a))
directoryDecoded dirPath =
  transform
    (right (produce fileDecoded) >>^ join)
    (directoryContents dirPath)