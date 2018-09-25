module Potoki.Cereal.Produce
where

import Potoki.Cereal.Prelude
import Potoki.Core.Produce
import qualified Data.ByteString.Lazy as A
import qualified Data.ByteString.Lazy.Internal as A
import qualified Potoki.Cereal.Transform as D
import qualified Data.Serialize as C
import Potoki.Core.Transform


put :: C.Put -> Produce ByteString
put = lazyByteString . C.runPutLazy

putImplicitly :: Serialize a => a -> Produce ByteString
putImplicitly = put . C.put

fileDecoded :: Serialize a => FilePath -> Produce (Either IOException (Either Text a))
fileDecoded = transform (right D.decode) . fileBytes

stdinDecoded :: (Serialize a) => Produce (Either IOException (Either Text a))
stdinDecoded = transform (right' D.decode) stdinBytes

directoryDecoded :: Serialize a => FilePath -> Produce (Either IOException (Either Text a))
directoryDecoded dirPath =
  transform
    (right (produce fileDecoded) >>^ join)
    (directoryContents dirPath)
