module Potoki.Cereal.Produce
(
  fileDecoded,
  directoryDecoded,
)
where

import Potoki.Cereal.Prelude
import Potoki.Produce
import Potoki.Core.Transform
import qualified Potoki.Cereal.Transform as D
import qualified Data.Serialize as C


fileDecoded :: C.Serialize a => FilePath -> Produce (Either IOException (Either Text a))
fileDecoded = transform (right D.decode) . fileBytes

directoryDecoded :: C.Serialize a => FilePath -> Produce (Either IOException (Either Text a))
directoryDecoded dirPath =
  transform
    (right (produce fileDecoded) >>^ join)
    (directoryContents dirPath)
