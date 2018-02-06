module Potoki.Cereal.Consume
(
  encodeToFile,
)
where

import Potoki.Cereal.Prelude
import Potoki.Consume
import Potoki.Core.Transform
import qualified Potoki.Cereal.Transform as D
import qualified Data.Serialize as C


encodeToFile :: C.Serialize a => FilePath -> Consume a (Either IOException ())
encodeToFile = transform D.encode . writeBytesToFile
