module Potoki.Cereal.IO
where

import Potoki.Cereal.Prelude
import Potoki.Core.IO
import qualified Data.Serialize as Serialize
import qualified Potoki.Core.Produce as Produce
import qualified Potoki.Core.Consume as Consume
import qualified Potoki.Cereal.Consume as CerealConsume
import qualified Potoki.Cereal.Produce as CerealProduce


deserializeFromFile :: Serialize a => FilePath -> IO (Either IOException (Either Text a))
deserializeFromFile file = produceAndConsume (Produce.fileBytes file) (right' (CerealConsume.getImplicitly))

serializeToFile :: Serialize a => FilePath -> a -> IO (Either IOException ())
serializeToFile file a = produceAndConsume (CerealProduce.putImplicitly a) (Consume.writeBytesToFile file)
