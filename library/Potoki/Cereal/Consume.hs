module Potoki.Cereal.Consume
where

import Potoki.Cereal.Prelude
import Potoki.Core.Consume
import Data.Serialize
import qualified Potoki.Core.Fetch as E


get :: Get a -> Consume ByteString (Either String a)
get get =
  Consume $ \ (E.Fetch fetchIO) ->
  let
    loop decodeChunk =
      let
        none = case decodeChunk "" of
          Done result _ -> return (Right result)
          Fail error _ -> return (Left (fromString error))
          _ -> return (Left "Not enough data")
        some chunk = case decodeChunk chunk of
          Partial newDecodeChunk -> loop newDecodeChunk
          Done result _ -> return (Right result)
          Fail error _ -> return (Left (fromString error))
        in join (fetchIO none some)
    in loop (runGetPartial get)
