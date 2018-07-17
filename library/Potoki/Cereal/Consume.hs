module Potoki.Cereal.Consume
where

import Potoki.Cereal.Prelude
import Potoki.Core.Consume
import Data.Serialize
import qualified Potoki.Core.Fetch as E
import qualified Potoki.Cereal.Transform as D


get :: Get a -> Consume ByteString (Either Text a)
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
        in do
          fetch <- fetchIO
          case fetch of
            Nothing -> none
            Just res -> some res
    in loop (runGetPartial get)
    
encodeToFile :: Serialize a => FilePath -> Consume a (Either IOException ())
encodeToFile = transform D.encode . writeBytesToFile
