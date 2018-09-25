module Potoki.Cereal.Consume
where

import Potoki.Cereal.Prelude
import Potoki.Core.Consume
import qualified Data.Serialize as Serialize
import qualified Potoki.Core.Fetch as E
import qualified Potoki.Cereal.Transform as D


get :: Serialize.Get a -> Consume ByteString (Either Text a)
get get =
  Consume $ \ (E.Fetch fetchIO) ->
  let
    loop decodeChunk =
      let
        none = case decodeChunk "" of
          Serialize.Done result _ -> return (Right result)
          Serialize.Fail error _ -> return (Left (fromString error))
          _ -> return (Left "Not enough data")
        some chunk = case decodeChunk chunk of
          Serialize.Partial newDecodeChunk -> loop newDecodeChunk
          Serialize.Done result _ -> return (Right result)
          Serialize.Fail error _ -> return (Left (fromString error))
        in do
          fetch <- fetchIO
          case fetch of
            Nothing -> none
            Just res -> some res
    in loop (Serialize.runGetPartial get)

getImplicitly :: Serialize a => Consume ByteString (Either Text a)
getImplicitly = get Serialize.get

encodeToFile :: Serialize a => FilePath -> Consume a (Either IOException ())
encodeToFile = transform D.encode . writeBytesToFile

encodeToStdout :: Serialize a => Consume a ()
encodeToStdout = transform D.encode writeBytesToStdout
