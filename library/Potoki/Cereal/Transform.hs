module Potoki.Cereal.Transform
(
  encode,
  decode,
)
where

import Potoki.Cereal.Prelude
import Potoki.Core.Transform
import qualified Potoki.Core.Fetch as A
import qualified Potoki.Core.Transform as D
import qualified Data.Attoparsec.Types as B
import qualified Data.Serialize as C


encode :: C.Serialize element => Transform element ByteString
encode =
  arr C.encode

decode :: C.Serialize element => Transform ByteString (Either Text element)
decode =
  runPartialDecoder (C.runGetPartial C.get)

result2IResult :: C.Result decoded -> B.IResult ByteString decoded
result2IResult =
  \case
    C.Fail err input -> B.Fail input [] err
    C.Partial func   -> B.Partial $ result2IResult . func
    C.Done val input -> B.Done input val

{-# INLINE runPartialDecoder #-}
runPartialDecoder :: forall decoded. (ByteString -> C.Result decoded) -> Transform ByteString (Either Text decoded)
runPartialDecoder input2Result =
  D.mapWithParseResult $ result2IResult . input2Result
