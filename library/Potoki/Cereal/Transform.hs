module Potoki.Cereal.Transform
(
  encode,
  decode,
)
where

import Potoki.Cereal.Prelude
import Potoki.Core.Transform
import qualified Potoki.Core.Fetch as A
import qualified Data.Serialize as C


encode :: C.Serialize element => Transform element ByteString
encode =
  arr C.encode

decode :: C.Serialize element => Transform ByteString (Either Text element)
decode =
  runPartialDecoder (C.runGetPartial C.get)

{-# INLINE runPartialDecoder #-}
runPartialDecoder :: forall decoded. (ByteString -> C.Result decoded) -> Transform ByteString (Either Text decoded)
runPartialDecoder inputToResult =
  Transform $ \ inputFetch ->
  do
    unconsumedRef <- newIORef mempty
    finishedRef <- newIORef False
    return (A.Fetch (fetchParsed inputFetch finishedRef unconsumedRef))
  where
    fetchParsed :: A.Fetch ByteString -> IORef Bool -> IORef ByteString -> forall x. x -> (Either Text decoded -> x) -> IO x
    fetchParsed (A.Fetch inputFetchIO) finishedRef unconsumedRef nil just =
      do
        finished <- readIORef finishedRef
        if finished
          then return nil
          else do
            unconsumed <- readIORef unconsumedRef
            if unconsumed == mempty
              then
                join $ inputFetchIO
                  (return nil)
                  (\ input -> do
                    if input == mempty
                      then return nil
                      else matchResult (inputToResult input))
              else do
                writeIORef unconsumedRef mempty
                matchResult (inputToResult unconsumed)
      where
        matchResult =
          \ case
            C.Partial inputToResult ->
              consume inputToResult
            C.Done decoded unconsumed ->
              do
                writeIORef unconsumedRef unconsumed
                return (just (Right decoded))
            C.Fail message unconsumed ->
              do
                writeIORef unconsumedRef unconsumed
                writeIORef finishedRef True
                return (just (Left resultMessage))
              where
                resultMessage =
                  fromString message
        consume inputToResult =
          join $ inputFetchIO
            (do
              writeIORef finishedRef True
              matchResult (inputToResult mempty))
            (\ input -> do
              when (input == mempty) (writeIORef finishedRef True)
              matchResult (inputToResult input))
