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
  Transform $ return $ \ inputFetch -> A.Fetch $ do
    unconsumedRef <- newIORef mempty
    finishedRef <- newIORef False
    fetchParsed inputFetch finishedRef unconsumedRef
  where
    fetchParsed :: A.Fetch ByteString -> IORef Bool -> IORef ByteString -> IO (Maybe (Either Text decoded))
    fetchParsed (A.Fetch inputFetchIO) finishedRef unconsumedRef =
      do
        finished <- readIORef finishedRef
        if finished
          then return Nothing
          else do
            unconsumed <- readIORef unconsumedRef
            if unconsumed == mempty
              then do
                inputFetch <- inputFetchIO
                case inputFetch of
                  Nothing    -> return Nothing
                  Just input -> do
                    if input == mempty
                      then return Nothing
                      else matchResult $ inputToResult input
              else do
                writeIORef unconsumedRef mempty
                matchResult $ inputToResult unconsumed
      where
        matchResult =
          \ case
            C.Partial inputToResult ->
              consume inputToResult
            C.Done decoded unconsumed ->
              do
                writeIORef unconsumedRef unconsumed
                return $ Just (Right decoded)
            C.Fail message unconsumed ->
              do
                writeIORef unconsumedRef unconsumed
                writeIORef finishedRef True
                return $ Just (Left resultMessage)
              where
                resultMessage =
                  fromString message
        consume inputToResult = do
          inputFetch <- inputFetchIO
          case inputFetch of
            Nothing -> do
              writeIORef finishedRef True
              matchResult $ inputToResult mempty
            Just input -> do
              when (input == mempty) (writeIORef finishedRef True)
              matchResult $ inputToResult input

