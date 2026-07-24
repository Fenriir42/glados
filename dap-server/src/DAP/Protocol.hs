module DAP.Protocol
  ( readDAPMessage,
    writeDAPMessage,
    makeResponse,
    makeErrorResponse,
    makeEvent,
  )
where

import Control.Exception (IOException, try)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isDigit, toLower)
import Data.IORef (IORef, atomicModifyIORef')
import Data.Text (Text)
import System.IO (Handle, hFlush, hGetLine, hPutStr)

-- | Read one DAP message from a handle.
-- Returns Nothing on EOF or parse failure.
readDAPMessage :: Handle -> IO (Maybe Value)
readDAPMessage h = do
  r <- try (readFrame h) :: IO (Either IOException (Maybe BSL.ByteString))
  case r of
    Left _ -> return Nothing
    Right Nothing -> return Nothing
    Right (Just body) -> return (Aeson.decode body)

readFrame :: Handle -> IO (Maybe BSL.ByteString)
readFrame h = do
  lenM <- readContentLength h
  case lenM of
    Nothing -> return Nothing
    Just len -> Just <$> BSL.hGet h len

readContentLength :: Handle -> IO (Maybe Int)
readContentLength h = go Nothing
  where
    go acc = do
      line <- hGetLine h
      let stripped = reverse (dropWhile (== '\r') (reverse line))
      if null stripped
        then return acc
        else do
          let (key, rest) = break (== ':') stripped
              lkey = map toLower key
          if lkey == "content-length" && not (null rest)
            then
              let val = dropWhile (== ' ') (drop 1 rest)
               in if all isDigit val && not (null val)
                    then go (Just (read val))
                    else go acc
            else go acc

-- | Write one DAP message to a handle (thread-safe via caller's lock).
writeDAPMessage :: Handle -> IORef Int -> Value -> IO ()
writeDAPMessage h _seqRef val = do
  let body = Aeson.encode val
      len = BSL.length body
  hPutStr h ("Content-Length: " ++ show len ++ "\r\n\r\n")
  BSL.hPut h body
  hFlush h

nextSeq :: IORef Int -> IO Int
nextSeq ref = atomicModifyIORef' ref (\n -> (n + 1, n))

makeResponse :: IORef Int -> Int -> Text -> Bool -> Maybe Value -> IO Value
makeResponse seqRef reqSeq cmd success body = do
  n <- nextSeq seqRef
  return $
    object $
      [ "seq" .= n,
        "type" .= ("response" :: Text),
        "request_seq" .= reqSeq,
        "command" .= cmd,
        "success" .= success
      ]
        ++ maybe [] (\b -> ["body" .= b]) body

makeErrorResponse :: IORef Int -> Int -> Text -> Text -> IO Value
makeErrorResponse seqRef reqSeq cmd msg = do
  n <- nextSeq seqRef
  return $
    object
      [ "seq" .= n,
        "type" .= ("response" :: Text),
        "request_seq" .= reqSeq,
        "command" .= cmd,
        "success" .= False,
        "message" .= msg
      ]

makeEvent :: IORef Int -> Text -> Maybe Value -> IO Value
makeEvent seqRef ev body = do
  n <- nextSeq seqRef
  return $
    object $
      [ "seq" .= n,
        "type" .= ("event" :: Text),
        "event" .= ev
      ]
        ++ maybe [] (\b -> ["body" .= b]) body
