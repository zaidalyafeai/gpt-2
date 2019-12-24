{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}

module GPT2 where


import Data.Time.Clock
import Data.Time.Format
import           Control.Exception
import           Control.Lens
import qualified Data.Aeson as A
import qualified Data.Aeson.Lens as A
import           Data.ByteString (ByteString)
import           Data.Char
import           Data.List.CommonSubstring
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Network.Wreq as Wreq
import           System.Timeout

formatTimestamp :: UTCTime -> Text
formatTimestamp time = T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time)

timeoutRetry :: IO a -> IO a
timeoutRetry m = do ma <- timeout (60 * 1000000) m
                    case ma of
                      Just a -> pure a
                      Nothing -> putStrLn "Timed out while sampling chunk, retrying" >> timeoutRetry m

data Timeout = Timeout
  deriving Show

instance Exception Timeout

data Err = Err String
  deriving Show
instance Exception Err

timeoutThrow :: IO a -> IO a
timeoutThrow m = do ma <- timeout (180 * 1000000) m
                    case ma of
                      Just a -> pure a
                      Nothing -> putStrLn "Timed out while sampling" >> throwIO Timeout

sampleChunk :: Text -> IO (Text, Text)
sampleChunk ctx = timeoutRetry $ do
  putStr "."
  -- print ("Sampling chunk,", ctx)
  let url = "http://localhost:8000/predict"
  req <- Wreq.post url (T.encodeUtf8 ctx)
  case A.eitherDecode (req ^. Wreq.responseBody) of
    Right (json :: A.Value) ->
      let text = json ^?! A.key "text" . A.values . A._String
          newctx = json ^?! A.key "context" . A._String
      in return (newctx, text)
    Left msg -> throwIO (Err msg)

-- Returns sampled text from ctx <> prompt, as well as new truncation of ctx.
sampleChunkWithPrompt :: Text -> Text -> IO (Text, Text)
sampleChunkWithPrompt ctx prompt = do
  let lctx = ctx <> prompt
  (newlctx, chunk) <- sampleChunk (ctx <> prompt)
  let newctx = T.dropEnd (T.length prompt) newlctx
  return (T.dropEnd (T.length prompt) newlctx, chunk)

sampleLine :: Text -> IO (Text, Text)
sampleLine ctx = sampleLineWithPrompt ctx ""

-- Returns sampled line starting with prompt, as well as new truncation of ctx.
sampleLineWithPrompt :: Text -> Text -> IO (Text, Text)
sampleLineWithPrompt ctx partline = putStr "@" >> go ctx partline
  where
    go newctx partline
      | T.isInfixOf "\n" partline = pure (newctx, T.takeWhile (/= '\n') partline)
      | otherwise = do (newctx, piece) <- sampleChunkWithPrompt newctx partline
                       go newctx (partline <> piece)

sampleLineWithPrompt' :: Text -> Text -> IO (Text, Text)
sampleLineWithPrompt' ctx prompt = putStr "#" >> go [] 32
  where
    go samples maxn = do
      (newctx, line) <- sampleLineWithPrompt ctx prompt
      let n = length $ longestSubstring (T.unpack ctx) (T.unpack line)
      if n > maxn then
        T.putStrLn ("Failing sample with similarity " <> T.pack (show n) <> ": " <> line)
        >> go ((n, line) : samples) (maxn+10)
        else
        do let (chosen_n, chosen_line) = head $ sort ((n, line) : samples)
           -- T.putStrLn $ "Choosing sample with lcs=" <> T.pack (show chosen_n) <> ", innovation=" <> T.pack (show (T.length chosen_line - chosen_n)) <> ": " <> chosen_line
           return (newctx, chosen_line)
