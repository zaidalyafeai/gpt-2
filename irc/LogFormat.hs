{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LogFormat where

import           Control.Exception
import           Control.Lens
import           Data.ByteString (ByteString)
import           Data.Char
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Data.Time.Clock
import           Data.Time.Format
import           System.Timeout

import           GPT2

data Msg = Msg {
  mtime :: Text,
  muser :: Text,
  mtext :: Text
  }
  deriving (Show)

parseMsg :: Text -> Maybe Msg
parseMsg line =
  case T.splitOn "\t" line of
    [ts, nick, msg] -> Just (Msg ts nick msg)
    _ -> Nothing

formatMsg :: Msg -> Text
formatMsg Msg{..} = T.intercalate "\t" [mtime, muser, mtext]

formatPrompt :: Msg -> Text
formatPrompt Msg{..}
  | T.null mtime && T.null muser && T.null mtext = ""
  | T.null muser && T.null mtext = mtime <> "\t"
  | otherwise = T.intercalate "\t" [mtime, muser, mtext]

sampleMessage :: Text -> IO (Text, Msg)
sampleMessage ctx = sampleMessageWithPrompt ctx (Msg "" "" "")

-- Some basic censoring of possibly offensive content.
checkOk :: Text -> Bool
checkOk line
  | T.isInfixOf "c0rw1n" line = False
  | T.isInfixOf "trann" line = False
  | otherwise = True

sampleMessageWithPrompt :: Text -> Msg -> IO (Text, Msg)
sampleMessageWithPrompt ctx prompt = timeoutThrow $ putStr "/" >> go
  where
    go = do
      (newctx, line) <- sampleLineWithPrompt' ctx (formatPrompt prompt)
      case parseMsg line of
        Just msg
          | checkOk line -> return (newctx, msg)
        _ -> T.putStrLn ("Failing sample: " <> line) >> go

sampleGwernpaste :: Text -> Text -> IO [Msg]
sampleGwernpaste orig_ctx prompt = timeoutThrow $ do
  let
    go [] = do
      now <- getCurrentTime
      let lctx = Msg (formatTimestamp now) "@gwern" ("'" <> prompt)
      line1 <- snd <$> sampleMessageWithPrompt orig_ctx lctx
      if "..." `T.isSuffixOf` (mtext line1) then
        go [line1]
        else
        go []
    go ls = do
      now <- getCurrentTime
      let
        lctx = Msg (formatTimestamp now) "@gwern" "..."
        ctx = orig_ctx <> T.unlines (map formatMsg ls)
      line2 <- snd <$> sampleMessageWithPrompt ctx lctx
      if "..." `T.isSuffixOf` (mtext line2) then
        go (ls ++ [line2])
        else
        finish (ls ++ [line2])
    finish ls = return ls
  go []
