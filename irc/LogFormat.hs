{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LogFormat where

import           Control.Applicative
import           Control.Exception hiding (try)
import           Control.Monad
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
import           Text.Parser.Char
import           Text.Parser.Combinators


import           GPT2
import           Parse

data Msg = Msg {
  mtime :: Text,
  muser :: Text,
  mtext :: Text
  }
  deriving (Show)

-- Old Format
-- ==========

-- parseMsg :: Text -> Maybe Msg
-- parseMsg line =
--   case T.splitOn "\t" line of
--     [ts, nick, msg] -> Just (Msg ts nick msg)
--     _ -> Nothing

-- formatMsg :: Msg -> Text
-- formatMsg Msg{..} = T.intercalate "\t" [mtime, muser, mtext]

-- formatPrompt :: Msg -> Text
-- formatPrompt Msg{..}
--   | T.null mtime && T.null muser && T.null mtext = ""
--   | T.null muser && T.null mtext = mtime <> "\t"
--   | otherwise = T.intercalate "\t" [mtime, muser, mtext]


-- New (feep) format
-- =================

-- parseNick :: Text -> Maybe Text
-- parseNick "*" = Just "*"
-- parseNick nick | T.isPrefixOf "<" nick && T.isSuffixOf ">" nick
--   = Just (T.drop 1 $ T.dropEnd 1 nick)
-- parseNick _ = Nothing

-- printNick :: Text -> Text
-- printNick "*" = "*"
-- printNick nick = "<" <> nick <> ">"

-- parseMsg :: Text -> Maybe Msg
-- parseMsg line =
--   case T.breakOn "\t" line of
--     (n, msg) -> case parseNick n of
--       Just nick -> Just (Msg "" nick (T.drop 1 msg))
--       Nothing | T.null line -> Just (Msg "" "" "")
--       Nothing -> Nothing

-- formatMsg :: Msg -> Text
-- formatMsg Msg{..} = printNick muser <> "\t" <> mtext

-- formatPrompt :: Msg -> Text
-- formatPrompt Msg{..}
--   | T.null muser && T.null mtext = ""
--   |                 T.null mtext = printNick muser
--   | otherwise                    = formatMsg Msg{..}


-- Shawwn format
-- =============

parseMsg :: Text -> Maybe Msg
parseMsg line = parse p line
  where
    p = do
      ts <- T.pack <$> replicateM 2 digit ## string ":" ## replicateM 2 digit
      char ' '
      muser <- T.pack <$> (between (char '<') (char '>') (some (noneOf ['>']))
                           <|> string "*"
                           <|> string ">>")
      char ' '
      mtext <- T.pack <$> some (noneOf ['\n'])
      return (Just (Msg ts muser mtext))

squashTime :: Text -> Text
squashTime txt = parse p txt
  where
    p = long <|> short
    long = do
      try (replicateM 4 digit ## string "-" ## replicateM 2 digit ## string "-" ## replicateM 2 digit)
      char ' '
      hhmm <- T.pack <$> replicateM 2 digit ## string ":" ## replicateM 2 digit
      return hhmm
    short = T.pack <$> replicateM 2 digit ## string ":" ## replicateM 2 digit

formatMsg :: Msg -> Text
formatMsg Msg{..} = T.intercalate " " [ts, nick, mtext]
  where
    ts = squashTime mtime
    nick | muser `elem` ["*", ">>"] = muser
         | otherwise = "<" <> muser <> ">"

formatPrompt :: Msg -> Text
formatPrompt m@Msg{..}
  | T.null mtime && T.null muser && T.null mtext = ""
  |                 T.null muser && T.null mtext = ts
  |                                 T.null mtext = T.intercalate " " [ts, nick]
  | otherwise = formatMsg m
  where
    ts = squashTime mtime
    nick | muser `elem` ["*", ">>"] = muser
         | otherwise = "<" <> muser <> ">"

-----

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
          | not (checkOk line) ->
              T.putStrLn ("Failing check: " <> line) >> go
          | muser msg == ">>" && muser prompt /= ">>" ->
              T.putStrLn ("Discarding >>: " <> line) >> go
          | otherwise -> pure (newctx, msg)

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

formatTimestamp :: UTCTime -> Text
formatTimestamp time = T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time)
