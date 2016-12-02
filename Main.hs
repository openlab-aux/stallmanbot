{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns, LambdaCase #-}
module Main where

import Protolude
import Network.IRC.Client hiding (reply)
import qualified Data.Text as T

main :: IO ()
main = ircbot

ircbot :: IO ()
ircbot = do
  conn <- connect "chat.freenode.net" 6667 3
  start (conn { _logfunc = stdoutLogger })
        $ (defaultIRCConf nick)
            { _eventHandlers = defaultEventHandlers <> [ joinAugsburg, replyToWrongLinux ] }

nick :: Text
nick = "GNUpaganda"
channel :: Text
channel = "##augsburg"

joinAugsburg :: EventHandler ()
joinAugsburg = EventHandler
  { _description = "join " <> channel
  , _matchType = ENumeric
  , _eventFunc = \ev -> case _message ev of
      Numeric 1 _ -> send $ Join channel
      _ -> return () }


replyToWrongLinux :: EventHandler ()
replyToWrongLinux = replyIfNotReplyToMe channel rep
  where rep msg | " linux " `T.isInfixOf` (T.toLower msg)
                            = Just "Meinten Sie: GNU/Linux"
                | otherwise = Nothing


---- helpers

type Msg = Text
type Channel = Text

-- | reply to messages on a channel if need be
reply :: Channel -> (Msg -> Maybe Msg) -> EventHandler ()
reply ch rep = EventHandler
  { _description = "reply to something"
  , _matchType = EPrivmsg
  , _eventFunc = \ev -> case _message ev of
      Privmsg ch' (Right msg)
        | ch' == ch -> maybe (pure ()) (send . Privmsg ch . Right) $ rep msg
        | otherwise -> pure ()
      _             -> pure () }

replyIfNotReplyToMe :: Channel -> (Msg -> Maybe Msg) -> EventHandler ()
replyIfNotReplyToMe ch rep = reply ch $ (\msg -> do
                                            guard . not $ nick `T.isInfixOf` msg
                                            rep msg)
