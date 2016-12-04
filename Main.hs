{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns, LambdaCase #-}
module Main where

import Protolude
import Network.IRC.Client hiding (reply)
import qualified Data.Text as T
import qualified Data.Time.Clock as TCl
import qualified Data.Time.Calendar as TCal
import qualified Control.Concurrent.STM as STM

main :: IO ()
main = ircbot

data Reminded = Never | Date TCl.UTCTime

data BotState
  = BotState { _lastReminded :: Reminded }

ircbot :: IO ()
ircbot = do
  conn <- connect "chat.freenode.net" 6667 3
  startStateful (conn { _logfunc = stdoutLogger })
                ((defaultIRCConf nick) { _eventHandlers = handlers })
                (BotState
                  -- we don’t want to spam the channel on join
                  { _lastReminded = Never })

nick :: Text
nick = "GNUpaganda"
channel :: Text
channel = "##augsburg"

meintenSieMsg, reminderMsg :: Msg
meintenSieMsg = "Meinten Sie: GNU/Linux"
reminderMsg   = "Your daily reminder to contribute to free software or GTFO"

handlers :: [EventHandler BotState]
handlers = defaultEventHandlers
  <> [ joinAugsburg
     , replyToWrongLinux
     , dailyReminder ]

joinAugsburg :: EventHandler s
joinAugsburg = EventHandler
  { _description = "join " <> channel
  , _matchType = ENumeric
  , _eventFunc = \ev -> case _message ev of
      Numeric 1 _ -> send $ Join channel
      _ -> return () }


replyToWrongLinux :: EventHandler s
replyToWrongLinux = replyIfNotReplyToMe channel rep
  where rep msg | " linux " `T.isInfixOf` (T.toLower msg)
                            = Just meintenSieMsg
                | otherwise = Nothing

dailyReminder :: EventHandler BotState
dailyReminder = EventHandler
  { _description = "remind about free software first thing in a day"
  , _matchType = EPrivmsg
  , _eventFunc = f channel }
  where
    f ch ev = case _source ev of
      Channel msgCh _ -> when (ch == msgCh) $ do
        usVar <- _userState <$> ask
        us <- liftIO $ STM.readTVarIO usVar
        now <- liftIO $ TCl.getCurrentTime
        let remindedNow = liftIO $ atomically . STM.writeTVar usVar
                            $ us { _lastReminded = Date now }
            remind = do send (Privmsg ch (Right reminderMsg))
                        remindedNow

        case _lastReminded us of
          Never  -> remindedNow
          Date d -> do
            let timeOfFirstRemind = 6 * 3600 -- 6am
                -- we subtract how many hours into the day
                -- the reminder can first trigger
                theDay t = TCl.utctDay (TCl.addUTCTime (-timeOfFirstRemind) t)
            when (theDay now > theDay d) remind
            remindedNow
      _ -> return ()


---- helpers

type Msg = Text
type Channel = Text

-- | reply to messages on a channel if need be
reply :: Channel -> (Msg -> Maybe Msg) -> EventHandler s
reply ch rep = EventHandler
  { _description = "reply to something"
  , _matchType = EPrivmsg
  , _eventFunc = \ev -> case _message ev of
      Privmsg ch' (Right msg)
        | ch' == ch -> maybe (pure ()) (send . Privmsg ch . Right) $ rep msg
        | otherwise -> pure ()
      _             -> pure () }

replyIfNotReplyToMe :: Channel -> (Msg -> Maybe Msg) -> EventHandler s
replyIfNotReplyToMe ch rep = reply ch $ (\msg -> do
                                            guard . not $ nick `T.isInfixOf` msg
                                            rep msg)
