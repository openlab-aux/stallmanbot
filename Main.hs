{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns, LambdaCase #-}
module Main where

import Protolude
import Network.IRC.Client hiding (reply)
import qualified Data.Text as T
import qualified Data.Time.Clock as TCl
import qualified Data.Time.Calendar as TCal
import qualified Control.Concurrent.STM as STM
import qualified Text.Regex.TDFA as R
import Text.Regex.TDFA.Text ()

main :: IO ()
main = do
  headMay <$> getArgs >>= \case
    Just channel -> ircbot $ toS channel
    Nothing -> panic "no channel given"

data Reminded = Never | Date TCl.UTCTime

data BotState
  = BotState { _lastReminded :: Reminded }

type Msg = Text
-- TODO: refactor out the explicit Channels given to every function
type Channel = Text

ircbot :: Channel -> IO ()
ircbot ch = do
  conn <- connect "chat.freenode.net" 6667 3
  startStateful (conn { _logfunc = stdoutLogger })
                ((defaultIRCConf nick) { _eventHandlers = handlers ch })
                (BotState
                  -- we don’t want to spam the channel on join
                  { _lastReminded = Never })

nick :: Text
nick = "GNUpaganda"

meintenSieMsg, reminderMsg :: Msg
meintenSieMsg = "Meinten Sie: GNU/Linux"
reminderMsg   = "Your daily reminder to contribute to free software or GTFO"


handlers :: Channel -> [EventHandler BotState]
handlers ch = defaultEventHandlers
  <> [ joinChannel ch
     , replyToWrongLinux ch
     , dailyReminder ch ]

joinChannel :: Channel -> EventHandler s
joinChannel ch = EventHandler
  { _description = "join " <> ch
  , _matchType = ENumeric
  , _eventFunc = \ev -> case _message ev of
      Numeric 1 _ -> send $ Join ch
      _ -> return () }


replyToWrongLinux :: Channel -> EventHandler s
replyToWrongLinux ch = replyIfNotReplyToMe ch rep
  where rep msg | matches msg = Just meintenSieMsg
                | otherwise   = Nothing
        matches :: Msg -> Bool
        matches msg = all ($ (T.toLower msg)) $
            [ matchReg "linux"
            , not . matchReg "gnu[^a-z]+linux" ]
        matchReg :: Text -> Msg -> Bool
        matchReg r = R.matchTest (R.makeRegex r :: R.Regex)

dailyReminder :: Channel -> EventHandler BotState
dailyReminder ch = EventHandler
  { _description = "remind about free software first thing in a day"
  , _matchType = EPrivmsg
  , _eventFunc = f ch }
  where
    f ch' ev = case _source ev of
      Channel msgCh _ -> when (ch' == msgCh) $ do
        usVar <- _userState <$> ask
        us <- liftIO $ STM.readTVarIO usVar
        now <- liftIO $ TCl.getCurrentTime
        let remindedNow = liftIO $ atomically . STM.writeTVar usVar
                            $ us { _lastReminded = Date now }
            remind = do send (Privmsg ch' (Right reminderMsg))
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
