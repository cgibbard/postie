
module Web.Postie.Protocol(
    TlsStatus(..)
  , Mailbox
  , Event(..)
  , Command(..)
  , SmtpFSM
  , Reply
  , initSmtpFSM
  , step
  , reply
  , reply'
  , renderReply

  , parseCommand
  , parseHelo
  , parseMailFrom
  ) where

import Prelude hiding (takeWhile)

import Web.Postie.Address

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

import Control.Monad (void)

data TlsStatus = Active | Forbidden | Permitted | Required deriving (Eq)

data SessionState = Unknown
                  | HaveHelo
                  | HaveEhlo
                  | HaveMailFrom
                  | HaveRcptTo
                  | HaveData
                  | HaveQuit

type Mailbox = Address

data Event =  SayHelo BS.ByteString
           | SayHeloAgain BS.ByteString
           | SayEhlo BS.ByteString
           | SayEhloAgain BS.ByteString
           | SayOK
           | SetMailFrom Mailbox
           | AddRcptTo Mailbox
           | StartData
           | WantTls
           | WantReset
           | WantQuit
           | TlsAlreadyActive
           | TlsNotSupported
           | NeedStartTlsFirst
           | NeedHeloFirst
           | NeedMailFromFirst
           | NeedRcptToFirst
           deriving (Eq, Show)

data Command = Helo BS.ByteString
             | Ehlo BS.ByteString
             | MailFrom Mailbox
             | RcptTo Mailbox
             | StartTls
             | Data
             | Rset
             | Quit
             deriving (Eq, Show)

newtype SmtpFSM = SmtpFSM { step :: Command -> TlsStatus -> (Event, SmtpFSM) }

initSmtpFSM :: SmtpFSM
initSmtpFSM = SmtpFSM (handleSmtpCmd Unknown)

handleSmtpCmd :: SessionState -> Command -> TlsStatus -> (Event, SmtpFSM)
handleSmtpCmd st cmd tlsSt = matchEvent tlsSt st cmd
  where
    matchEvent :: TlsStatus -> SessionState -> Command -> (Event, SmtpFSM)
    matchEvent _         HaveQuit  _            = undefined
    matchEvent _         HaveData  Data         = undefined
    matchEvent _         _         Quit         = trans (HaveQuit, WantQuit)
    matchEvent _         Unknown   (Helo x)     = trans (HaveHelo, SayHelo x)
    matchEvent _         _         (Helo x)     = event (SayHeloAgain x)
    matchEvent _         Unknown   (Ehlo x)     = trans (HaveEhlo, SayEhlo x)
    matchEvent _         _         (Ehlo x)     = event (SayEhloAgain x)
    matchEvent Required  _         (MailFrom _) = event NeedStartTlsFirst
    matchEvent _         Unknown   (MailFrom _) = event NeedHeloFirst
    matchEvent _         _         (MailFrom x) = trans (HaveMailFrom, SetMailFrom x)
    matchEvent Required  _         (RcptTo _)   = event NeedStartTlsFirst
    matchEvent _         Unknown   (RcptTo _)   = event NeedHeloFirst
    matchEvent _         HaveHelo  (RcptTo _)   = event NeedMailFromFirst
    matchEvent _         HaveEhlo  (RcptTo _)   = event NeedMailFromFirst
    matchEvent _         _         (RcptTo x)   = trans (HaveRcptTo, AddRcptTo x)
    matchEvent Required  _            Data      = event NeedStartTlsFirst
    matchEvent _         Unknown      Data      = event NeedHeloFirst
    matchEvent _         HaveHelo     Data      = event NeedMailFromFirst
    matchEvent _         HaveEhlo     Data      = event NeedMailFromFirst
    matchEvent _         HaveMailFrom Data      = event NeedRcptToFirst
    matchEvent _         HaveRcptTo   Data      = trans (HaveData, StartData)
    matchEvent Required  _           Rset       = event NeedStartTlsFirst
    matchEvent _         _           Rset       = trans (HaveHelo, WantReset)
    matchEvent Active    _           StartTls   = event TlsAlreadyActive
    matchEvent Forbidden _           StartTls   = event TlsNotSupported
    matchEvent _         _           StartTls   = trans (Unknown, WantTls)

    event :: Event -> (Event, SmtpFSM)
    event e = (e, SmtpFSM (handleSmtpCmd st))

    trans :: (SessionState, Event) -> (Event, SmtpFSM)
    trans (st', e) = (e, SmtpFSM (handleSmtpCmd st'))


type StatusCode = Int

data Reply = Reply StatusCode [LBS.ByteString]

reply :: StatusCode -> LBS.ByteString -> Reply
reply c s = reply' c [s]

reply' :: StatusCode -> [LBS.ByteString] -> Reply
reply' = Reply

renderReply :: Reply -> LBS.ByteString
renderReply (Reply code msgs) = LBS.concat msg'
  where
    prefixCon = LBS.pack (show code ++ "-")
    prefixEnd = LBS.pack (show code ++ " ")
    fmt p l = LBS.concat [p, l, "\r\n"]
    (x:xs) = reverse msgs
    msgCon = map (fmt prefixCon) xs
    msgEnd = fmt prefixEnd x
    msg' = reverse (msgEnd:msgCon)

parseCommand :: Parser Command
parseCommand = commands <* crlf
  where
    commands = choice [
                        parseQuit
                      , parseData
                      , parseRset
                      , parseHelo
                      , parseEhlo
                      , parseStartTls
                      , parseMailFrom
                      , parseRcptTo
                      ]

crlf :: Parser ()
crlf = void $ char '\r' >> char '\n'

parseHello :: (BS.ByteString -> Command) -> BS.ByteString -> Parser Command
parseHello f s = f `fmap` parser
  where
    parser = stringCI s *> char ' ' *> takeWhile (notInClass "\r ")

parseHelo :: Parser Command
parseHelo = parseHello Helo "helo"

parseEhlo :: Parser Command
parseEhlo = parseHello Ehlo "ehlo"

parseMailFrom :: Parser Command
parseMailFrom = stringCI "mail from:<" *> (MailFrom `fmap` addrSpec) <* char '>'

parseRcptTo :: Parser Command
parseRcptTo = stringCI "rcpt to:<" *> (RcptTo `fmap` addrSpec) <* char '>'

parseStartTls :: Parser Command
parseStartTls = stringCI "starttls" *> pure StartTls

parseRset :: Parser Command
parseRset = stringCI "rset" *> pure Rset

parseData :: Parser Command
parseData = stringCI "data" *> pure Data

parseQuit :: Parser Command
parseQuit = stringCI "quit" *> pure Quit
