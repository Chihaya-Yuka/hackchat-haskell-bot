{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Data.Aeson (encode, decode, object, (.=), Value)
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.Text as T
import qualified Network.WebSockets as WS

data HackChat = HackChat
    { nick       :: T.Text
    , channel    :: T.Text
    , onMessage  :: [HackChat -> T.Text -> T.Text -> IO ()]
    , onWhisper  :: [HackChat -> T.Text -> T.Text -> Value -> IO ()]
    , onJoin     :: [HackChat -> T.Text -> IO ()]
    , onLeave    :: [HackChat -> T.Text -> IO ()]
    , onlineUsers :: [T.Text]
    , connection :: WS.Connection
    }

connectToChannel :: T.Text -> T.Text -> IO HackChat
connectToChannel nick channel = do
    conn <- WS.runClient "hack.chat" 443 "/chat-ws" WS.defaultConnectionOptions [] (return . id)
    let hc = HackChat nick channel [] [] [] [] [] conn
    sendPacket hc $ object ["cmd" .= ("join" :: T.Text), "channel" .= channel, "nick" .= nick]
    _ <- forkIO $ pingThread hc
    return hc

sendPacket :: HackChat -> Value -> IO ()
sendPacket hc packet = WS.sendTextData (connection hc) (encode packet)

sendMessage :: HackChat -> T.Text -> IO ()
sendMessage hc msg = sendPacket hc $ object ["cmd" .= ("chat" :: T.Text), "text" .= msg]

sendTo :: HackChat -> T.Text -> T.Text -> IO ()
sendTo hc target msg = sendPacket hc $ object ["cmd" .= ("whisper" :: T.Text), "nick" .= target, "text" .= msg]

moveChannel :: HackChat -> T.Text -> IO ()
moveChannel hc newChannel = sendPacket hc $ object ["cmd" .= ("move" :: T.Text), "channel" .= newChannel]

changeNick :: HackChat -> T.Text -> IO ()
changeNick hc newNick = sendPacket hc $ object ["cmd" .= ("changenick" :: T.Text), "nick" .= newNick]

runDaemon :: HackChat -> IO ()
runDaemon hc = do
    _ <- forkIO $ run hc
    return ()

run :: HackChat -> IO ()
run hc = forever $ do
    msg <- WS.receiveData (connection hc)
    case decode msg of
        Just obj -> handlePacket hc obj
        Nothing  -> return ()

handlePacket :: HackChat -> Value -> IO ()
handlePacket hc obj = do
    case obj of
        Object o -> case lookup "cmd" o of
            Just "chat" -> 
                let nick = o ! "nick"
                    text = o ! "text"
                in if nick /= hcNick hc
                   then mapM_ (\h -> h hc text nick) (onMessage hc)
                   else return ()
            Just "onlineAdd" -> do
                let nick = o ! "nick"
                modifyOnlineUsers hc (nick :)
                mapM_ (\h -> h hc nick) (onJoin hc)
            Just "onlineRemove" -> do
                let nick = o ! "nick"
                modifyOnlineUsers hc (filter (/= nick))
                mapM_ (\h -> h hc nick) (onLeave hc)
            Just "onlineSet" -> do
                let nicks = o ! "nicks"
                modifyOnlineUsers hc (const nicks)
            Just "info" -> 
                if lookup "type" o == Just "whisper"
                then let from = o ! "from"
                         text = o ! "text"
                     in mapM_ (\h -> h hc text from obj) (onWhisper hc)
                else return ()
            _ -> return ()
        _ -> return ()

pingThread :: HackChat -> IO ()
pingThread hc = forever $ do
    sendPacket hc $ object ["cmd" .= ("ping" :: T.Text)]
    threadDelay 60000000 -- 60 seconds

modifyOnlineUsers :: HackChat -> ([T.Text] -> [T.Text]) -> IO ()
modifyOnlineUsers hc f = do
    let newOnlineUsers = f (onlineUsers hc)
    return hc { onlineUsers = newOnlineUsers }

(!) :: Value -> T.Text -> T.Text
Object o ! key = case lookup key o of
    Just (String v) -> v
    _ -> error "Key not found or not a String"
_ ! _ = error "Expected Object"
