{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Media where

import qualified Data.ByteString.Char8     as BSC
import           Import
import           Network.Socket            (AddrInfo (..), SocketType (Stream),
                                            close, defaultHints, getAddrInfo,
                                            socket, withSocketsDo)
import qualified Network.Socket            as NS
import           Network.Socket.ByteString (sendAll)
import           System.Directory.Extra    (listFilesRecursive)


getMediaR :: Handler Value
getMediaR = do
    settings <- appSettings <$> getYesod
    files <- traverse (liftIO . listFilesRecursive) (appMediaFolders settings)
    returnJson files

postMediaR ∷ Handler Value
postMediaR = do
  settings ← appSettings <$> getYesod
  liftIO $ withSocketsDo $ do
    addr ← resolve (appRPIAddress settings) (appRPIPort settings)
    bracket (open addr) close sendMessage
  returnJson ("hello" ∷ String)

  where

  resolve host port = do
    let hints = defaultHints { addrSocketType = Stream }
    addr:_ ← getAddrInfo (Just hints) (Just host) (Just port)
    return addr

  open addr = do
    sock ← socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    NS.connect sock $ addrAddress addr
    return sock

  sendMessage sock = do
    sendAll sock (BSC.pack "Hello, world!")
