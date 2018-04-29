{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Media where

import           Import
import           System.Directory.Extra (listFilesRecursive)


getMediaR :: Handler Value
getMediaR = do
    settings <- appSettings <$> getYesod
    files <- traverse (liftIO . listFilesRecursive) (appMediaFolders settings)
    returnJson files
