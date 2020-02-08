{-# LANGUAGE OverloadedStrings   #-}

module Handler.Number where 

import Import

getNumberR :: Handler Value
getNumberR = return $ object ["number" .= (24 :: Int32)]