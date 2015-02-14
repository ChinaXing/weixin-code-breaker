{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- 
-- XML representation of WeiXin TextMessage
-- 2015-02-09 01:30:58
-- 
module WxMessage
       (
         WxTextMessage (..)
       , renderWxTextMessage
       , parseWxTextMessage
       , buildWxTextMessage
       ) where
import Control.Monad
import Text.XML
import Data.Text (Text, pack, unpack,concat)
import Text.XML.Cursor
import Text.Hamlet.XML
import Prelude hiding (writeFile, concat)
import qualified Data.Text.Lazy as LT
import Data.Map (empty)
import Control.Exception (SomeException)
data WxTextMessage = WxTextMsg
     { toUser :: Text
     ,fromUser :: Text
     ,createTime :: Int
     ,msgType :: Text
     ,msgContent :: Text
     ,msgId :: Text
     } deriving (Show, Eq)

buildWxTextMessage :: String -> String -> Int -> String -> String -> String -> WxTextMessage
buildWxTextMessage t f c tt cc i = WxTextMsg {
                   toUser = pack t
                  ,fromUser = pack f
                  ,createTime = c
                  ,msgType =pack  tt
                  ,msgContent = pack cc
                  ,msgId = pack i
                 } 

getDocument :: WxTextMessage -> Document
getDocument msg =
  Document (Prologue [] Nothing [])
           root []
           where
             root = Element "xml" empty [xml|
                       <ToUserName>#{toUser msg}
                       <FromUserName>#{fromUser msg}
                       <CreateTime>#{pack $ show $ createTime msg}
                       <MsgType>#{msgType msg}
                       <Content>#{msgContent msg}
                       <MsgId>#{msgId msg}
                      |]
renderDoc :: Document -> String
renderDoc = LT.unpack . renderText def 

renderWxTextMessage :: WxTextMessage -> String
renderWxTextMessage  = renderDoc . getDocument  



parseWxTextMessage :: String -> Either SomeException WxTextMessage
parseWxTextMessage  str = do
  doc <- parseText def $ LT.fromChunks [pack str]
  let cursor =  fromDocument doc
      _toUser =  concat $ cursor $// element "ToUserName" &// content
      _fromUser = concat $ cursor $// element "FromUserName" &// content
      _createTime = concat $ cursor $// element "CreateTime" &// content
      _msgType = concat $ cursor $// element "MsgType" &// content
      _msgContent = concat $ cursor $// element "Content" &// content
      _msgId = concat $ cursor $// element "MsgId" &// content
      ret = WxTextMsg _toUser _fromUser (read $ unpack _createTime) _msgType _msgContent _msgId
  return ret

-- 
