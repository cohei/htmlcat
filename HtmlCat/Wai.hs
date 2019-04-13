{-# LANGUAGE OverloadedStrings #-}
module HtmlCat.Wai (feedStdIn, runHtmlCat) where

import qualified Blaze.ByteString.Builder.Char.Utf8 as B
import           Conduit                            (MonadThrow, MonadUnliftIO,
                                                     runResourceT, sourceHandle)
import           Control.Concurrent                 (Chan, forkIO, writeChan)
import           Control.Monad                      (forever, void)
import           Control.Monad.Trans                (MonadIO (..))
import           Data.Conduit                       (Conduit, Sink, Source,
                                                     await, ($$), ($=))
import qualified Data.Conduit.List                  as CL
import           Data.Conduit.Text                  (decode, utf8)
import           Data.String                        (fromString)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Network.HTTP.Types                 (hContentType, notFound404,
                                                     ok200)
import           Network.Wai                        (Application, Request (..),
                                                     responseBuilder,
                                                     responseLBS)
import           Network.Wai.EventSource            (ServerEvent (..),
                                                     eventSourceAppChan)
import           Network.Wai.Handler.Warp           (defaultSettings,
                                                     runSettings, setHost,
                                                     setPort)
import           Prelude                            hiding (lines)
import           System.IO                          (stdin)
import           Text.Blaze.Html.Renderer.Utf8      (renderHtmlBuilder)

import           HtmlCat.Html                       (html)

feedStdIn :: Chan ServerEvent -> IO ()
feedStdIn chan = void . forkIO . runResourceT $
  sourceStdIn $= lines $= textsToEventSource $$ sinkChan chan

runHtmlCat :: Chan ServerEvent -> String -> Int -> IO ()
runHtmlCat chan host port =
  runSettings (setHost (fromString host) $ setPort port defaultSettings)
              (app chan)

app :: Chan ServerEvent -> Application
app chan req =
  case pathInfo req of
    []         -> appTop req
    ["stream"] -> appStream chan req
    _          -> app404 req

appTop :: Application
appTop _ respond = respond $
  responseBuilder ok200
                  [(hContentType, "text/html; charset=utf-8")]
                  (renderHtmlBuilder html)

appStream :: Chan ServerEvent -> Application
appStream = eventSourceAppChan

app404 :: Application
app404 _ respond = respond $ responseLBS notFound404 [] "Not found"

sourceStdIn :: (MonadIO m, MonadThrow m) => Source m Text
sourceStdIn = sourceHandle stdin $= decode utf8

lines :: Monad m => Conduit Text m [Text]
lines = CL.map T.lines

textsToEventSource :: Monad m => Conduit [Text] m ServerEvent
textsToEventSource = CL.map f
  where
    f texts = ServerEvent { eventName = Nothing
                          , eventId   = Nothing
                          , eventData = map B.fromText texts }

sinkChan :: MonadUnliftIO m => Chan a -> Sink a m ()
sinkChan chan = forever $ await >>= maybe (pure ()) (liftIO . writeChan chan)
