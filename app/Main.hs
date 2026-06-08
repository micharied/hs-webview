{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Web.Scotty (ScottyM, get, html, redirect, scotty)

import WebView
import WebView.Extras (enableReloadShortcuts)
import qualified Data.Time as Time
import Web.Scotty.Trans (liftIO)

main :: IO ()
main = do
  void . forkIO $ runServer
  putStrLn "Starte WebView ..."
  withWebView False $ \wv -> do
    _ <- setTitle wv "hs-webview demo"
    _ <- setSize wv 640 400 HintNone
    _ <- enableReloadShortcuts wv
    _ <- navigate wv (T.pack helloOneUrl)
    _ <- run wv
    pure ()

runServer :: IO ()
runServer = do
  putStrLn $ "Starte Scotty Server auf " <> serverBaseUrl <> " ..."
  scotty serverPort helloServer

helloServer :: ScottyM ()
helloServer = do
  get "/" $ redirect (LT.pack helloOnePath)
  get "/hello-one" $ do 
    time <- liftIO Time.getCurrentTime
    html (helloOnePage time)
  get "/hello-two" $ html helloTwoPage

helloOnePage :: Time.UTCTime -> LT.Text
helloOnePage time =
  LT.unlines
    [ "<!doctype html>"
    , "<html>"
    , "  <head>"
    , "    <meta charset=\"utf-8\" />"
    , "    <title>Hello Scotty</title>"
    , "    <style>"
    , "      body { font-family: -apple-system, Segoe UI, sans-serif; margin: 2rem; }"
    , "      button { padding: 0.6rem 1.2rem; font-size: 1rem; cursor: pointer; }"
    , "      .field { margin: 1rem 0; }"
    , "      input, textarea { width: 100%; max-width: 32rem; font: inherit; padding: 0.5rem; }"
    , "      .editable { border: 1px solid #ccc; padding: 0.6rem; min-height: 4rem; }"
    , "    </style>"
    , "  </head>"
    , "  <body>"
    , "    <h1>Hello from endpoint one!</h1>"
    , LT.concat ["    <p>Current server time: ", LT.pack (show time), "</p>"]
    , "    <p>Diese Seite wird vom Scotty Server geliefert.</p>"
    , "    <h2>Clipboard Test</h2>"
    , "    <p>Nutze Cmd/Ctrl+C/X/V/A in den Feldern.</p>"
    , "    <div class=\"field\">"
    , "      <input type=\"text\" value=\"Bearbeite diesen Text.\" />"
    , "    </div>"
    , "    <div class=\"field\">"
    , "      <textarea rows=\"4\">Hier kannst du Text markieren und kopieren.</textarea>"
    , "    </div>"
    , "    <div class=\"field editable\" contenteditable=\"true\">"
    , "      Dieser Bereich ist contenteditable."
    , "    </div>"
    , LT.concat
        [ "    <button onclick=\"window.location.href='"
        , LT.pack helloTwoUrl
        , "'\">Weiter zu Hello Two</button>"
        ]
    , "  </body>"
    , "</html>"
    ]

helloTwoPage :: LT.Text
helloTwoPage =
  LT.unlines
    [ "<!doctype html>"
    , "<html>"
    , "  <head>"
    , "    <meta charset=\"utf-8\" />"
    , "    <title>Hello Scotty 2</title>"
    , "    <style>"
    , "      body { font-family: -apple-system, Segoe UI, sans-serif; margin: 2rem; }"
    , "      button { padding: 0.6rem 1.2rem; font-size: 1rem; cursor: pointer; }"
    , "    </style>"
    , "  </head>"
    , "  <body>"
    , "    <h1>Hello from endpoint two!</h1>"
    , "    <p>Auch dieser Inhalt kommt vom Scotty Server.</p>"
    , LT.concat
        [ "    <button onclick=\"window.location.href='"
        , LT.pack helloOneUrl
        , "'\">Zurueck zu Hello One</button>"
        ]
    , "  </body>"
    , "</html>"
    ]

serverHost :: String
serverHost = "localhost"

serverPort :: Int
serverPort = 3737

helloOnePath :: String
helloOnePath = "/hello-one"

helloTwoPath :: String
helloTwoPath = "/hello-two"

helloOneUrl :: String
helloOneUrl = serverBaseUrl <> helloOnePath

helloTwoUrl :: String
helloTwoUrl = serverBaseUrl <> helloTwoPath

serverBaseUrl :: String
serverBaseUrl = "http://" <> serverHost <> ":" <> show serverPort
