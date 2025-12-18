{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import qualified Data.Text.Lazy as LT
import Web.Scotty (ScottyM, get, html, redirect, scotty)

import WebView

main :: IO ()
main = do
  void . forkIO $ runServer
  putStrLn "Starte WebView ..."
  withWebView False $ \wv -> do
    setTitle wv "hs-webview demo"
    setSize wv 640 400 HintNone
    navigate wv helloOneUrl
    run wv

runServer :: IO ()
runServer = do
  putStrLn $ "Starte Scotty Server auf " <> serverBaseUrl <> " ..."
  scotty serverPort helloServer

helloServer :: ScottyM ()
helloServer = do
  get "/" $ redirect (LT.pack helloOnePath)
  get "/hello-one" $ html helloOnePage
  get "/hello-two" $ html helloTwoPage

helloOnePage :: LT.Text
helloOnePage =
  LT.unlines
    [ "<!doctype html>"
    , "<html>"
    , "  <head>"
    , "    <meta charset=\"utf-8\" />"
    , "    <title>Hello Scotty</title>"
    , "    <style>"
    , "      body { font-family: -apple-system, Segoe UI, sans-serif; margin: 2rem; }"
    , "      button { padding: 0.6rem 1.2rem; font-size: 1rem; cursor: pointer; }"
    , "    </style>"
    , "  </head>"
    , "  <body>"
    , "    <h1>Hello from endpoint one!</h1>"
    , "    <p>Diese Seite wird vom Scotty Server geliefert.</p>"
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
