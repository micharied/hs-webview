{-# LANGUAGE ForeignFunctionInterface #-}

-- | Minimal Haskell bindings for the official C webview API.
module WebView
  ( WebView
  , WebViewHint (..)
  , create
  , destroy
  , run
  , terminate
  , setTitle
  , setSize
  , setHtml
  , navigate
  , withWebView
  ) where

import Control.Exception (bracket)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Utils (fromBool)
import Foreign.Ptr (Ptr, nullPtr)

newtype WebView = WebView (Ptr ())

-- | Window sizing hints understood by webview.
data WebViewHint
  = HintNone
  | HintMin
  | HintMax
  | HintFixed
  deriving (Eq, Show)

create :: Bool -> IO WebView
create debug = WebView <$> c_webview_create (fromBool debug) nullPtr

run :: WebView -> IO ()
run (WebView ptr) = c_webview_run ptr

destroy :: WebView -> IO ()
destroy (WebView ptr) = c_webview_destroy ptr

terminate :: WebView -> IO ()
terminate (WebView ptr) = c_webview_terminate ptr

setTitle :: WebView -> String -> IO ()
setTitle (WebView ptr) title = withCString title (c_webview_set_title ptr)

setSize :: WebView -> Int -> Int -> WebViewHint -> IO ()
setSize (WebView ptr) width height hint =
  c_webview_set_size ptr (fromIntegral width) (fromIntegral height) (hintToC hint)

setHtml :: WebView -> String -> IO ()
setHtml (WebView ptr) html = withCString html (c_webview_set_html ptr)

navigate :: WebView -> String -> IO ()
navigate (WebView ptr) url = withCString url (c_webview_navigate ptr)

withWebView :: Bool -> (WebView -> IO a) -> IO a
withWebView debug = bracket (create debug) destroy

hintToC :: WebViewHint -> CInt
hintToC HintNone  = 0
hintToC HintMin   = 1
hintToC HintMax   = 2
hintToC HintFixed = 3

foreign import ccall unsafe "webview_create"
  c_webview_create :: CInt -> Ptr () -> IO (Ptr ())

-- | 'webview_run' enters the GUI loop and blocks, so mark it as a safe call to
-- allow other Haskell threads (like our Scotty server) to keep running.
foreign import ccall safe "webview_run"
  c_webview_run :: Ptr () -> IO ()

foreign import ccall unsafe "webview_destroy"
  c_webview_destroy :: Ptr () -> IO ()

foreign import ccall unsafe "webview_terminate"
  c_webview_terminate :: Ptr () -> IO ()

foreign import ccall unsafe "webview_set_title"
  c_webview_set_title :: Ptr () -> CString -> IO ()

foreign import ccall unsafe "webview_set_size"
  c_webview_set_size :: Ptr () -> CInt -> CInt -> CInt -> IO ()

foreign import ccall unsafe "webview_set_html"
  c_webview_set_html :: Ptr () -> CString -> IO ()

foreign import ccall unsafe "webview_navigate"
  c_webview_navigate :: Ptr () -> CString -> IO ()
