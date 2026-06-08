{-# LANGUAGE OverloadedStrings #-}

module WebView.Extras (enableReloadShortcuts) where

import Data.Text (Text)
import qualified Data.Text as T
import Foreign.C.String (withCString)
import qualified HsBindgen.Runtime.PtrConst as PtrConst

import WebView.Error (WebviewError, toResult)
import WebView.Internal (WebView)
import qualified WebView.Raw.Unsafe as Raw

-- | Install Cmd/Ctrl+R and F5 shortcuts for the current and future pages.
-- Call this before any navigation/setHtml to affect the first load.
enableReloadShortcuts :: WebView -> IO (Either WebviewError ())
enableReloadShortcuts wv =
  withCString (T.unpack reloadShortcutScript) $ \cstr ->
    toResult <$> Raw.webview_init wv (PtrConst.unsafeFromPtr cstr)

reloadShortcutScript :: Text
reloadShortcutScript = T.unlines
  [ "(function () {"
  , "  if (window.__hsWebviewReloadShortcuts) return;"
  , "  window.__hsWebviewReloadShortcuts = true;"
  , "  window.addEventListener('keydown', function (event) {"
  , "    var isReload = (event.key === 'r' || event.key === 'R') &&"
  , "      (event.metaKey || event.ctrlKey);"
  , "    var isF5 = event.key === 'F5';"
  , "    if (isReload || isF5) {"
  , "      event.preventDefault();"
  , "      window.location.reload();"
  , "    }"
  , "  }, true);"
  , "})();"
  ]
