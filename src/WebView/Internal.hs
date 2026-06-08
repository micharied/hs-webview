-- | Internal re-exports. Power users that need to drop down to the raw FFI
-- can import this module to access the underlying 'Webview_t' constructor.
module WebView.Internal
  ( WebView
  , unsafeFromRaw
  , toRaw
  ) where

import qualified WebView.Raw as Raw

-- | Opaque handle to a webview instance.
type WebView = Raw.Webview_t

-- | Wrap a raw @webview_t@ pointer. Intended for power users only; the
-- normal way to obtain a 'WebView' is via "WebView".'WebView.create'.
unsafeFromRaw :: Raw.Webview_t -> WebView
unsafeFromRaw = id

-- | Project to the raw @webview_t@. Use this when calling into
-- "WebView.Raw.Safe" or "WebView.Raw.Unsafe" directly.
toRaw :: WebView -> Raw.Webview_t
toRaw = id
