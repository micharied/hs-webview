-- | Error codes returned by the webview C API.
module WebView.Error
  ( WebviewError (..)
  , fromRaw
  , toResult
  ) where

import qualified WebView.Raw as Raw

-- | Idiomatic Haskell representation of @webview_error_t@.
--
-- @Ok@ is intentionally absent: the idiomatic API converts @WEBVIEW_ERROR_OK@
-- into the 'Right' branch of an 'Either', so an error value is never @Ok@.
-- Unknown values surfacing from a newer C library are wrapped in 'UnknownError'.
data WebviewError
  = MissingDependency
  | Canceled
  | InvalidState
  | InvalidArgument
  | Unspecified
  | Duplicate
  | NotFound
  | UnknownError Int
  deriving (Eq, Show)

-- | Map a raw @webview_error_t@ to a 'WebviewError'. Returns 'Nothing' for
-- @WEBVIEW_ERROR_OK@.
fromRaw :: Raw.Webview_error_t -> Maybe WebviewError
fromRaw raw
  | raw == Raw.WEBVIEW_ERROR_OK                 = Nothing
  | raw == Raw.WEBVIEW_ERROR_MISSING_DEPENDENCY = Just MissingDependency
  | raw == Raw.WEBVIEW_ERROR_CANCELED           = Just Canceled
  | raw == Raw.WEBVIEW_ERROR_INVALID_STATE      = Just InvalidState
  | raw == Raw.WEBVIEW_ERROR_INVALID_ARGUMENT   = Just InvalidArgument
  | raw == Raw.WEBVIEW_ERROR_UNSPECIFIED        = Just Unspecified
  | raw == Raw.WEBVIEW_ERROR_DUPLICATE          = Just Duplicate
  | raw == Raw.WEBVIEW_ERROR_NOT_FOUND          = Just NotFound
  | otherwise = Just (UnknownError (fromIntegral (Raw.unwrapWebview_error_t raw)))

-- | Lift a raw error code into 'Either WebviewError ()'.
toResult :: Raw.Webview_error_t -> Either WebviewError ()
toResult raw = maybe (Right ()) Left (fromRaw raw)
