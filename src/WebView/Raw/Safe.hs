{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module WebView.Raw.Safe
    ( WebView.Raw.Safe.webview_create
    , WebView.Raw.Safe.webview_destroy
    , WebView.Raw.Safe.webview_run
    , WebView.Raw.Safe.webview_terminate
    , WebView.Raw.Safe.webview_dispatch
    , WebView.Raw.Safe.webview_get_window
    , WebView.Raw.Safe.webview_get_native_handle
    , WebView.Raw.Safe.webview_set_title
    , WebView.Raw.Safe.webview_set_size
    , WebView.Raw.Safe.webview_navigate
    , WebView.Raw.Safe.webview_set_html
    , WebView.Raw.Safe.webview_init
    , WebView.Raw.Safe.webview_eval
    , WebView.Raw.Safe.webview_bind
    , WebView.Raw.Safe.webview_unbind
    , WebView.Raw.Safe.webview_return
    , WebView.Raw.Safe.webview_version
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import WebView.Raw

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <webview/webview.h>"
  , "webview_t hs_bindgen_57a100a2dbed06ad ("
  , "  signed int arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  return (webview_create)(arg1, arg2);"
  , "}"
  , "webview_error_t hs_bindgen_7cfa88c6415d344e ("
  , "  webview_t arg1"
  , ")"
  , "{"
  , "  return (webview_destroy)(arg1);"
  , "}"
  , "webview_error_t hs_bindgen_2f1487d6f0824c7e ("
  , "  webview_t arg1"
  , ")"
  , "{"
  , "  return (webview_run)(arg1);"
  , "}"
  , "webview_error_t hs_bindgen_ffab00ebb2450d5b ("
  , "  webview_t arg1"
  , ")"
  , "{"
  , "  return (webview_terminate)(arg1);"
  , "}"
  , "webview_error_t hs_bindgen_cb7a0f2cf1026896 ("
  , "  webview_t arg1,"
  , "  void (*arg2) ("
  , "  webview_t arg1,"
  , "  void *arg2"
  , "),"
  , "  void *arg3"
  , ")"
  , "{"
  , "  return (webview_dispatch)(arg1, arg2, arg3);"
  , "}"
  , "void *hs_bindgen_5aec7b10644a8fcc ("
  , "  webview_t arg1"
  , ")"
  , "{"
  , "  return (webview_get_window)(arg1);"
  , "}"
  , "void *hs_bindgen_b0ec7d0f11f68ed1 ("
  , "  webview_t arg1,"
  , "  webview_native_handle_kind_t arg2"
  , ")"
  , "{"
  , "  return (webview_get_native_handle)(arg1, arg2);"
  , "}"
  , "webview_error_t hs_bindgen_e8ff0c4c1185b01c ("
  , "  webview_t arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return (webview_set_title)(arg1, arg2);"
  , "}"
  , "webview_error_t hs_bindgen_c76fbfab4f559881 ("
  , "  webview_t arg1,"
  , "  signed int arg2,"
  , "  signed int arg3,"
  , "  webview_hint_t arg4"
  , ")"
  , "{"
  , "  return (webview_set_size)(arg1, arg2, arg3, arg4);"
  , "}"
  , "webview_error_t hs_bindgen_884ae06ecda3c256 ("
  , "  webview_t arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return (webview_navigate)(arg1, arg2);"
  , "}"
  , "webview_error_t hs_bindgen_a13bf572da9e1384 ("
  , "  webview_t arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return (webview_set_html)(arg1, arg2);"
  , "}"
  , "webview_error_t hs_bindgen_521e873d09a4d7a5 ("
  , "  webview_t arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return (webview_init)(arg1, arg2);"
  , "}"
  , "webview_error_t hs_bindgen_69fc8b7b4d230359 ("
  , "  webview_t arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return (webview_eval)(arg1, arg2);"
  , "}"
  , "webview_error_t hs_bindgen_c2b14124ccaaea79 ("
  , "  webview_t arg1,"
  , "  char const *arg2,"
  , "  void (*arg3) ("
  , "  char const *arg1,"
  , "  char const *arg2,"
  , "  void *arg3"
  , "),"
  , "  void *arg4"
  , ")"
  , "{"
  , "  return (webview_bind)(arg1, arg2, arg3, arg4);"
  , "}"
  , "webview_error_t hs_bindgen_eecdb5b94cb92b3c ("
  , "  webview_t arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return (webview_unbind)(arg1, arg2);"
  , "}"
  , "webview_error_t hs_bindgen_bd12679eb6e0f329 ("
  , "  webview_t arg1,"
  , "  char const *arg2,"
  , "  signed int arg3,"
  , "  char const *arg4"
  , ")"
  , "{"
  , "  return (webview_return)(arg1, arg2, arg3, arg4);"
  , "}"
  , "webview_version_info_t const *hs_bindgen_d9c157c58e92278e (void)"
  , "{"
  , "  return (webview_version)();"
  , "}"
  ]))

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_create@
foreign import ccall safe "hs_bindgen_57a100a2dbed06ad" hs_bindgen_57a100a2dbed06ad_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_create@
hs_bindgen_57a100a2dbed06ad ::
     RIP.CInt
  -> RIP.Ptr RIP.Void
  -> IO Webview_t
hs_bindgen_57a100a2dbed06ad =
  RIP.fromFFIType hs_bindgen_57a100a2dbed06ad_base

{-| __C declaration:__ @webview_create@

    __defined at:__ @api.h 60:23@

    __exported by:__ @webview\/webview.h@
-}
webview_create ::
     RIP.CInt
     -- ^ __C declaration:__ @debug@
  -> RIP.Ptr RIP.Void
     -- ^ __C declaration:__ @window@
  -> IO Webview_t
webview_create = hs_bindgen_57a100a2dbed06ad

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_destroy@
foreign import ccall safe "hs_bindgen_7cfa88c6415d344e" hs_bindgen_7cfa88c6415d344e_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_destroy@
hs_bindgen_7cfa88c6415d344e ::
     Webview_t
  -> IO Webview_error_t
hs_bindgen_7cfa88c6415d344e =
  RIP.fromFFIType hs_bindgen_7cfa88c6415d344e_base

{-| __C declaration:__ @webview_destroy@

    __defined at:__ @api.h 67:29@

    __exported by:__ @webview\/webview.h@
-}
webview_destroy ::
     Webview_t
     -- ^ __C declaration:__ @w@
  -> IO Webview_error_t
webview_destroy = hs_bindgen_7cfa88c6415d344e

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_run@
foreign import ccall safe "hs_bindgen_2f1487d6f0824c7e" hs_bindgen_2f1487d6f0824c7e_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_run@
hs_bindgen_2f1487d6f0824c7e ::
     Webview_t
  -> IO Webview_error_t
hs_bindgen_2f1487d6f0824c7e =
  RIP.fromFFIType hs_bindgen_2f1487d6f0824c7e_base

{-| __C declaration:__ @webview_run@

    __defined at:__ @api.h 74:29@

    __exported by:__ @webview\/webview.h@
-}
webview_run ::
     Webview_t
     -- ^ __C declaration:__ @w@
  -> IO Webview_error_t
webview_run = hs_bindgen_2f1487d6f0824c7e

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_terminate@
foreign import ccall safe "hs_bindgen_ffab00ebb2450d5b" hs_bindgen_ffab00ebb2450d5b_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_terminate@
hs_bindgen_ffab00ebb2450d5b ::
     Webview_t
  -> IO Webview_error_t
hs_bindgen_ffab00ebb2450d5b =
  RIP.fromFFIType hs_bindgen_ffab00ebb2450d5b_base

{-| __C declaration:__ @webview_terminate@

    __defined at:__ @api.h 82:29@

    __exported by:__ @webview\/webview.h@
-}
webview_terminate ::
     Webview_t
     -- ^ __C declaration:__ @w@
  -> IO Webview_error_t
webview_terminate = hs_bindgen_ffab00ebb2450d5b

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_dispatch@
foreign import ccall safe "hs_bindgen_cb7a0f2cf1026896" hs_bindgen_cb7a0f2cf1026896_base ::
     RIP.Ptr RIP.Void
  -> RIP.FunPtr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_dispatch@
hs_bindgen_cb7a0f2cf1026896 ::
     Webview_t
  -> RIP.FunPtr (Webview_t -> RIP.Ptr RIP.Void -> IO ())
  -> RIP.Ptr RIP.Void
  -> IO Webview_error_t
hs_bindgen_cb7a0f2cf1026896 =
  RIP.fromFFIType hs_bindgen_cb7a0f2cf1026896_base

{-| __C declaration:__ @webview_dispatch@

    __defined at:__ @api.h 95:29@

    __exported by:__ @webview\/webview.h@
-}
webview_dispatch ::
     Webview_t
     -- ^ __C declaration:__ @w@
  -> RIP.FunPtr (Webview_t -> RIP.Ptr RIP.Void -> IO ())
     -- ^ __C declaration:__ @fn@
  -> RIP.Ptr RIP.Void
     -- ^ __C declaration:__ @arg@
  -> IO Webview_error_t
webview_dispatch = hs_bindgen_cb7a0f2cf1026896

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_get_window@
foreign import ccall safe "hs_bindgen_5aec7b10644a8fcc" hs_bindgen_5aec7b10644a8fcc_base ::
     RIP.Ptr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_get_window@
hs_bindgen_5aec7b10644a8fcc ::
     Webview_t
  -> IO (RIP.Ptr RIP.Void)
hs_bindgen_5aec7b10644a8fcc =
  RIP.fromFFIType hs_bindgen_5aec7b10644a8fcc_base

{-| __C declaration:__ @webview_get_window@

    __defined at:__ @api.h 107:19@

    __exported by:__ @webview\/webview.h@
-}
webview_get_window ::
     Webview_t
     -- ^ __C declaration:__ @w@
  -> IO (RIP.Ptr RIP.Void)
webview_get_window = hs_bindgen_5aec7b10644a8fcc

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_get_native_handle@
foreign import ccall safe "hs_bindgen_b0ec7d0f11f68ed1" hs_bindgen_b0ec7d0f11f68ed1_base ::
     RIP.Ptr RIP.Void
  -> RIP.Word32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_get_native_handle@
hs_bindgen_b0ec7d0f11f68ed1 ::
     Webview_t
  -> Webview_native_handle_kind_t
  -> IO (RIP.Ptr RIP.Void)
hs_bindgen_b0ec7d0f11f68ed1 =
  RIP.fromFFIType hs_bindgen_b0ec7d0f11f68ed1_base

{-| __C declaration:__ @webview_get_native_handle@

    __defined at:__ @api.h 117:19@

    __exported by:__ @webview\/webview.h@
-}
webview_get_native_handle ::
     Webview_t
     -- ^ __C declaration:__ @w@
  -> Webview_native_handle_kind_t
     -- ^ __C declaration:__ @kind@
  -> IO (RIP.Ptr RIP.Void)
webview_get_native_handle =
  hs_bindgen_b0ec7d0f11f68ed1

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_set_title@
foreign import ccall safe "hs_bindgen_e8ff0c4c1185b01c" hs_bindgen_e8ff0c4c1185b01c_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_set_title@
hs_bindgen_e8ff0c4c1185b01c ::
     Webview_t
  -> PtrConst.PtrConst RIP.CChar
  -> IO Webview_error_t
hs_bindgen_e8ff0c4c1185b01c =
  RIP.fromFFIType hs_bindgen_e8ff0c4c1185b01c_base

{-| __C declaration:__ @webview_set_title@

    __defined at:__ @api.h 126:29@

    __exported by:__ @webview\/webview.h@
-}
webview_set_title ::
     Webview_t
     -- ^ __C declaration:__ @w@
  -> PtrConst.PtrConst RIP.CChar
     -- ^ __C declaration:__ @title@
  -> IO Webview_error_t
webview_set_title = hs_bindgen_e8ff0c4c1185b01c

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_set_size@
foreign import ccall safe "hs_bindgen_c76fbfab4f559881" hs_bindgen_c76fbfab4f559881_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int32
  -> RIP.Int32
  -> RIP.Word32
  -> IO RIP.Int32

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_set_size@
hs_bindgen_c76fbfab4f559881 ::
     Webview_t
  -> RIP.CInt
  -> RIP.CInt
  -> Webview_hint_t
  -> IO Webview_error_t
hs_bindgen_c76fbfab4f559881 =
  RIP.fromFFIType hs_bindgen_c76fbfab4f559881_base

{-| __C declaration:__ @webview_set_size@

    __defined at:__ @api.h 144:29@

    __exported by:__ @webview\/webview.h@
-}
webview_set_size ::
     Webview_t
     -- ^ __C declaration:__ @w@
  -> RIP.CInt
     -- ^ __C declaration:__ @width@
  -> RIP.CInt
     -- ^ __C declaration:__ @height@
  -> Webview_hint_t
     -- ^ __C declaration:__ @hints@
  -> IO Webview_error_t
webview_set_size = hs_bindgen_c76fbfab4f559881

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_navigate@
foreign import ccall safe "hs_bindgen_884ae06ecda3c256" hs_bindgen_884ae06ecda3c256_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_navigate@
hs_bindgen_884ae06ecda3c256 ::
     Webview_t
  -> PtrConst.PtrConst RIP.CChar
  -> IO Webview_error_t
hs_bindgen_884ae06ecda3c256 =
  RIP.fromFFIType hs_bindgen_884ae06ecda3c256_base

{-| __C declaration:__ @webview_navigate@

    __defined at:__ @api.h 160:29@

    __exported by:__ @webview\/webview.h@
-}
webview_navigate ::
     Webview_t
     -- ^ __C declaration:__ @w@
  -> PtrConst.PtrConst RIP.CChar
     -- ^ __C declaration:__ @url@
  -> IO Webview_error_t
webview_navigate = hs_bindgen_884ae06ecda3c256

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_set_html@
foreign import ccall safe "hs_bindgen_a13bf572da9e1384" hs_bindgen_a13bf572da9e1384_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_set_html@
hs_bindgen_a13bf572da9e1384 ::
     Webview_t
  -> PtrConst.PtrConst RIP.CChar
  -> IO Webview_error_t
hs_bindgen_a13bf572da9e1384 =
  RIP.fromFFIType hs_bindgen_a13bf572da9e1384_base

{-| __C declaration:__ @webview_set_html@

    __defined at:__ @api.h 173:29@

    __exported by:__ @webview\/webview.h@
-}
webview_set_html ::
     Webview_t
     -- ^ __C declaration:__ @w@
  -> PtrConst.PtrConst RIP.CChar
     -- ^ __C declaration:__ @html@
  -> IO Webview_error_t
webview_set_html = hs_bindgen_a13bf572da9e1384

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_init@
foreign import ccall safe "hs_bindgen_521e873d09a4d7a5" hs_bindgen_521e873d09a4d7a5_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_init@
hs_bindgen_521e873d09a4d7a5 ::
     Webview_t
  -> PtrConst.PtrConst RIP.CChar
  -> IO Webview_error_t
hs_bindgen_521e873d09a4d7a5 =
  RIP.fromFFIType hs_bindgen_521e873d09a4d7a5_base

{-| __C declaration:__ @webview_init@

    __defined at:__ @api.h 182:29@

    __exported by:__ @webview\/webview.h@
-}
webview_init ::
     Webview_t
     -- ^ __C declaration:__ @w@
  -> PtrConst.PtrConst RIP.CChar
     -- ^ __C declaration:__ @js@
  -> IO Webview_error_t
webview_init = hs_bindgen_521e873d09a4d7a5

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_eval@
foreign import ccall safe "hs_bindgen_69fc8b7b4d230359" hs_bindgen_69fc8b7b4d230359_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_eval@
hs_bindgen_69fc8b7b4d230359 ::
     Webview_t
  -> PtrConst.PtrConst RIP.CChar
  -> IO Webview_error_t
hs_bindgen_69fc8b7b4d230359 =
  RIP.fromFFIType hs_bindgen_69fc8b7b4d230359_base

{-| __C declaration:__ @webview_eval@

    __defined at:__ @api.h 192:29@

    __exported by:__ @webview\/webview.h@
-}
webview_eval ::
     Webview_t
     -- ^ __C declaration:__ @w@
  -> PtrConst.PtrConst RIP.CChar
     -- ^ __C declaration:__ @js@
  -> IO Webview_error_t
webview_eval = hs_bindgen_69fc8b7b4d230359

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_bind@
foreign import ccall safe "hs_bindgen_c2b14124ccaaea79" hs_bindgen_c2b14124ccaaea79_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> RIP.FunPtr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_bind@
hs_bindgen_c2b14124ccaaea79 ::
     Webview_t
  -> PtrConst.PtrConst RIP.CChar
  -> RIP.FunPtr (PtrConst.PtrConst RIP.CChar -> PtrConst.PtrConst RIP.CChar -> RIP.Ptr RIP.Void -> IO ())
  -> RIP.Ptr RIP.Void
  -> IO Webview_error_t
hs_bindgen_c2b14124ccaaea79 =
  RIP.fromFFIType hs_bindgen_c2b14124ccaaea79_base

{-| __C declaration:__ @webview_bind@

    __defined at:__ @api.h 209:29@

    __exported by:__ @webview\/webview.h@
-}
webview_bind ::
     Webview_t
     -- ^ __C declaration:__ @w@
  -> PtrConst.PtrConst RIP.CChar
     -- ^ __C declaration:__ @name@
  -> RIP.FunPtr (PtrConst.PtrConst RIP.CChar -> PtrConst.PtrConst RIP.CChar -> RIP.Ptr RIP.Void -> IO ())
     -- ^ __C declaration:__ @fn@
  -> RIP.Ptr RIP.Void
     -- ^ __C declaration:__ @arg@
  -> IO Webview_error_t
webview_bind = hs_bindgen_c2b14124ccaaea79

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_unbind@
foreign import ccall safe "hs_bindgen_eecdb5b94cb92b3c" hs_bindgen_eecdb5b94cb92b3c_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_unbind@
hs_bindgen_eecdb5b94cb92b3c ::
     Webview_t
  -> PtrConst.PtrConst RIP.CChar
  -> IO Webview_error_t
hs_bindgen_eecdb5b94cb92b3c =
  RIP.fromFFIType hs_bindgen_eecdb5b94cb92b3c_base

{-| __C declaration:__ @webview_unbind@

    __defined at:__ @api.h 221:29@

    __exported by:__ @webview\/webview.h@
-}
webview_unbind ::
     Webview_t
     -- ^ __C declaration:__ @w@
  -> PtrConst.PtrConst RIP.CChar
     -- ^ __C declaration:__ @name@
  -> IO Webview_error_t
webview_unbind = hs_bindgen_eecdb5b94cb92b3c

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_return@
foreign import ccall safe "hs_bindgen_bd12679eb6e0f329" hs_bindgen_bd12679eb6e0f329_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_return@
hs_bindgen_bd12679eb6e0f329 ::
     Webview_t
  -> PtrConst.PtrConst RIP.CChar
  -> RIP.CInt
  -> PtrConst.PtrConst RIP.CChar
  -> IO Webview_error_t
hs_bindgen_bd12679eb6e0f329 =
  RIP.fromFFIType hs_bindgen_bd12679eb6e0f329_base

{-| __C declaration:__ @webview_return@

    __defined at:__ @api.h 237:29@

    __exported by:__ @webview\/webview.h@
-}
webview_return ::
     Webview_t
     -- ^ __C declaration:__ @w@
  -> PtrConst.PtrConst RIP.CChar
     -- ^ __C declaration:__ @id@
  -> RIP.CInt
     -- ^ __C declaration:__ @status@
  -> PtrConst.PtrConst RIP.CChar
     -- ^ __C declaration:__ @result@
  -> IO Webview_error_t
webview_return = hs_bindgen_bd12679eb6e0f329

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_version@
foreign import ccall safe "hs_bindgen_d9c157c58e92278e" hs_bindgen_d9c157c58e92278e_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Safe_webview_version@
hs_bindgen_d9c157c58e92278e :: IO (PtrConst.PtrConst Webview_version_info_t)
hs_bindgen_d9c157c58e92278e =
  RIP.fromFFIType hs_bindgen_d9c157c58e92278e_base

{-| __C declaration:__ @webview_version@

    __defined at:__ @api.h 245:43@

    __exported by:__ @webview\/webview.h@
-}
webview_version :: IO (PtrConst.PtrConst Webview_version_info_t)
webview_version = hs_bindgen_d9c157c58e92278e
