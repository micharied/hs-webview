{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module WebView.Raw.Unsafe
    ( WebView.Raw.Unsafe.webview_create
    , WebView.Raw.Unsafe.webview_destroy
    , WebView.Raw.Unsafe.webview_run
    , WebView.Raw.Unsafe.webview_terminate
    , WebView.Raw.Unsafe.webview_dispatch
    , WebView.Raw.Unsafe.webview_get_window
    , WebView.Raw.Unsafe.webview_get_native_handle
    , WebView.Raw.Unsafe.webview_set_title
    , WebView.Raw.Unsafe.webview_set_size
    , WebView.Raw.Unsafe.webview_navigate
    , WebView.Raw.Unsafe.webview_set_html
    , WebView.Raw.Unsafe.webview_init
    , WebView.Raw.Unsafe.webview_eval
    , WebView.Raw.Unsafe.webview_bind
    , WebView.Raw.Unsafe.webview_unbind
    , WebView.Raw.Unsafe.webview_return
    , WebView.Raw.Unsafe.webview_version
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import WebView.Raw

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <webview/webview.h>"
  , "webview_t hs_bindgen_a5a8a94fcfc7c716 ("
  , "  signed int arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  return (webview_create)(arg1, arg2);"
  , "}"
  , "webview_error_t hs_bindgen_05c427a35c12d654 ("
  , "  webview_t arg1"
  , ")"
  , "{"
  , "  return (webview_destroy)(arg1);"
  , "}"
  , "webview_error_t hs_bindgen_8608f6bc8f77a97e ("
  , "  webview_t arg1"
  , ")"
  , "{"
  , "  return (webview_run)(arg1);"
  , "}"
  , "webview_error_t hs_bindgen_246ed776acecea2b ("
  , "  webview_t arg1"
  , ")"
  , "{"
  , "  return (webview_terminate)(arg1);"
  , "}"
  , "webview_error_t hs_bindgen_1c97d298cfd9383c ("
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
  , "void *hs_bindgen_5ef552e574675a11 ("
  , "  webview_t arg1"
  , ")"
  , "{"
  , "  return (webview_get_window)(arg1);"
  , "}"
  , "void *hs_bindgen_a3acd72e74fffd6f ("
  , "  webview_t arg1,"
  , "  webview_native_handle_kind_t arg2"
  , ")"
  , "{"
  , "  return (webview_get_native_handle)(arg1, arg2);"
  , "}"
  , "webview_error_t hs_bindgen_d7a80634981704d1 ("
  , "  webview_t arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return (webview_set_title)(arg1, arg2);"
  , "}"
  , "webview_error_t hs_bindgen_b53fe9b785381883 ("
  , "  webview_t arg1,"
  , "  signed int arg2,"
  , "  signed int arg3,"
  , "  webview_hint_t arg4"
  , ")"
  , "{"
  , "  return (webview_set_size)(arg1, arg2, arg3, arg4);"
  , "}"
  , "webview_error_t hs_bindgen_a0e5409509a601e2 ("
  , "  webview_t arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return (webview_navigate)(arg1, arg2);"
  , "}"
  , "webview_error_t hs_bindgen_08879dae7c6c89ec ("
  , "  webview_t arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return (webview_set_html)(arg1, arg2);"
  , "}"
  , "webview_error_t hs_bindgen_bff3d11bcab99ff3 ("
  , "  webview_t arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return (webview_init)(arg1, arg2);"
  , "}"
  , "webview_error_t hs_bindgen_10609e3d8c598556 ("
  , "  webview_t arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return (webview_eval)(arg1, arg2);"
  , "}"
  , "webview_error_t hs_bindgen_e0867e465092ecf4 ("
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
  , "webview_error_t hs_bindgen_f61127d6b8a15df3 ("
  , "  webview_t arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return (webview_unbind)(arg1, arg2);"
  , "}"
  , "webview_error_t hs_bindgen_bdea3bfb115985a2 ("
  , "  webview_t arg1,"
  , "  char const *arg2,"
  , "  signed int arg3,"
  , "  char const *arg4"
  , ")"
  , "{"
  , "  return (webview_return)(arg1, arg2, arg3, arg4);"
  , "}"
  , "webview_version_info_t const *hs_bindgen_21d642e62f73e8ee (void)"
  , "{"
  , "  return (webview_version)();"
  , "}"
  ]))

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_create@
foreign import ccall unsafe "hs_bindgen_a5a8a94fcfc7c716" hs_bindgen_a5a8a94fcfc7c716_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_create@
hs_bindgen_a5a8a94fcfc7c716 ::
     RIP.CInt
  -> RIP.Ptr RIP.Void
  -> IO Webview_t
hs_bindgen_a5a8a94fcfc7c716 =
  RIP.fromFFIType hs_bindgen_a5a8a94fcfc7c716_base

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
webview_create = hs_bindgen_a5a8a94fcfc7c716

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_destroy@
foreign import ccall unsafe "hs_bindgen_05c427a35c12d654" hs_bindgen_05c427a35c12d654_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_destroy@
hs_bindgen_05c427a35c12d654 ::
     Webview_t
  -> IO Webview_error_t
hs_bindgen_05c427a35c12d654 =
  RIP.fromFFIType hs_bindgen_05c427a35c12d654_base

{-| __C declaration:__ @webview_destroy@

    __defined at:__ @api.h 67:29@

    __exported by:__ @webview\/webview.h@
-}
webview_destroy ::
     Webview_t
     -- ^ __C declaration:__ @w@
  -> IO Webview_error_t
webview_destroy = hs_bindgen_05c427a35c12d654

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_run@
foreign import ccall unsafe "hs_bindgen_8608f6bc8f77a97e" hs_bindgen_8608f6bc8f77a97e_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_run@
hs_bindgen_8608f6bc8f77a97e ::
     Webview_t
  -> IO Webview_error_t
hs_bindgen_8608f6bc8f77a97e =
  RIP.fromFFIType hs_bindgen_8608f6bc8f77a97e_base

{-| __C declaration:__ @webview_run@

    __defined at:__ @api.h 74:29@

    __exported by:__ @webview\/webview.h@
-}
webview_run ::
     Webview_t
     -- ^ __C declaration:__ @w@
  -> IO Webview_error_t
webview_run = hs_bindgen_8608f6bc8f77a97e

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_terminate@
foreign import ccall unsafe "hs_bindgen_246ed776acecea2b" hs_bindgen_246ed776acecea2b_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_terminate@
hs_bindgen_246ed776acecea2b ::
     Webview_t
  -> IO Webview_error_t
hs_bindgen_246ed776acecea2b =
  RIP.fromFFIType hs_bindgen_246ed776acecea2b_base

{-| __C declaration:__ @webview_terminate@

    __defined at:__ @api.h 82:29@

    __exported by:__ @webview\/webview.h@
-}
webview_terminate ::
     Webview_t
     -- ^ __C declaration:__ @w@
  -> IO Webview_error_t
webview_terminate = hs_bindgen_246ed776acecea2b

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_dispatch@
foreign import ccall unsafe "hs_bindgen_1c97d298cfd9383c" hs_bindgen_1c97d298cfd9383c_base ::
     RIP.Ptr RIP.Void
  -> RIP.FunPtr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_dispatch@
hs_bindgen_1c97d298cfd9383c ::
     Webview_t
  -> RIP.FunPtr (Webview_t -> RIP.Ptr RIP.Void -> IO ())
  -> RIP.Ptr RIP.Void
  -> IO Webview_error_t
hs_bindgen_1c97d298cfd9383c =
  RIP.fromFFIType hs_bindgen_1c97d298cfd9383c_base

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
webview_dispatch = hs_bindgen_1c97d298cfd9383c

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_get_window@
foreign import ccall unsafe "hs_bindgen_5ef552e574675a11" hs_bindgen_5ef552e574675a11_base ::
     RIP.Ptr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_get_window@
hs_bindgen_5ef552e574675a11 ::
     Webview_t
  -> IO (RIP.Ptr RIP.Void)
hs_bindgen_5ef552e574675a11 =
  RIP.fromFFIType hs_bindgen_5ef552e574675a11_base

{-| __C declaration:__ @webview_get_window@

    __defined at:__ @api.h 107:19@

    __exported by:__ @webview\/webview.h@
-}
webview_get_window ::
     Webview_t
     -- ^ __C declaration:__ @w@
  -> IO (RIP.Ptr RIP.Void)
webview_get_window = hs_bindgen_5ef552e574675a11

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_get_native_handle@
foreign import ccall unsafe "hs_bindgen_a3acd72e74fffd6f" hs_bindgen_a3acd72e74fffd6f_base ::
     RIP.Ptr RIP.Void
  -> RIP.Word32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_get_native_handle@
hs_bindgen_a3acd72e74fffd6f ::
     Webview_t
  -> Webview_native_handle_kind_t
  -> IO (RIP.Ptr RIP.Void)
hs_bindgen_a3acd72e74fffd6f =
  RIP.fromFFIType hs_bindgen_a3acd72e74fffd6f_base

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
  hs_bindgen_a3acd72e74fffd6f

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_set_title@
foreign import ccall unsafe "hs_bindgen_d7a80634981704d1" hs_bindgen_d7a80634981704d1_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_set_title@
hs_bindgen_d7a80634981704d1 ::
     Webview_t
  -> PtrConst.PtrConst RIP.CChar
  -> IO Webview_error_t
hs_bindgen_d7a80634981704d1 =
  RIP.fromFFIType hs_bindgen_d7a80634981704d1_base

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
webview_set_title = hs_bindgen_d7a80634981704d1

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_set_size@
foreign import ccall unsafe "hs_bindgen_b53fe9b785381883" hs_bindgen_b53fe9b785381883_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int32
  -> RIP.Int32
  -> RIP.Word32
  -> IO RIP.Int32

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_set_size@
hs_bindgen_b53fe9b785381883 ::
     Webview_t
  -> RIP.CInt
  -> RIP.CInt
  -> Webview_hint_t
  -> IO Webview_error_t
hs_bindgen_b53fe9b785381883 =
  RIP.fromFFIType hs_bindgen_b53fe9b785381883_base

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
webview_set_size = hs_bindgen_b53fe9b785381883

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_navigate@
foreign import ccall unsafe "hs_bindgen_a0e5409509a601e2" hs_bindgen_a0e5409509a601e2_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_navigate@
hs_bindgen_a0e5409509a601e2 ::
     Webview_t
  -> PtrConst.PtrConst RIP.CChar
  -> IO Webview_error_t
hs_bindgen_a0e5409509a601e2 =
  RIP.fromFFIType hs_bindgen_a0e5409509a601e2_base

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
webview_navigate = hs_bindgen_a0e5409509a601e2

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_set_html@
foreign import ccall unsafe "hs_bindgen_08879dae7c6c89ec" hs_bindgen_08879dae7c6c89ec_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_set_html@
hs_bindgen_08879dae7c6c89ec ::
     Webview_t
  -> PtrConst.PtrConst RIP.CChar
  -> IO Webview_error_t
hs_bindgen_08879dae7c6c89ec =
  RIP.fromFFIType hs_bindgen_08879dae7c6c89ec_base

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
webview_set_html = hs_bindgen_08879dae7c6c89ec

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_init@
foreign import ccall unsafe "hs_bindgen_bff3d11bcab99ff3" hs_bindgen_bff3d11bcab99ff3_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_init@
hs_bindgen_bff3d11bcab99ff3 ::
     Webview_t
  -> PtrConst.PtrConst RIP.CChar
  -> IO Webview_error_t
hs_bindgen_bff3d11bcab99ff3 =
  RIP.fromFFIType hs_bindgen_bff3d11bcab99ff3_base

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
webview_init = hs_bindgen_bff3d11bcab99ff3

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_eval@
foreign import ccall unsafe "hs_bindgen_10609e3d8c598556" hs_bindgen_10609e3d8c598556_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_eval@
hs_bindgen_10609e3d8c598556 ::
     Webview_t
  -> PtrConst.PtrConst RIP.CChar
  -> IO Webview_error_t
hs_bindgen_10609e3d8c598556 =
  RIP.fromFFIType hs_bindgen_10609e3d8c598556_base

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
webview_eval = hs_bindgen_10609e3d8c598556

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_bind@
foreign import ccall unsafe "hs_bindgen_e0867e465092ecf4" hs_bindgen_e0867e465092ecf4_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> RIP.FunPtr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_bind@
hs_bindgen_e0867e465092ecf4 ::
     Webview_t
  -> PtrConst.PtrConst RIP.CChar
  -> RIP.FunPtr (PtrConst.PtrConst RIP.CChar -> PtrConst.PtrConst RIP.CChar -> RIP.Ptr RIP.Void -> IO ())
  -> RIP.Ptr RIP.Void
  -> IO Webview_error_t
hs_bindgen_e0867e465092ecf4 =
  RIP.fromFFIType hs_bindgen_e0867e465092ecf4_base

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
webview_bind = hs_bindgen_e0867e465092ecf4

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_unbind@
foreign import ccall unsafe "hs_bindgen_f61127d6b8a15df3" hs_bindgen_f61127d6b8a15df3_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_unbind@
hs_bindgen_f61127d6b8a15df3 ::
     Webview_t
  -> PtrConst.PtrConst RIP.CChar
  -> IO Webview_error_t
hs_bindgen_f61127d6b8a15df3 =
  RIP.fromFFIType hs_bindgen_f61127d6b8a15df3_base

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
webview_unbind = hs_bindgen_f61127d6b8a15df3

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_return@
foreign import ccall unsafe "hs_bindgen_bdea3bfb115985a2" hs_bindgen_bdea3bfb115985a2_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_return@
hs_bindgen_bdea3bfb115985a2 ::
     Webview_t
  -> PtrConst.PtrConst RIP.CChar
  -> RIP.CInt
  -> PtrConst.PtrConst RIP.CChar
  -> IO Webview_error_t
hs_bindgen_bdea3bfb115985a2 =
  RIP.fromFFIType hs_bindgen_bdea3bfb115985a2_base

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
webview_return = hs_bindgen_bdea3bfb115985a2

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_version@
foreign import ccall unsafe "hs_bindgen_21d642e62f73e8ee" hs_bindgen_21d642e62f73e8ee_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @io_github_micharied_hswebview_WebView.Raw_Unsafe_webview_version@
hs_bindgen_21d642e62f73e8ee :: IO (PtrConst.PtrConst Webview_version_info_t)
hs_bindgen_21d642e62f73e8ee =
  RIP.fromFFIType hs_bindgen_21d642e62f73e8ee_base

{-| __C declaration:__ @webview_version@

    __defined at:__ @api.h 245:43@

    __exported by:__ @webview\/webview.h@
-}
webview_version :: IO (PtrConst.PtrConst Webview_version_info_t)
webview_version = hs_bindgen_21d642e62f73e8ee
