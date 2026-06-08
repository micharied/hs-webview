{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module WebView.Raw
    ( WebView.Raw.Webview_error_t(..)
    , pattern WebView.Raw.WEBVIEW_ERROR_MISSING_DEPENDENCY
    , pattern WebView.Raw.WEBVIEW_ERROR_CANCELED
    , pattern WebView.Raw.WEBVIEW_ERROR_INVALID_STATE
    , pattern WebView.Raw.WEBVIEW_ERROR_INVALID_ARGUMENT
    , pattern WebView.Raw.WEBVIEW_ERROR_UNSPECIFIED
    , pattern WebView.Raw.WEBVIEW_ERROR_OK
    , pattern WebView.Raw.WEBVIEW_ERROR_DUPLICATE
    , pattern WebView.Raw.WEBVIEW_ERROR_NOT_FOUND
    , WebView.Raw.Webview_version_t(..)
    , WebView.Raw.Webview_version_info_t(..)
    , WebView.Raw.Webview_t(..)
    , WebView.Raw.Webview_native_handle_kind_t(..)
    , pattern WebView.Raw.WEBVIEW_NATIVE_HANDLE_KIND_UI_WINDOW
    , pattern WebView.Raw.WEBVIEW_NATIVE_HANDLE_KIND_UI_WIDGET
    , pattern WebView.Raw.WEBVIEW_NATIVE_HANDLE_KIND_BROWSER_CONTROLLER
    , WebView.Raw.Webview_hint_t(..)
    , pattern WebView.Raw.WEBVIEW_HINT_NONE
    , pattern WebView.Raw.WEBVIEW_HINT_MIN
    , pattern WebView.Raw.WEBVIEW_HINT_MAX
    , pattern WebView.Raw.WEBVIEW_HINT_FIXED
    )
  where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @enum webview_error_t@

    __defined at:__ @errors.h 45:9@

    __exported by:__ @webview\/webview.h@
-}
newtype Webview_error_t = Webview_error_t
  { unwrapWebview_error_t :: RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize Webview_error_t where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Webview_error_t where

  readRaw =
    \ptr0 ->
          pure Webview_error_t
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw Webview_error_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Webview_error_t unwrapWebview_error_t2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapWebview_error_t2

deriving via Marshal.EquivStorable Webview_error_t instance RIP.Storable Webview_error_t

deriving via RIP.CInt instance RIP.Prim Webview_error_t

instance CEnum.CEnum Webview_error_t where

  type CEnumZ Webview_error_t = RIP.CInt

  toCEnum = Webview_error_t

  fromCEnum = RIP.getField @"unwrapWebview_error_t"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [ (-5, RIP.singleton "WEBVIEW_ERROR_MISSING_DEPENDENCY")
                                   , (-4, RIP.singleton "WEBVIEW_ERROR_CANCELED")
                                   , (-3, RIP.singleton "WEBVIEW_ERROR_INVALID_STATE")
                                   , (-2, RIP.singleton "WEBVIEW_ERROR_INVALID_ARGUMENT")
                                   , (-1, RIP.singleton "WEBVIEW_ERROR_UNSPECIFIED")
                                   , (0, RIP.singleton "WEBVIEW_ERROR_OK")
                                   , (1, RIP.singleton "WEBVIEW_ERROR_DUPLICATE")
                                   , (2, RIP.singleton "WEBVIEW_ERROR_NOT_FOUND")
                                   ]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "Webview_error_t"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "Webview_error_t"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum Webview_error_t where

  minDeclaredValue = WEBVIEW_ERROR_MISSING_DEPENDENCY

  maxDeclaredValue = WEBVIEW_ERROR_NOT_FOUND

instance Show Webview_error_t where

  showsPrec = CEnum.shows

instance Read Webview_error_t where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "unwrapWebview_error_t" (RIP.Ptr Webview_error_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapWebview_error_t")

instance HasCField.HasCField Webview_error_t "unwrapWebview_error_t" where

  type CFieldType Webview_error_t "unwrapWebview_error_t" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @WEBVIEW_ERROR_MISSING_DEPENDENCY@

    __defined at:__ @errors.h 47:3@

    __exported by:__ @webview\/webview.h@
-}
pattern WEBVIEW_ERROR_MISSING_DEPENDENCY :: Webview_error_t
pattern WEBVIEW_ERROR_MISSING_DEPENDENCY = Webview_error_t (-5)

{-| __C declaration:__ @WEBVIEW_ERROR_CANCELED@

    __defined at:__ @errors.h 49:3@

    __exported by:__ @webview\/webview.h@
-}
pattern WEBVIEW_ERROR_CANCELED :: Webview_error_t
pattern WEBVIEW_ERROR_CANCELED = Webview_error_t (-4)

{-| __C declaration:__ @WEBVIEW_ERROR_INVALID_STATE@

    __defined at:__ @errors.h 51:3@

    __exported by:__ @webview\/webview.h@
-}
pattern WEBVIEW_ERROR_INVALID_STATE :: Webview_error_t
pattern WEBVIEW_ERROR_INVALID_STATE = Webview_error_t (-3)

{-| __C declaration:__ @WEBVIEW_ERROR_INVALID_ARGUMENT@

    __defined at:__ @errors.h 53:3@

    __exported by:__ @webview\/webview.h@
-}
pattern WEBVIEW_ERROR_INVALID_ARGUMENT :: Webview_error_t
pattern WEBVIEW_ERROR_INVALID_ARGUMENT = Webview_error_t (-2)

{-| __C declaration:__ @WEBVIEW_ERROR_UNSPECIFIED@

    __defined at:__ @errors.h 55:3@

    __exported by:__ @webview\/webview.h@
-}
pattern WEBVIEW_ERROR_UNSPECIFIED :: Webview_error_t
pattern WEBVIEW_ERROR_UNSPECIFIED = Webview_error_t (-1)

{-| __C declaration:__ @WEBVIEW_ERROR_OK@

    __defined at:__ @errors.h 58:3@

    __exported by:__ @webview\/webview.h@
-}
pattern WEBVIEW_ERROR_OK :: Webview_error_t
pattern WEBVIEW_ERROR_OK = Webview_error_t 0

{-| __C declaration:__ @WEBVIEW_ERROR_DUPLICATE@

    __defined at:__ @errors.h 60:3@

    __exported by:__ @webview\/webview.h@
-}
pattern WEBVIEW_ERROR_DUPLICATE :: Webview_error_t
pattern WEBVIEW_ERROR_DUPLICATE = Webview_error_t 1

{-| __C declaration:__ @WEBVIEW_ERROR_NOT_FOUND@

    __defined at:__ @errors.h 62:3@

    __exported by:__ @webview\/webview.h@
-}
pattern WEBVIEW_ERROR_NOT_FOUND :: Webview_error_t
pattern WEBVIEW_ERROR_NOT_FOUND = Webview_error_t 2

{-| __C declaration:__ @struct webview_version_t@

    __defined at:__ @types.h 30:9@

    __exported by:__ @webview\/webview.h@
-}
data Webview_version_t = Webview_version_t
  { webview_version_t_major :: RIP.CUInt
    {- ^ __C declaration:__ @major@

         __defined at:__ @types.h 32:16@

         __exported by:__ @webview\/webview.h@
    -}
  , webview_version_t_minor :: RIP.CUInt
    {- ^ __C declaration:__ @minor@

         __defined at:__ @types.h 34:16@

         __exported by:__ @webview\/webview.h@
    -}
  , webview_version_t_patch :: RIP.CUInt
    {- ^ __C declaration:__ @patch@

         __defined at:__ @types.h 36:16@

         __exported by:__ @webview\/webview.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Webview_version_t where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Webview_version_t where

  readRaw =
    \ptr0 ->
          pure Webview_version_t
      <*> HasCField.readRaw (RIP.Proxy @"webview_version_t_major") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"webview_version_t_minor") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"webview_version_t_patch") ptr0

instance Marshal.WriteRaw Webview_version_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Webview_version_t
            webview_version_t_major2
            webview_version_t_minor3
            webview_version_t_patch4 ->
                 HasCField.writeRaw (RIP.Proxy @"webview_version_t_major") ptr0 webview_version_t_major2
              >> HasCField.writeRaw (RIP.Proxy @"webview_version_t_minor") ptr0 webview_version_t_minor3
              >> HasCField.writeRaw (RIP.Proxy @"webview_version_t_patch") ptr0 webview_version_t_patch4

deriving via Marshal.EquivStorable Webview_version_t instance RIP.Storable Webview_version_t

instance HasCField.HasCField Webview_version_t "webview_version_t_major" where

  type CFieldType Webview_version_t "webview_version_t_major" =
    RIP.CUInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "webview_version_t_major" (RIP.Ptr Webview_version_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"webview_version_t_major")

instance HasCField.HasCField Webview_version_t "webview_version_t_minor" where

  type CFieldType Webview_version_t "webview_version_t_minor" =
    RIP.CUInt

  offset# = \_ -> \_ -> 4

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "webview_version_t_minor" (RIP.Ptr Webview_version_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"webview_version_t_minor")

instance HasCField.HasCField Webview_version_t "webview_version_t_patch" where

  type CFieldType Webview_version_t "webview_version_t_patch" =
    RIP.CUInt

  offset# = \_ -> \_ -> 8

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "webview_version_t_patch" (RIP.Ptr Webview_version_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"webview_version_t_patch")

{-| __C declaration:__ @struct webview_version_info_t@

    __defined at:__ @types.h 40:9@

    __exported by:__ @webview\/webview.h@
-}
data Webview_version_info_t = Webview_version_info_t
  { webview_version_info_t_version :: Webview_version_t
    {- ^ __C declaration:__ @version@

         __defined at:__ @types.h 42:21@

         __exported by:__ @webview\/webview.h@
    -}
  , webview_version_info_t_version_number :: CA.ConstantArray 32 RIP.CChar
    {- ^ __C declaration:__ @version_number@

         __defined at:__ @types.h 44:8@

         __exported by:__ @webview\/webview.h@
    -}
  , webview_version_info_t_pre_release :: CA.ConstantArray 48 RIP.CChar
    {- ^ __C declaration:__ @pre_release@

         __defined at:__ @types.h 47:8@

         __exported by:__ @webview\/webview.h@
    -}
  , webview_version_info_t_build_metadata :: CA.ConstantArray 48 RIP.CChar
    {- ^ __C declaration:__ @build_metadata@

         __defined at:__ @types.h 49:8@

         __exported by:__ @webview\/webview.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Webview_version_info_t where

  staticSizeOf = \_ -> (140 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Webview_version_info_t where

  readRaw =
    \ptr0 ->
          pure Webview_version_info_t
      <*> HasCField.readRaw (RIP.Proxy @"webview_version_info_t_version") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"webview_version_info_t_version_number") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"webview_version_info_t_pre_release") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"webview_version_info_t_build_metadata") ptr0

instance Marshal.WriteRaw Webview_version_info_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Webview_version_info_t
            webview_version_info_t_version2
            webview_version_info_t_version_number3
            webview_version_info_t_pre_release4
            webview_version_info_t_build_metadata5 ->
                 HasCField.writeRaw (RIP.Proxy @"webview_version_info_t_version") ptr0 webview_version_info_t_version2
              >> HasCField.writeRaw (RIP.Proxy @"webview_version_info_t_version_number") ptr0 webview_version_info_t_version_number3
              >> HasCField.writeRaw (RIP.Proxy @"webview_version_info_t_pre_release") ptr0 webview_version_info_t_pre_release4
              >> HasCField.writeRaw (RIP.Proxy @"webview_version_info_t_build_metadata") ptr0 webview_version_info_t_build_metadata5

deriving via Marshal.EquivStorable Webview_version_info_t instance RIP.Storable Webview_version_info_t

instance HasCField.HasCField Webview_version_info_t "webview_version_info_t_version" where

  type CFieldType Webview_version_info_t "webview_version_info_t_version" =
    Webview_version_t

  offset# = \_ -> \_ -> 0

instance ( ty ~ Webview_version_t
         ) => RIP.HasField "webview_version_info_t_version" (RIP.Ptr Webview_version_info_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"webview_version_info_t_version")

instance HasCField.HasCField Webview_version_info_t "webview_version_info_t_version_number" where

  type CFieldType Webview_version_info_t "webview_version_info_t_version_number" =
    CA.ConstantArray 32 RIP.CChar

  offset# = \_ -> \_ -> 12

instance ( ty ~ CA.ConstantArray 32 RIP.CChar
         ) => RIP.HasField "webview_version_info_t_version_number" (RIP.Ptr Webview_version_info_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"webview_version_info_t_version_number")

instance HasCField.HasCField Webview_version_info_t "webview_version_info_t_pre_release" where

  type CFieldType Webview_version_info_t "webview_version_info_t_pre_release" =
    CA.ConstantArray 48 RIP.CChar

  offset# = \_ -> \_ -> 44

instance ( ty ~ CA.ConstantArray 48 RIP.CChar
         ) => RIP.HasField "webview_version_info_t_pre_release" (RIP.Ptr Webview_version_info_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"webview_version_info_t_pre_release")

instance HasCField.HasCField Webview_version_info_t "webview_version_info_t_build_metadata" where

  type CFieldType Webview_version_info_t "webview_version_info_t_build_metadata" =
    CA.ConstantArray 48 RIP.CChar

  offset# = \_ -> \_ -> 92

instance ( ty ~ CA.ConstantArray 48 RIP.CChar
         ) => RIP.HasField "webview_version_info_t_build_metadata" (RIP.Ptr Webview_version_info_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"webview_version_info_t_build_metadata")

{-| __C declaration:__ @webview_t@

    __defined at:__ @types.h 53:15@

    __exported by:__ @webview\/webview.h@
-}
newtype Webview_t = Webview_t
  { unwrapWebview_t :: RIP.Ptr RIP.Void
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ RIP.Ptr RIP.Void
         ) => RIP.HasField "unwrapWebview_t" (RIP.Ptr Webview_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapWebview_t")

instance HasCField.HasCField Webview_t "unwrapWebview_t" where

  type CFieldType Webview_t "unwrapWebview_t" =
    RIP.Ptr RIP.Void

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @enum webview_native_handle_kind_t@

    __defined at:__ @types.h 56:9@

    __exported by:__ @webview\/webview.h@
-}
newtype Webview_native_handle_kind_t = Webview_native_handle_kind_t
  { unwrapWebview_native_handle_kind_t :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize Webview_native_handle_kind_t where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Webview_native_handle_kind_t where

  readRaw =
    \ptr0 ->
          pure Webview_native_handle_kind_t
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw Webview_native_handle_kind_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Webview_native_handle_kind_t unwrapWebview_native_handle_kind_t2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapWebview_native_handle_kind_t2

deriving via Marshal.EquivStorable Webview_native_handle_kind_t instance RIP.Storable Webview_native_handle_kind_t

deriving via RIP.CUInt instance RIP.Prim Webview_native_handle_kind_t

instance CEnum.CEnum Webview_native_handle_kind_t where

  type CEnumZ Webview_native_handle_kind_t = RIP.CUInt

  toCEnum = Webview_native_handle_kind_t

  fromCEnum =
    RIP.getField @"unwrapWebview_native_handle_kind_t"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [ (0, RIP.singleton "WEBVIEW_NATIVE_HANDLE_KIND_UI_WINDOW")
                                   , (1, RIP.singleton "WEBVIEW_NATIVE_HANDLE_KIND_UI_WIDGET")
                                   , (2, RIP.singleton "WEBVIEW_NATIVE_HANDLE_KIND_BROWSER_CONTROLLER")
                                   ]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "Webview_native_handle_kind_t"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "Webview_native_handle_kind_t"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum Webview_native_handle_kind_t where

  minDeclaredValue =
    WEBVIEW_NATIVE_HANDLE_KIND_UI_WINDOW

  maxDeclaredValue =
    WEBVIEW_NATIVE_HANDLE_KIND_BROWSER_CONTROLLER

instance Show Webview_native_handle_kind_t where

  showsPrec = CEnum.shows

instance Read Webview_native_handle_kind_t where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "unwrapWebview_native_handle_kind_t" (RIP.Ptr Webview_native_handle_kind_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapWebview_native_handle_kind_t")

instance HasCField.HasCField Webview_native_handle_kind_t "unwrapWebview_native_handle_kind_t" where

  type CFieldType Webview_native_handle_kind_t "unwrapWebview_native_handle_kind_t" =
    RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @WEBVIEW_NATIVE_HANDLE_KIND_UI_WINDOW@

    __defined at:__ @types.h 59:3@

    __exported by:__ @webview\/webview.h@
-}
pattern WEBVIEW_NATIVE_HANDLE_KIND_UI_WINDOW :: Webview_native_handle_kind_t
pattern WEBVIEW_NATIVE_HANDLE_KIND_UI_WINDOW = Webview_native_handle_kind_t 0

{-| __C declaration:__ @WEBVIEW_NATIVE_HANDLE_KIND_UI_WIDGET@

    __defined at:__ @types.h 62:3@

    __exported by:__ @webview\/webview.h@
-}
pattern WEBVIEW_NATIVE_HANDLE_KIND_UI_WIDGET :: Webview_native_handle_kind_t
pattern WEBVIEW_NATIVE_HANDLE_KIND_UI_WIDGET = Webview_native_handle_kind_t 1

{-| __C declaration:__ @WEBVIEW_NATIVE_HANDLE_KIND_BROWSER_CONTROLLER@

    __defined at:__ @types.h 66:3@

    __exported by:__ @webview\/webview.h@
-}
pattern WEBVIEW_NATIVE_HANDLE_KIND_BROWSER_CONTROLLER :: Webview_native_handle_kind_t
pattern WEBVIEW_NATIVE_HANDLE_KIND_BROWSER_CONTROLLER = Webview_native_handle_kind_t 2

{-| __C declaration:__ @enum webview_hint_t@

    __defined at:__ @types.h 70:9@

    __exported by:__ @webview\/webview.h@
-}
newtype Webview_hint_t = Webview_hint_t
  { unwrapWebview_hint_t :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize Webview_hint_t where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Webview_hint_t where

  readRaw =
    \ptr0 ->
          pure Webview_hint_t
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw Webview_hint_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Webview_hint_t unwrapWebview_hint_t2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapWebview_hint_t2

deriving via Marshal.EquivStorable Webview_hint_t instance RIP.Storable Webview_hint_t

deriving via RIP.CUInt instance RIP.Prim Webview_hint_t

instance CEnum.CEnum Webview_hint_t where

  type CEnumZ Webview_hint_t = RIP.CUInt

  toCEnum = Webview_hint_t

  fromCEnum = RIP.getField @"unwrapWebview_hint_t"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [ (0, RIP.singleton "WEBVIEW_HINT_NONE")
                                   , (1, RIP.singleton "WEBVIEW_HINT_MIN")
                                   , (2, RIP.singleton "WEBVIEW_HINT_MAX")
                                   , (3, RIP.singleton "WEBVIEW_HINT_FIXED")
                                   ]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "Webview_hint_t"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "Webview_hint_t"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum Webview_hint_t where

  minDeclaredValue = WEBVIEW_HINT_NONE

  maxDeclaredValue = WEBVIEW_HINT_FIXED

instance Show Webview_hint_t where

  showsPrec = CEnum.shows

instance Read Webview_hint_t where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "unwrapWebview_hint_t" (RIP.Ptr Webview_hint_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapWebview_hint_t")

instance HasCField.HasCField Webview_hint_t "unwrapWebview_hint_t" where

  type CFieldType Webview_hint_t "unwrapWebview_hint_t" =
    RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @WEBVIEW_HINT_NONE@

    __defined at:__ @types.h 72:3@

    __exported by:__ @webview\/webview.h@
-}
pattern WEBVIEW_HINT_NONE :: Webview_hint_t
pattern WEBVIEW_HINT_NONE = Webview_hint_t 0

{-| __C declaration:__ @WEBVIEW_HINT_MIN@

    __defined at:__ @types.h 74:3@

    __exported by:__ @webview\/webview.h@
-}
pattern WEBVIEW_HINT_MIN :: Webview_hint_t
pattern WEBVIEW_HINT_MIN = Webview_hint_t 1

{-| __C declaration:__ @WEBVIEW_HINT_MAX@

    __defined at:__ @types.h 76:3@

    __exported by:__ @webview\/webview.h@
-}
pattern WEBVIEW_HINT_MAX :: Webview_hint_t
pattern WEBVIEW_HINT_MAX = Webview_hint_t 2

{-| __C declaration:__ @WEBVIEW_HINT_FIXED@

    __defined at:__ @types.h 78:3@

    __exported by:__ @webview\/webview.h@
-}
pattern WEBVIEW_HINT_FIXED :: Webview_hint_t
pattern WEBVIEW_HINT_FIXED = Webview_hint_t 3

-- __unique:__ @instance ToFunPtr (Webview_t -> RIP.Ptr RIP.Void -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_f8d150db4953efa9_base ::
     (RIP.Ptr RIP.Void -> RIP.Ptr RIP.Void -> IO ())
  -> IO (RIP.FunPtr (RIP.Ptr RIP.Void -> RIP.Ptr RIP.Void -> IO ()))

-- __unique:__ @instance ToFunPtr (Webview_t -> RIP.Ptr RIP.Void -> IO ())@
hs_bindgen_f8d150db4953efa9 ::
     (Webview_t -> RIP.Ptr RIP.Void -> IO ())
  -> IO (RIP.FunPtr (Webview_t -> RIP.Ptr RIP.Void -> IO ()))
hs_bindgen_f8d150db4953efa9 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_f8d150db4953efa9_base (RIP.toFFIType fun0))

-- __unique:__ @instance FromFunPtr (Webview_t -> RIP.Ptr RIP.Void -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_680ce9e006f0c0b2_base ::
     RIP.FunPtr (RIP.Ptr RIP.Void -> RIP.Ptr RIP.Void -> IO ())
  -> RIP.Ptr RIP.Void -> RIP.Ptr RIP.Void -> IO ()

-- __unique:__ @instance FromFunPtr (Webview_t -> RIP.Ptr RIP.Void -> IO ())@
hs_bindgen_680ce9e006f0c0b2 ::
     RIP.FunPtr (Webview_t -> RIP.Ptr RIP.Void -> IO ())
  -> Webview_t -> RIP.Ptr RIP.Void -> IO ()
hs_bindgen_680ce9e006f0c0b2 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_680ce9e006f0c0b2_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr (Webview_t -> RIP.Ptr RIP.Void -> IO ()) where

  toFunPtr = hs_bindgen_f8d150db4953efa9

instance RIP.FromFunPtr (Webview_t -> RIP.Ptr RIP.Void -> IO ()) where

  fromFunPtr = hs_bindgen_680ce9e006f0c0b2
