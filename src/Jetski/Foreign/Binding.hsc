{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
module Jetski.Foreign.Binding where

#include <ffi.h>

import           Data.Word (Word32)

import           Foreign.C.Types (CInt, CUInt(..), CLong, CULong)
import           Foreign.C.Types (CWchar, CSize, CTime)
import           Foreign.Storable (Storable(..))
import           Foreign.Ptr (Ptr, FunPtr)

import           P

import qualified Prelude as Savage

import           System.IO (IO)


data CValue
data CType
data CIF

type C_ffi_status =
  (#type ffi_status)

type C_ffi_abi =
  (#type ffi_abi)

ffi_default_abi :: C_ffi_abi
ffi_default_abi =
  #const FFI_DEFAULT_ABI

ffi_ok :: C_ffi_status
ffi_ok =
  #const FFI_OK

ffi_cif_size :: Int
ffi_cif_size =
  #size ffi_cif

foreign import ccall safe "ffi_prep_cif"
  ffi_prep_cif :: Ptr CIF -> C_ffi_abi -> CUInt -> Ptr CType -> Ptr (Ptr CType) -> IO C_ffi_status

foreign import ccall safe "ffi_call"
  ffi_call :: Ptr CIF -> FunPtr a -> Ptr CValue -> Ptr (Ptr CValue) -> IO ()

foreign import ccall unsafe "&ffi_type_void"
  ffi_type_void :: Ptr CType

foreign import ccall unsafe "&ffi_type_sint8"
  ffi_type_sint8 :: Ptr CType

foreign import ccall unsafe "&ffi_type_uint8"
  ffi_type_uint8 :: Ptr CType

foreign import ccall unsafe "&ffi_type_uint16"
  ffi_type_uint16 :: Ptr CType

foreign import ccall unsafe "&ffi_type_sint16"
  ffi_type_sint16 :: Ptr CType

foreign import ccall unsafe "&ffi_type_uint32"
  ffi_type_uint32 :: Ptr CType

foreign import ccall unsafe "&ffi_type_sint32"
  ffi_type_sint32 :: Ptr CType

foreign import ccall unsafe "&ffi_type_uint64"
  ffi_type_uint64 :: Ptr CType

foreign import ccall unsafe "&ffi_type_sint64"
  ffi_type_sint64 :: Ptr CType

foreign import ccall unsafe "&ffi_type_float"
  ffi_type_float  :: Ptr CType

foreign import ccall unsafe "&ffi_type_double"
  ffi_type_double :: Ptr CType

foreign import ccall unsafe "&ffi_type_pointer"
  ffi_type_pointer :: Ptr CType

ffi_type_uchar :: Ptr CType
ffi_type_uchar =
  ffi_type_uint8

ffi_type_schar :: Ptr CType
ffi_type_schar =
  ffi_type_sint8

ffi_type_wchar :: Ptr CType
ffi_type_wchar =
  case sizeOf (Savage.undefined :: CWchar) of
    2 ->
      ffi_type_sint16
    4 ->
      ffi_type_sint32
    8 ->
      ffi_type_sint64
    _ ->
      Savage.error "Jetski.Foreign.Binding.ffi_type_wchar: unsupported size"

ffi_type_uint :: Ptr CType
ffi_type_uint =
  case sizeOf (Savage.undefined :: CUInt) of
    4 ->
      ffi_type_uint32
    8 ->
      ffi_type_uint64
    _ ->
      Savage.error "Jetski.Foreign.Binding.ffi_type_uint: unsupported size"

ffi_type_sint :: Ptr CType
ffi_type_sint =
  case sizeOf (Savage.undefined :: CInt) of
    4 ->
      ffi_type_sint32
    8 ->
      ffi_type_sint64
    _ ->
      Savage.error "Jetski.Foreign.Binding.ffi_type_sint: unsupported size"

ffi_type_ulong :: Ptr CType
ffi_type_ulong =
  case sizeOf (Savage.undefined :: CULong) of
    4 ->
      ffi_type_uint32
    8 ->
      ffi_type_uint64
    _ ->
      Savage.error "Jetski.Foreign.Binding.ffi_type_ulong: unsupported size"

ffi_type_slong :: Ptr CType
ffi_type_slong =
  case sizeOf (Savage.undefined :: CLong) of
    4 ->
      ffi_type_sint32
    8 ->
      ffi_type_sint64
    _ ->
      Savage.error "Jetski.Foreign.Binding.ffi_type_slong: unsupported size"

ffi_type_size :: Ptr CType
ffi_type_size =
  case sizeOf (Savage.undefined :: CSize) of
    4 ->
      ffi_type_uint32
    8 ->
      ffi_type_uint64
    _ ->
      Savage.error "Jetski.Foreign.Binding.ffi_type_size: unsupported size"

ffi_type_time :: Ptr CType
ffi_type_time =
  case sizeOf (Savage.undefined :: CTime) of
    4 ->
      ffi_type_sint32
    8 ->
      ffi_type_sint64
    _ ->
      Savage.error "Jetski.Foreign.Binding.ffi_type_time: unsupported size"
