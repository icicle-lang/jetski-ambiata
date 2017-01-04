{-# LANGUAGE NoImplicitPrelude #-}
module Jetski.Foreign.Return (
    Return(..)
  , storableReturn
  , withReturn

  , retVoid
  , retInt8
  , retInt16
  , retInt32
  , retInt64
  , retWord8
  , retWord16
  , retWord32
  , retWord64
  , retFloat
  , retDouble

  , retCChar
  , retCUChar
  , retCWchar
  , retCInt
  , retCUInt
  , retCLong
  , retCULong
  , retCSize
  , retCTime

  , retPtr
  , retFunPtr
  , retByteString
  , retByteStringCopy
  ) where

import           Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

import           Foreign.Marshal (alloca)
import           Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import           Foreign.Storable (Storable(..))
import           Foreign.C.Types (CChar, CUChar, CWchar)
import           Foreign.C.Types (CInt, CUInt, CLong, CULong)
import           Foreign.C.Types (CSize, CTime)

import           Jetski.Foreign.Binding

import           P

import           System.IO (IO)


data Return a =
  Return {
      returnType :: !(Ptr CType)
    , returnWith :: (Ptr CValue -> IO ()) -> IO a
    }

instance Functor Return where
  fmap f (Return typ withPoke) =
    Return typ (fmap f . withPoke)

withReturn :: (a -> IO b) -> Return a -> Return b
withReturn f (Return typ withPoke) =
  Return typ (withPoke >=> f)

storableReturn :: Storable a => Ptr CType -> Return a
storableReturn typ =
  Return typ $ \write ->
  alloca $ \ptr -> do
    write (castPtr ptr)
    peek ptr

retVoid :: Return ()
retVoid =
  Return ffi_type_void $ \write -> do
    write nullPtr
    return ()

retInt8 :: Return Int8
retInt8 =
  storableReturn ffi_type_sint8

retInt16 :: Return Int16
retInt16 =
  storableReturn ffi_type_sint16

retInt32 :: Return Int32
retInt32 =
  storableReturn ffi_type_sint32

retInt64 :: Return Int64
retInt64 =
  storableReturn ffi_type_sint64

retWord8 :: Return Word8
retWord8 =
  storableReturn ffi_type_uint8

retWord16 :: Return Word16
retWord16 =
  storableReturn ffi_type_uint16

retWord32 :: Return Word32
retWord32 =
  storableReturn ffi_type_uint32

retWord64 :: Return Word64
retWord64 =
  storableReturn ffi_type_uint64

retFloat :: Return Float
retFloat =
  storableReturn ffi_type_float

retDouble :: Return Double
retDouble =
  storableReturn ffi_type_double

retCChar :: Return CChar
retCChar =
  storableReturn ffi_type_schar

retCUChar :: Return CUChar
retCUChar =
  storableReturn ffi_type_uchar

retCWchar :: Return CWchar
retCWchar =
  storableReturn ffi_type_schar

retCInt :: Return CInt
retCInt =
  storableReturn ffi_type_sint

retCUInt :: Return CUInt
retCUInt =
  storableReturn ffi_type_uint

retCLong :: Return CLong
retCLong =
  storableReturn ffi_type_slong

retCULong :: Return CULong
retCULong =
  storableReturn ffi_type_ulong

retCSize :: Return CSize
retCSize =
  storableReturn ffi_type_size

retCTime :: Return CTime
retCTime =
  storableReturn ffi_type_time

retFunPtr :: Return a -> Return (FunPtr a)
retFunPtr _ =
  storableReturn ffi_type_pointer

retPtr :: Return a -> Return (Ptr a)
retPtr _ =
  storableReturn ffi_type_pointer

retByteString :: Return B.ByteString
retByteString =
  withReturn B.packCString (retPtr retCChar)

retByteStringCopy :: Return B.ByteString
retByteStringCopy =
  withReturn B.unsafePackMallocCString (retPtr retCChar)
