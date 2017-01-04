{-# LANGUAGE NoImplicitPrelude #-}
module Jetski.Foreign.Argument (
    CArgument(..)
  , Argument(..)
  , storableArgument
  , pointerArgument

  , argInt8
  , argInt16
  , argInt32
  , argInt64
  , argWord8
  , argWord16
  , argWord32
  , argWord64
  , argFloat
  , argDouble

  , argCChar
  , argCUChar
  , argCWchar
  , argCInt
  , argCUInt
  , argCLong
  , argCULong
  , argCSize
  , argCTime

  , argPtr
  , argFunPtr
  , argByteString
  , argByteStringCopy
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import           Data.Word (Word8, Word16, Word32, Word64)

import           Foreign.C.Types (CChar, CUChar, CWchar)
import           Foreign.C.Types (CInt, CUInt, CLong, CULong)
import           Foreign.C.Types (CSize, CTime)
import           Foreign.Marshal (malloc, free, new)
import           Foreign.Ptr (Ptr, FunPtr, castPtr)
import           Foreign.Storable (Storable(..))

import           Jetski.Foreign.Binding

import           P

import           System.IO (IO)


data CArgument =
  CArgument {
      argumentType :: !(Ptr CType)
    , argumentValue :: !(Ptr CValue)
    , argumentFree :: IO ()
    }

newtype Argument =
  Argument {
      allocArgument :: IO CArgument
    }

storableArgument :: Storable a => Ptr CType -> a -> Argument
storableArgument typ a =
  Argument $ do
    p <- malloc
    poke p a
    return $
      CArgument typ (castPtr p) (free p)

pointerArgument :: (a -> IO (Ptr b)) -> (Ptr b -> IO ()) -> a -> Argument
pointerArgument newA freeA a =
  Argument $ do
    p <- newA a
    pp <- new p
    return $
      CArgument ffi_type_pointer (castPtr pp) (free pp >> freeA p)

argInt8 :: Int8 -> Argument
argInt8 =
  storableArgument ffi_type_sint8

argInt16 :: Int16 -> Argument
argInt16 =
  storableArgument ffi_type_sint16

argInt32 :: Int32 -> Argument
argInt32 =
  storableArgument ffi_type_sint32

argInt64 :: Int64 -> Argument
argInt64 =
  storableArgument ffi_type_sint64

argWord8 :: Word8 -> Argument
argWord8 =
  storableArgument ffi_type_uint8

argWord16 :: Word16 -> Argument
argWord16 =
  storableArgument ffi_type_uint16

argWord32 :: Word32 -> Argument
argWord32 =
  storableArgument ffi_type_uint32

argWord64 :: Word64 -> Argument
argWord64 =
  storableArgument ffi_type_uint64

argFloat :: Float -> Argument
argFloat =
  storableArgument ffi_type_float

argDouble :: Double -> Argument
argDouble =
  storableArgument ffi_type_double

argCChar :: CChar -> Argument
argCChar =
  storableArgument ffi_type_schar

argCUChar :: CUChar -> Argument
argCUChar =
  storableArgument ffi_type_uchar

argCWchar :: CWchar -> Argument
argCWchar =
  storableArgument ffi_type_schar

argCInt :: CInt -> Argument
argCInt =
  storableArgument ffi_type_sint

argCUInt :: CUInt -> Argument
argCUInt =
  storableArgument ffi_type_uint

argCLong :: CLong -> Argument
argCLong =
  storableArgument ffi_type_slong

argCULong :: CULong -> Argument
argCULong =
  storableArgument ffi_type_ulong

argCSize :: CSize -> Argument
argCSize =
  storableArgument ffi_type_size

argCTime :: CTime -> Argument
argCTime =
  storableArgument ffi_type_time

argPtr :: Ptr a -> Argument
argPtr =
  storableArgument ffi_type_pointer

argFunPtr :: FunPtr a -> Argument
argFunPtr =
  storableArgument ffi_type_pointer

argByteString :: ByteString -> Argument
argByteString =
  pointerArgument (flip B.unsafeUseAsCString return) (const $ return ())

argByteStringCopy :: ByteString -> Argument
argByteStringCopy =
  pointerArgument (flip B.useAsCString return) (const $ return ())
