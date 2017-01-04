{-# LANGUAGE NoImplicitPrelude #-}
module Jetski.Foreign.Function (
    call
  ) where

import           Foreign.Marshal (allocaBytes, withArray)
import           Foreign.Ptr (FunPtr)

import           Jetski.Foreign.Argument
import           Jetski.Foreign.Binding
import           Jetski.Foreign.Return

import           P

import qualified Prelude as Savage

import           System.IO (IO)


call :: FunPtr a -> Return b -> [Argument] -> IO b
call funPtr (Return typ withRet) args0 =
  allocaBytes ffi_cif_size $ \cif -> do
    args <- traverse allocArgument args0
    withArray (fmap argumentType args) $ \typesPtr -> do
      status <- ffi_prep_cif cif ffi_default_abi (fromIntegral $ length args) typ typesPtr

      unless (status == ffi_ok) $
        Savage.error "Jetski.Foreign.Function.call: ffi_prep_cif failed"

      withArray (fmap argumentValue args) $ \valuesPtr -> do
        ret <- withRet $ \ptr -> ffi_call cif funPtr ptr valuesPtr
        traverse_ argumentFree args
        return ret
