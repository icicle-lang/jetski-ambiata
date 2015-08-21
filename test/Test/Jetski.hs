{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Jetski where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either (EitherT(..))

import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as T

import           Disorder.Core.IO

import           Foreign.Ptr (intPtrToPtr)

import           Jetski

import           P

import           System.IO (IO)

import           Test.Jetski.Arbitrary
import           Test.QuickCheck

------------------------------------------------------------------------

prop_arguments name args = testEitherT $ do
    withLibrary opts (source name args') $ \library -> do
      f <- function library (unName name) retInt
      _ <- liftIO (f (fmap ffiArg args'))
      return (True === True)
  where
    args' = List.nubBy ((==) `on` var) args
    opts  = ["-Ofast", "-march=native"]


------------------------------------------------------------------------

source :: Name -> [Argument] -> Text
source name args = T.unlines [
      "#include <stdint.h>"
    , ""
    , "int " <> unName name <> "(" <> params <> ") {"
    , "    return " <> expr  <> ";"
    , "}"
    ]
  where
    params  = T.intercalate ", " (fmap param args)
    param x = ctype x <> " " <> var x

    expr   = T.intercalate " + "
           . fmap ("(int)" <>)
           . ("42":)
           $ fmap var args

var :: Argument -> Text
var = \case
  Double  n _ -> unName n
  Int32   n _ -> unName n
  VoidPtr n _ -> unName n

ctype :: Argument -> Text
ctype = \case
  Double  _ _ -> "double"
  Int32   _ _ -> "int32_t"
  VoidPtr _ _ -> "void*"

ffiArg :: Argument -> Arg
ffiArg (Double  _ x) = argDouble x
ffiArg (Int32   _ x) = argInt32  x
ffiArg (VoidPtr _ x) = argPtr (intPtrToPtr x)


------------------------------------------------------------------------

testEitherT :: EitherT JetskiError IO Property -> Property
testEitherT action = testIO $ do
    e <- runEitherT action
    case e of
      Left  l -> failProp l
      Right r -> return r
  where
    failProp x =
      let msg = T.unpack ("testEitherT: " <> errorRender x)
      in  return (counterexample msg (False === True))

errorRender :: JetskiError -> Text
errorRender = \case
    CompilerError opts src err
        -> "when compiling with: " <> T.unwords opts <> "\n\n"
        <> indent src
        <> "\nencountered the following error:\n\n"
        <> indent err
    err -> T.pack (show err)
  where
    indent = T.unlines . fmap ("    " <>) . T.lines


------------------------------------------------------------------------

return []
tests = $quickCheckAll
