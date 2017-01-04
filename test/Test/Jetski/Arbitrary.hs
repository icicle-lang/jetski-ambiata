{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Jetski.Arbitrary where

import qualified Data.List as List
import qualified Data.Text as T
import           Data.Word (Word)

import           Disorder.Corpus

import           Foreign.Ptr (IntPtr)

import           P

import           Test.QuickCheck


------------------------------------------------------------------------

newtype Name = Name { unName :: Text }
  deriving (Eq, Ord, Show)

data Value =
    Double  Name Double
  | Int32   Name Int32
  | VoidPtr Name IntPtr
  deriving (Eq, Ord, Show)

newtype Values = Values { getArguments :: [Value] }
  deriving (Eq, Ord, Show)

nameOfArgument :: Value -> Text
nameOfArgument = \case
  Double  n _ -> unName n
  Int32   n _ -> unName n
  VoidPtr n _ -> unName n


------------------------------------------------------------------------

instance Arbitrary Values where
  arbitrary =
    Values . List.nubBy ((==) `on` nameOfArgument) <$> arbitrary

instance Arbitrary Value where
  arbitrary = oneof [
      Double  <$> arbitrary <*> arbitrary
    , Int32   <$> arbitrary <*> arbitrary
    , VoidPtr <$> arbitrary <*> (fromInteger <$> arbitrary)
    ]

instance Arbitrary Name where
  arbitrary = do
    name        <- elements muppets
    (i :: Word) <- (`mod` 100) <$> arbitrary
    return (Name (name <> T.pack (show i)))
