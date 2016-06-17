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

data Argument =
    Double  Name Double
  | Int32   Name Int32
  | VoidPtr Name IntPtr
  deriving (Eq, Ord, Show)

newtype Arguments = Arguments { getArguments :: [Argument] }
  deriving (Eq, Ord, Show)

nameOfArgument :: Argument -> Text
nameOfArgument = \case
  Double  n _ -> unName n
  Int32   n _ -> unName n
  VoidPtr n _ -> unName n


------------------------------------------------------------------------

instance Arbitrary Arguments where
  arbitrary =
    Arguments . List.nubBy ((==) `on` nameOfArgument) <$> arbitrary

instance Arbitrary Argument where
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
