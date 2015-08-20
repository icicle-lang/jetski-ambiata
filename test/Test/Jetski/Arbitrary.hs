{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Jetski.Arbitrary where

import           Data.Text (Text)
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


------------------------------------------------------------------------

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
