{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Jetski.OS
    ( OS(..)
    , currentOS
    ) where

import           P

------------------------------------------------------------------------

-- | Supported operating systems for Jetski.
data OS =
    Linux  -- ^ Linux
  | Darwin -- ^ OS/X
  deriving (Eq, Ord, Read, Show)

------------------------------------------------------------------------

-- | The operating system we are currently running on.
currentOS :: Maybe OS

#if darwin_HOST_OS
currentOS = Just Darwin
#elif linux_HOST_OS
currentOS = Just Linux
#else
-- We don't use BSD or Solaris, and Windows is not a thing.
currentOS = Nothing
#endif
