{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Jetski
    ( -- * Types
      JetskiT
    , JetskiError(..)
    , SourceCode
    , CompilerOption
    , CompilerError
    , Symbol
    , Library(..)

      -- * Compiling and Loading C Source
    , withLibrary
    , compileLibrary
    , releaseLibrary
    , compileAssembly

      -- * Accessing Functions
    , function

      -- * Exports from 'libffi'
    , module X
    , Arg
    , argDouble
    , retDouble
    ) where

import           Data.String (String)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Control.Exception (IOException)
import           Control.Monad.Catch (MonadCatch, MonadMask, handle)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Either (EitherT(..), left)

import           Jetski.OS (OS(..), currentOS)

import           Foreign.LibFFI (Arg, RetType, callFFI)
import           Foreign.LibFFI.Base (mkStorableArg, mkStorableRetType)
import           Foreign.LibFFI.FFITypes (ffi_type_double)
import           Foreign.LibFFI.Types as X

import           P

import           System.Directory (getCurrentDirectory, setCurrentDirectory)
import           System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import           System.Exit (ExitCode(..))
import           System.IO (IO, FilePath)
import           System.IO.Temp (createTempDirectory)
import           System.Posix.DynamicLinker (DL(..), RTLDFlags(..), dlopen, dlclose, dlsym)
import           System.Process (readProcessWithExitCode)

import           X.Control.Monad.Catch (bracketEitherT')


------------------------------------------------------------------------
-- Types

type JetskiT = EitherT JetskiError

data JetskiError =
    UnsupportedOS
  | CompilerError  [CompilerOption] SourceCode CompilerError
  | SymbolNotFound Symbol
  | Disaster       IOException
  deriving (Eq, Show)

type SourceCode   = Text
type AssemblyCode = Text

-- | Additional options to be passed to the C compiler.
--
--   Some examples:
-- @
--   -O2
--   -O3
--   -Ofast
--   -march=native
--   -funroll-loops
-- @
--
type CompilerOption = Text

-- | The @stderr@ output from the C compiler.
type CompilerError  = Text

-- | The name of a function in the compiled library.
type Symbol         = Text

data Library = Library {
    -- | A reference to the dynamic library itself.
    libDL :: DL

    -- | The source code of the compiled library.
  , libSource :: SourceCode
  }


------------------------------------------------------------------------
-- Missing 'libffi' Arguments

argDouble :: Double -> Arg
argDouble = mkStorableArg ffi_type_double

retDouble :: RetType Double
retDouble = mkStorableRetType ffi_type_double


------------------------------------------------------------------------
-- Compiling and Loading C Source

withLibrary :: (MonadIO m, MonadMask m) => [CompilerOption] -> Text -> (Library -> JetskiT m a) -> JetskiT m a
withLibrary options source action = bracketEitherT' acquire release action
  where
    acquire = compileLibrary options source
    release = releaseLibrary

compile :: (MonadIO m, MonadMask m) => [CompilerOption] -> Text -> JetskiT m a -> JetskiT m a
compile options source action =
  bracketEitherT' (liftIO getCurrentDirectory) (liftIO . setCurrentDirectory) $ \_ -> do
  withSystemTempDirectory "jetski-" $ \dir -> do
    liftIO (setCurrentDirectory dir)

    let srcPath = "jetski.cpp"
        source' = "#line 1 \"jetski.cpp\"\n" <> source
        gccArgs = [srcPath] <> fmap T.unpack options

    tryIO (T.writeFile srcPath source')

    (code, _, stderr) <- readProcess "g++" gccArgs

    case code of
      ExitSuccess   -> return ()
      ExitFailure _ -> left (CompilerError options source stderr)

    action

compileLibrary :: (MonadIO m, MonadMask m) => [CompilerOption] -> Text -> JetskiT m Library
compileLibrary options source = do
  os  <- supportedOS

  let libName  = "jetski." <> libExtension os
      options' = [gccShared os, "-o", libName] <> options

  compile options' source $ do
    dir <- liftIO getCurrentDirectory
    let libPath = dir <> "/" <> T.unpack libName
    lib <- tryIO (dlopen libPath [RTLD_NOW, RTLD_LOCAL])
    return (Library lib source)

compileAssembly :: (MonadIO m, MonadMask m) => [CompilerOption] -> Text -> JetskiT m AssemblyCode
compileAssembly options source = do
  let asmPath  = "jetski.s"
      options' = ["-S"] <> options

  compile options' source $
    tryIO (T.readFile asmPath)

releaseLibrary :: MonadIO m => Library -> m ()
releaseLibrary = liftIO . hushIO . dlclose . libDL


------------------------------------------------------------------------
-- Accessing Functions

function :: (MonadIO m, MonadCatch m)
         => Library -> Symbol -> RetType a -> JetskiT m ([Arg] -> IO a)
function lib symbol retType = do
    fptr <- tryIO' (const (SymbolNotFound symbol))
                   (dlsym (libDL lib) (T.unpack symbol))
    return (callFFI fptr retType)


------------------------------------------------------------------------
-- Operating System Specifics

supportedOS :: Monad m => JetskiT m OS
supportedOS = maybe (left UnsupportedOS) return currentOS

libExtension :: OS -> Text
libExtension Linux  = "so"
libExtension Darwin = "dylib"

gccShared :: OS -> CompilerOption
gccShared Linux  = "-shared"
gccShared Darwin = "-dynamiclib"


------------------------------------------------------------------------
-- Running Processes

readProcess :: MonadIO m => FilePath -> [String] -> m (ExitCode, Text, Text)
readProcess cmd args = do
    (code, stdout, stderr) <- liftIO (readProcessWithExitCode cmd args "")

    let stdout' = T.pack stdout
        stderr' = T.pack stderr

    return (code, stdout', stderr')


------------------------------------------------------------------------
-- Temporary Files

withSystemTempDirectory :: (MonadMask m, MonadIO m) => FilePath -> (FilePath -> JetskiT m a) -> JetskiT m a
withSystemTempDirectory template action =
    bracketEitherT' acquire release action
  where
    acquire = tryIO $ getTemporaryDirectory >>= \tmp -> createTempDirectory tmp template
    release = tryIO . removeDirectoryRecursive


------------------------------------------------------------------------
-- Exception Handling

-- | Try running an IO action and if it throws an IOException, wrap it
--   with Disaster.
tryIO :: forall a m. MonadIO m => IO a -> JetskiT m a
tryIO = tryIO' Disaster

-- | Try running an IO action and if it throws an IOException, wrap it
--   in a JetskiError.
tryIO' :: forall a m. MonadIO m => (IOException -> JetskiError) -> IO a -> JetskiT m a
tryIO' takeError io = EitherT (liftIO (handle onError io'))
  where
    io' :: IO (Either JetskiError a)
    io' = io >>= return . Right

    onError :: IOException -> IO (Either JetskiError a)
    onError = return . Left . takeError

-- | Run an IO action and ignore any IOExceptions it throws.
hushIO :: IO a -> IO ()
hushIO io = handle onError (io >> return ())
  where
    onError :: IOException -> IO ()
    onError _ = return ()
