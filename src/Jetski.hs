{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Jetski
    ( -- * Types
      JetskiError(..)
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
    , compileIR

      -- * Accessing Functions
    , function

      -- * Exports from 'libffi'
    , module X
    , Arg
    , argDouble
    , retDouble
    ) where

import           Data.String (String)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Control.Exception (IOException)
import           Control.Monad.Catch (MonadCatch, MonadMask, handle)
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Jetski.OS (OS(..), currentOS)

import           Foreign.LibFFI (Arg, RetType, callFFI)
import           Foreign.LibFFI.Base (mkStorableArg, mkStorableRetType)
import           Foreign.LibFFI.FFITypes (ffi_type_double)
import           Foreign.LibFFI.Types as X

import           P

import           System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import           System.Exit (ExitCode(..))
import           System.IO (IO, FilePath)
import           System.IO.Temp (createTempDirectory)
import           System.Posix.DynamicLinker (DL(..), RTLDFlags(..), dlopen, dlclose, dlsym)
import           System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)

import           X.Control.Monad.Trans.Either (EitherT, pattern EitherT)
import           X.Control.Monad.Trans.Either (bracketEitherT', left)


------------------------------------------------------------------------
-- Types

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

    -- | The temporary directory which contains the library.
  , libDirectory :: FilePath

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

withLibrary
  :: (MonadIO m, MonadMask m)
  => [CompilerOption]
  -> Text
  -> (Library -> EitherT JetskiError m a)
  -> EitherT JetskiError m a

withLibrary options source action = bracketEitherT' acquire release action
  where
    acquire = compileLibrary options source
    release = releaseLibrary

compile :: (MonadIO m, MonadMask m) => FilePath -> [CompilerOption] -> Text -> EitherT JetskiError m ()
compile dir options source = do
  let srcPath = "jetski.c"
      source' = "#line 1 \"jetski.c\"\n" <> source
      gccArgs = [srcPath] <> fmap T.unpack options

  tryIO (T.writeFile (dir <> "/" <> srcPath) source')

  (code, _, stderr) <- readProcess dir "gcc" gccArgs

  case code of
    ExitSuccess   -> return ()
    ExitFailure _ -> left (CompilerError options source stderr)

compileLibrary :: (MonadIO m, MonadMask m) => [CompilerOption] -> Text -> EitherT JetskiError m Library
compileLibrary options source = do
  os  <- supportedOS

  let libName  = "jetski." <> libExtension os
      options' = gccShared os <> ["-o", libName] <> options

  dir <- createSystemTempDirectory "jetski-"
  compile dir options' source

  let libPath = dir <> "/" <> T.unpack libName
  lib <- tryIO (dlopen libPath [RTLD_NOW, RTLD_LOCAL])

  return (Library lib dir source)

compileAssembly :: (MonadIO m, MonadMask m) => [CompilerOption] -> Text -> EitherT JetskiError m AssemblyCode
compileAssembly options source = do
  let asmPath  = "jetski.s"
      options' = ["-S"] <> options

  withSystemTempDirectory "jetski-" $ \dir -> do
    compile dir options' source
    tryIO (T.readFile (dir <> "/" <> asmPath))

compileIR :: (MonadIO m, MonadMask m) => [CompilerOption] -> Text -> EitherT JetskiError m AssemblyCode
compileIR options source = do
  let irPath  = "jetski.ll"
      options' = ["-S", "-emit-llvm"] <> options

  withSystemTempDirectory "jetski-" $ \dir -> do
    compile dir options' source
    tryIO (T.readFile (dir <> "/" <> irPath))

releaseLibrary :: MonadIO m => Library -> m ()
releaseLibrary (Library lib dir _) =
  liftIO $ do
    hushIO (dlclose lib)
    hushIO (removeDirectoryRecursive dir)


------------------------------------------------------------------------
-- Accessing Functions

function :: (MonadIO m, MonadCatch m)
         => Library -> Symbol -> RetType a -> EitherT JetskiError m ([Arg] -> IO a)
function lib symbol retType = do
  fptr <- tryIO' (const (SymbolNotFound symbol))
                 (dlsym (libDL lib) (T.unpack symbol))
  return (callFFI fptr retType)


------------------------------------------------------------------------
-- Operating System Specifics

supportedOS :: Monad m => EitherT JetskiError m OS
supportedOS = maybe (left UnsupportedOS) return currentOS

libExtension :: OS -> Text
libExtension Linux  = "so"
libExtension Darwin = "dylib"

gccShared :: OS -> [CompilerOption]
gccShared Linux  = ["-shared", "-fPIC"]
gccShared Darwin = pure "-dynamiclib"


------------------------------------------------------------------------
-- Running Processes

readProcess :: MonadIO m => FilePath -> FilePath -> [String] -> m (ExitCode, Text, Text)
readProcess dir cmd args = do
  let cp = (proc cmd args) { cwd = Just dir }

  (code, stdout, stderr) <- liftIO (readCreateProcessWithExitCode cp "")

  let stdout' = T.pack stdout
      stderr' = T.pack stderr

  return (code, stdout', stderr')


------------------------------------------------------------------------
-- Temporary Files

withSystemTempDirectory
  :: (MonadMask m, MonadIO m)
  => FilePath
  -> (FilePath -> EitherT JetskiError m a)
  -> EitherT JetskiError m a

withSystemTempDirectory template =
  bracketEitherT' (createSystemTempDirectory template)
                  (tryIO . removeDirectoryRecursive)

createSystemTempDirectory :: MonadIO m => FilePath -> EitherT JetskiError m FilePath
createSystemTempDirectory template =
  tryIO (getTemporaryDirectory >>= \tmp -> createTempDirectory tmp template)


------------------------------------------------------------------------
-- Exception Handling

-- | Try running an IO action and if it throws an IOException, wrap it
--   with Disaster.
tryIO :: forall a m. MonadIO m => IO a -> EitherT JetskiError m a
tryIO = tryIO' Disaster

-- | Try running an IO action and if it throws an IOException, wrap it
--   in a JetskiError.
tryIO' :: forall a m. MonadIO m => (IOException -> JetskiError) -> IO a -> EitherT JetskiError m a
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
