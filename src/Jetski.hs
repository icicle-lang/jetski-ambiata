{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Jetski (
  -- * Types
    JetskiError(..)
  , CacheLibrary(..)
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
import           Control.Monad.Catch (MonadMask, handle)
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Jetski.Hash
import           Jetski.OS

import           Foreign.LibFFI (Arg, RetType, callFFI)
import           Foreign.LibFFI.Base (mkStorableArg, mkStorableRetType)
import           Foreign.LibFFI.FFITypes (ffi_type_double)
import           Foreign.LibFFI.Types as X

import           P

import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import           System.Environment (lookupEnv)
import           System.Exit (ExitCode(..))
import           System.FilePath ((</>))
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
  | CompilerError ![CompilerOption] !SourceCode !CompilerError
  | SymbolNotFound !Symbol
  | Disaster !IOException
    deriving (Eq, Show)

data CacheLibrary =
    NoCacheLibrary
  | CacheLibrary
    deriving (Eq, Ord, Show)

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

data Library =
  Library {
    -- | A reference to the dynamic library itself.
      libDL :: DL

    -- | Whether the library should be cached or not. If not we need to delete
    --   it on release.
    , libCache :: CacheLibrary

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
    acquire = compileLibrary NoCacheLibrary options source
    release = releaseLibrary

compile :: MonadIO m => FilePath -> [CompilerOption] -> Text -> EitherT JetskiError m ()
compile dir options source = do
  let
    srcFile =
      "jetski.c"

    srcPath =
      dir </> srcFile

    shPath =
      dir </> "Makefile"

    gccArgs =
      [T.pack srcFile] <> options

  tryIO $ do
    T.writeFile srcPath source
    T.writeFile shPath $ T.unlines [
        ".PHONY: default"
      , ""
      , "default: jetski.c"
      , "\tgcc " <> renderArgs gccArgs
      ]

  (code, _, stderr) <- readProcess dir "gcc" $ fmap T.unpack gccArgs

  case code of
    ExitSuccess ->
      pure ()
    ExitFailure _ ->
      left $ CompilerError options source stderr

renderArgs :: [Text] -> Text
renderArgs =
  let
    render opt =
      if " " `T.isInfixOf` opt then
        "\"" <> opt <> "\""
      else
        opt
  in
    T.unwords . fmap render

compileLibrary :: MonadIO m => CacheLibrary -> [CompilerOption] -> SourceCode -> EitherT JetskiError m Library
compileLibrary cache options source = do
  os <- supportedOS

  let
    libName =
      "jetski." <> libExtension os

    libPath dir =
      dir <> "/" <> T.unpack libName

    options' =
      gccShared os <> ["-o", libName] <> options

  dir <-
    case cache of
      NoCacheLibrary -> do
        dir <- createSystemTempDirectory "jetski-"
        compile dir options' source
        pure dir

      CacheLibrary -> do
        home <- getJetskiHome

        let
          optHash =
            hashHashes $ fmap hashText options

          srcHash =
            hashText source

          hash =
            hashHashes [optHash, srcHash]

          dir =
            home </> T.unpack (renderHash hash)

        unlessM (liftIO . doesFileExist $ libPath dir) $ do
          liftIO $ createDirectoryIfMissing True dir
          compile dir options' source

        pure dir

  lib <- tryIO $ dlopen (libPath dir) [RTLD_NOW, RTLD_LOCAL]
  pure $ Library lib cache dir source

compileAssembly :: (MonadIO m, MonadMask m) => [CompilerOption] -> SourceCode -> EitherT JetskiError m AssemblyCode
compileAssembly options source = do
  let asmPath  = "jetski.s"
      options' = ["-S"] <> options

  withSystemTempDirectory "jetski-" $ \dir -> do
    compile dir options' source
    tryIO (T.readFile (dir <> "/" <> asmPath))

compileIR :: (MonadIO m, MonadMask m) => [CompilerOption] -> SourceCode -> EitherT JetskiError m AssemblyCode
compileIR options source = do
  let irPath  = "jetski.ll"
      options' = ["-S", "-emit-llvm"] <> options

  withSystemTempDirectory "jetski-" $ \dir -> do
    compile dir options' source
    tryIO (T.readFile (dir <> "/" <> irPath))

releaseLibrary :: MonadIO m => Library -> m ()
releaseLibrary (Library lib cache dir _) = do
  liftIO . hushIO $ dlclose lib
  case cache of
    NoCacheLibrary ->
      liftIO . hushIO $ removeDirectoryRecursive dir
    CacheLibrary ->
      pure ()

------------------------------------------------------------------------
-- Environment

getJetskiHome :: MonadIO m => m FilePath
getJetskiHome = do
  mjhome <- liftIO $ lookupEnv "JETSKI_HOME"
  case mjhome of
    Just jhome ->
      pure jhome
    Nothing -> do
      mhome <- liftIO $ lookupEnv "HOME"
      case mhome of
        Nothing ->
          (</> "jetski") <$> liftIO getTemporaryDirectory
        Just home ->
          pure $ home </> ".jetski"

------------------------------------------------------------------------
-- Accessing Functions

function :: MonadIO m => Library -> Symbol -> RetType a -> EitherT JetskiError m ([Arg] -> IO a)
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
