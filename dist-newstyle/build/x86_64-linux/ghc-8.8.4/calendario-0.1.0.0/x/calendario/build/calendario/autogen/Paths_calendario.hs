{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_calendario (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/agustina/.cabal/bin"
libdir     = "/home/agustina/.cabal/lib/x86_64-linux-ghc-8.8.4/calendario-0.1.0.0-inplace-calendario"
dynlibdir  = "/home/agustina/.cabal/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/agustina/.cabal/share/x86_64-linux-ghc-8.8.4/calendario-0.1.0.0"
libexecdir = "/home/agustina/.cabal/libexec/x86_64-linux-ghc-8.8.4/calendario-0.1.0.0"
sysconfdir = "/home/agustina/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "calendario_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "calendario_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "calendario_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "calendario_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "calendario_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "calendario_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
