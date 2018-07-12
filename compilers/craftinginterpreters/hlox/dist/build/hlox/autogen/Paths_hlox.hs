{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_hlox (
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

bindir     = "/home/agustin/.cabal/bin"
libdir     = "/home/agustin/.cabal/lib/x86_64-linux-ghc-8.2.2/hlox-0.1.0.0-6yHmAdpaDRVDKwSIMAgWom-hlox"
dynlibdir  = "/home/agustin/.cabal/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/agustin/.cabal/share/x86_64-linux-ghc-8.2.2/hlox-0.1.0.0"
libexecdir = "/home/agustin/.cabal/libexec/x86_64-linux-ghc-8.2.2/hlox-0.1.0.0"
sysconfdir = "/home/agustin/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hlox_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hlox_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hlox_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hlox_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hlox_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hlox_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
