{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_ip2location (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
version = Version [8,0,3] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/root/.cabal/bin"
libdir     = "/root/.cabal/lib/x86_64-linux-ghc-8.0.1/ip2location-8.0.3-2Ud2j86PgFl8KCW91BcER5"
datadir    = "/root/.cabal/share/x86_64-linux-ghc-8.0.1/ip2location-8.0.3"
libexecdir = "/root/.cabal/libexec"
sysconfdir = "/root/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ip2location_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ip2location_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ip2location_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ip2location_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ip2location_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
