{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_var_config (
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

bindir     = "/home/pixlark/prog/Haskell/var-config/.stack-work/install/x86_64-linux/lts-13.11/8.6.4/bin"
libdir     = "/home/pixlark/prog/Haskell/var-config/.stack-work/install/x86_64-linux/lts-13.11/8.6.4/lib/x86_64-linux-ghc-8.6.4/var-config-0.1.0.0-JYc4cgf5ywS7u62M69R08t-var-config"
dynlibdir  = "/home/pixlark/prog/Haskell/var-config/.stack-work/install/x86_64-linux/lts-13.11/8.6.4/lib/x86_64-linux-ghc-8.6.4"
datadir    = "/home/pixlark/prog/Haskell/var-config/.stack-work/install/x86_64-linux/lts-13.11/8.6.4/share/x86_64-linux-ghc-8.6.4/var-config-0.1.0.0"
libexecdir = "/home/pixlark/prog/Haskell/var-config/.stack-work/install/x86_64-linux/lts-13.11/8.6.4/libexec/x86_64-linux-ghc-8.6.4/var-config-0.1.0.0"
sysconfdir = "/home/pixlark/prog/Haskell/var-config/.stack-work/install/x86_64-linux/lts-13.11/8.6.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "var_config_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "var_config_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "var_config_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "var_config_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "var_config_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "var_config_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
