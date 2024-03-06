{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hw03 (
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

bindir     = "/home/zevelyn/cmsc-433/homework-3/.stack-work/install/x86_64-linux/457159febb30ac70b1b19636c935528b368a83cb3bd9852d21b76bdff79f0ffe/8.10.4/bin"
libdir     = "/home/zevelyn/cmsc-433/homework-3/.stack-work/install/x86_64-linux/457159febb30ac70b1b19636c935528b368a83cb3bd9852d21b76bdff79f0ffe/8.10.4/lib/x86_64-linux-ghc-8.10.4/hw03-0.1.0.0-CxcT0k6T7i53XMKhSsfQP1-hw01"
dynlibdir  = "/home/zevelyn/cmsc-433/homework-3/.stack-work/install/x86_64-linux/457159febb30ac70b1b19636c935528b368a83cb3bd9852d21b76bdff79f0ffe/8.10.4/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/home/zevelyn/cmsc-433/homework-3/.stack-work/install/x86_64-linux/457159febb30ac70b1b19636c935528b368a83cb3bd9852d21b76bdff79f0ffe/8.10.4/share/x86_64-linux-ghc-8.10.4/hw03-0.1.0.0"
libexecdir = "/home/zevelyn/cmsc-433/homework-3/.stack-work/install/x86_64-linux/457159febb30ac70b1b19636c935528b368a83cb3bd9852d21b76bdff79f0ffe/8.10.4/libexec/x86_64-linux-ghc-8.10.4/hw03-0.1.0.0"
sysconfdir = "/home/zevelyn/cmsc-433/homework-3/.stack-work/install/x86_64-linux/457159febb30ac70b1b19636c935528b368a83cb3bd9852d21b76bdff79f0ffe/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hw03_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hw03_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hw03_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hw03_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw03_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw03_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
