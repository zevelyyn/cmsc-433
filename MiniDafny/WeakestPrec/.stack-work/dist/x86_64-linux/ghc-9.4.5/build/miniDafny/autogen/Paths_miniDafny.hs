{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_miniDafny (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/zevelyn/cmsc-433/MiniDafny/WeakestPrec/.stack-work/install/x86_64-linux/e1f5cc9fc80b9356b230c4abacb834c995dc5332944e735a4172ed3015e6a90c/9.4.5/bin"
libdir     = "/home/zevelyn/cmsc-433/MiniDafny/WeakestPrec/.stack-work/install/x86_64-linux/e1f5cc9fc80b9356b230c4abacb834c995dc5332944e735a4172ed3015e6a90c/9.4.5/lib/x86_64-linux-ghc-9.4.5/miniDafny-0.1.0.0-44U4u9PMdzoCL2BC7fE6Rv-miniDafny"
dynlibdir  = "/home/zevelyn/cmsc-433/MiniDafny/WeakestPrec/.stack-work/install/x86_64-linux/e1f5cc9fc80b9356b230c4abacb834c995dc5332944e735a4172ed3015e6a90c/9.4.5/lib/x86_64-linux-ghc-9.4.5"
datadir    = "/home/zevelyn/cmsc-433/MiniDafny/WeakestPrec/.stack-work/install/x86_64-linux/e1f5cc9fc80b9356b230c4abacb834c995dc5332944e735a4172ed3015e6a90c/9.4.5/share/x86_64-linux-ghc-9.4.5/miniDafny-0.1.0.0"
libexecdir = "/home/zevelyn/cmsc-433/MiniDafny/WeakestPrec/.stack-work/install/x86_64-linux/e1f5cc9fc80b9356b230c4abacb834c995dc5332944e735a4172ed3015e6a90c/9.4.5/libexec/x86_64-linux-ghc-9.4.5/miniDafny-0.1.0.0"
sysconfdir = "/home/zevelyn/cmsc-433/MiniDafny/WeakestPrec/.stack-work/install/x86_64-linux/e1f5cc9fc80b9356b230c4abacb834c995dc5332944e735a4172ed3015e6a90c/9.4.5/etc"

getBinDir     = catchIO (getEnv "miniDafny_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "miniDafny_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "miniDafny_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "miniDafny_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "miniDafny_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "miniDafny_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'