{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Multilinear (
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

bindir     = "C:\\Users\\Artur\\Downloads\\Multilinear\\.stack-work\\install\\401d7a24\\bin"
libdir     = "C:\\Users\\Artur\\Downloads\\Multilinear\\.stack-work\\install\\401d7a24\\lib\\x86_64-windows-ghc-8.0.2\\Multilinear-0.1.0.0-E9ARM2QGVFX7aqrS3x5JFU"
dynlibdir  = "C:\\Users\\Artur\\Downloads\\Multilinear\\.stack-work\\install\\401d7a24\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\Artur\\Downloads\\Multilinear\\.stack-work\\install\\401d7a24\\share\\x86_64-windows-ghc-8.0.2\\Multilinear-0.1.0.0"
libexecdir = "C:\\Users\\Artur\\Downloads\\Multilinear\\.stack-work\\install\\401d7a24\\libexec"
sysconfdir = "C:\\Users\\Artur\\Downloads\\Multilinear\\.stack-work\\install\\401d7a24\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Multilinear_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Multilinear_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Multilinear_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Multilinear_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Multilinear_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Multilinear_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
