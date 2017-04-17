{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Multilinear (
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
version = Version [0,8,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Artur\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Artur\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.1\\Multilinear-0.8.0.0-DGHahwKhwVt6qIIrGNXaj3"
datadir    = "C:\\Users\\Artur\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.1\\Multilinear-0.8.0.0"
libexecdir = "C:\\Users\\Artur\\AppData\\Roaming\\cabal\\Multilinear-0.8.0.0-DGHahwKhwVt6qIIrGNXaj3"
sysconfdir = "C:\\Users\\Artur\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Multilinear_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Multilinear_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Multilinear_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Multilinear_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Multilinear_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
