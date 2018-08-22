{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_HaskellTorb (
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

bindir     = "D:\\Zyphicx folders\\Programming\\Haskell\\HaskellTorb\\.stack-work\\install\\dbe016db\\bin"
libdir     = "D:\\Zyphicx folders\\Programming\\Haskell\\HaskellTorb\\.stack-work\\install\\dbe016db\\lib\\x86_64-windows-ghc-8.4.3\\HaskellTorb-0.1.0.0-GGpBjvooKb2IpUJVZzvPkz-HaskellTorb"
dynlibdir  = "D:\\Zyphicx folders\\Programming\\Haskell\\HaskellTorb\\.stack-work\\install\\dbe016db\\lib\\x86_64-windows-ghc-8.4.3"
datadir    = "D:\\Zyphicx folders\\Programming\\Haskell\\HaskellTorb\\.stack-work\\install\\dbe016db\\share\\x86_64-windows-ghc-8.4.3\\HaskellTorb-0.1.0.0"
libexecdir = "D:\\Zyphicx folders\\Programming\\Haskell\\HaskellTorb\\.stack-work\\install\\dbe016db\\libexec\\x86_64-windows-ghc-8.4.3\\HaskellTorb-0.1.0.0"
sysconfdir = "D:\\Zyphicx folders\\Programming\\Haskell\\HaskellTorb\\.stack-work\\install\\dbe016db\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "HaskellTorb_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HaskellTorb_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "HaskellTorb_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "HaskellTorb_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HaskellTorb_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HaskellTorb_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
