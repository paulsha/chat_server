{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_chat_server (
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

bindir     = "C:\\Users\\Paul\\Documents\\GitHub\\chat_server\\chat-server\\.stack-work\\install\\d7a37d2f\\bin"
libdir     = "C:\\Users\\Paul\\Documents\\GitHub\\chat_server\\chat-server\\.stack-work\\install\\d7a37d2f\\lib\\x86_64-windows-ghc-8.0.2\\chat-server-0.1.0.0-LPPVTbFQ8Ff7tFcQ6WoY7D"
dynlibdir  = "C:\\Users\\Paul\\Documents\\GitHub\\chat_server\\chat-server\\.stack-work\\install\\d7a37d2f\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\Paul\\Documents\\GitHub\\chat_server\\chat-server\\.stack-work\\install\\d7a37d2f\\share\\x86_64-windows-ghc-8.0.2\\chat-server-0.1.0.0"
libexecdir = "C:\\Users\\Paul\\Documents\\GitHub\\chat_server\\chat-server\\.stack-work\\install\\d7a37d2f\\libexec"
sysconfdir = "C:\\Users\\Paul\\Documents\\GitHub\\chat_server\\chat-server\\.stack-work\\install\\d7a37d2f\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "chat_server_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "chat_server_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "chat_server_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "chat_server_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "chat_server_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "chat_server_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
