module Paths_oncotime (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/brendan/Class/oncotime/comp520-2016-18/.cabal-sandbox/bin"
libdir     = "/home/brendan/Class/oncotime/comp520-2016-18/.cabal-sandbox/lib/i386-linux-ghc-7.10.3/oncotime-0.1.0.0-CPrxhykYabsH9abVNB51y8"
datadir    = "/home/brendan/Class/oncotime/comp520-2016-18/.cabal-sandbox/share/i386-linux-ghc-7.10.3/oncotime-0.1.0.0"
libexecdir = "/home/brendan/Class/oncotime/comp520-2016-18/.cabal-sandbox/libexec"
sysconfdir = "/home/brendan/Class/oncotime/comp520-2016-18/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "oncotime_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "oncotime_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "oncotime_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "oncotime_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "oncotime_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
