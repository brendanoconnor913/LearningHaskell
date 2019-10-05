module Paths_monoid (
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

bindir     = "/home/brendan/HaskellFiles/monoid/.cabal-sandbox/bin"
libdir     = "/home/brendan/HaskellFiles/monoid/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.3/monoid-0.1.0.0-EkymN8Syq3bKRaZpwB6D6Q"
datadir    = "/home/brendan/HaskellFiles/monoid/.cabal-sandbox/share/x86_64-linux-ghc-7.10.3/monoid-0.1.0.0"
libexecdir = "/home/brendan/HaskellFiles/monoid/.cabal-sandbox/libexec"
sysconfdir = "/home/brendan/HaskellFiles/monoid/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "monoid_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "monoid_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "monoid_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "monoid_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "monoid_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
