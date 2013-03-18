module Paths_nitrogen (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/hinkes/.cabal/bin"
libdir     = "/home/hinkes/.cabal/lib/nitrogen-0.0.0.0/ghc-7.6.1"
datadir    = "/home/hinkes/.cabal/share/nitrogen-0.0.0.0"
libexecdir = "/home/hinkes/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "nitrogen_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "nitrogen_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "nitrogen_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "nitrogen_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
