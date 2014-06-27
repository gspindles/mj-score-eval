module Paths_Mahjong (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/archon/Git/mj-score-eval/.cabal-sandbox/bin"
libdir     = "/home/archon/Git/mj-score-eval/.cabal-sandbox/lib/x86_64-linux-ghc-7.8.2/Mahjong-0.1.0.0"
datadir    = "/home/archon/Git/mj-score-eval/.cabal-sandbox/share/x86_64-linux-ghc-7.8.2/Mahjong-0.1.0.0"
libexecdir = "/home/archon/Git/mj-score-eval/.cabal-sandbox/libexec"
sysconfdir = "/home/archon/Git/mj-score-eval/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Mahjong_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Mahjong_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Mahjong_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Mahjong_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Mahjong_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
