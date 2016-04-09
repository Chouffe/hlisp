module Paths_hlisp (
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

bindir     = "/home/chouffe/Documents/Haskell/hlisp/.stack-work/install/x86_64-linux/lts-5.11/7.10.3/bin"
libdir     = "/home/chouffe/Documents/Haskell/hlisp/.stack-work/install/x86_64-linux/lts-5.11/7.10.3/lib/x86_64-linux-ghc-7.10.3/hlisp-0.1.0.0-GOYMb9K7faXDOlOke6FXo9"
datadir    = "/home/chouffe/Documents/Haskell/hlisp/.stack-work/install/x86_64-linux/lts-5.11/7.10.3/share/x86_64-linux-ghc-7.10.3/hlisp-0.1.0.0"
libexecdir = "/home/chouffe/Documents/Haskell/hlisp/.stack-work/install/x86_64-linux/lts-5.11/7.10.3/libexec"
sysconfdir = "/home/chouffe/Documents/Haskell/hlisp/.stack-work/install/x86_64-linux/lts-5.11/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hlisp_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hlisp_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hlisp_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hlisp_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hlisp_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
