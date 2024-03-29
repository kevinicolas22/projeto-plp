{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_hourglass (
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
version = Version [0,2,12] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\cabal\\store\\ghc-9.4.8\\hourglass-0.2.12-c6530661fc1cb5f12bf20d96ceedc1f3b865515b\\bin"
libdir     = "C:\\cabal\\store\\ghc-9.4.8\\hourglass-0.2.12-c6530661fc1cb5f12bf20d96ceedc1f3b865515b\\lib"
dynlibdir  = "C:\\cabal\\store\\ghc-9.4.8\\hourglass-0.2.12-c6530661fc1cb5f12bf20d96ceedc1f3b865515b\\lib"
datadir    = "C:\\cabal\\store\\ghc-9.4.8\\hourglass-0.2.12-c6530661fc1cb5f12bf20d96ceedc1f3b865515b\\share"
libexecdir = "C:\\cabal\\store\\ghc-9.4.8\\hourglass-0.2.12-c6530661fc1cb5f12bf20d96ceedc1f3b865515b\\libexec"
sysconfdir = "C:\\cabal\\store\\ghc-9.4.8\\hourglass-0.2.12-c6530661fc1cb5f12bf20d96ceedc1f3b865515b\\etc"

getBinDir     = catchIO (getEnv "hourglass_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "hourglass_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "hourglass_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "hourglass_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hourglass_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hourglass_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
