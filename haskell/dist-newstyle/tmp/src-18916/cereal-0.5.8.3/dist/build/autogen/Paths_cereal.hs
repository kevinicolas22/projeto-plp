{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_cereal (
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
version = Version [0,5,8,3] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\cabal\\store\\ghc-9.4.8\\cereal-0.5.8.3-06595b25dcf47d3e8c0b2d76f5a52f2e153c3f27\\bin"
libdir     = "C:\\cabal\\store\\ghc-9.4.8\\cereal-0.5.8.3-06595b25dcf47d3e8c0b2d76f5a52f2e153c3f27\\lib"
dynlibdir  = "C:\\cabal\\store\\ghc-9.4.8\\cereal-0.5.8.3-06595b25dcf47d3e8c0b2d76f5a52f2e153c3f27\\lib"
datadir    = "C:\\cabal\\store\\ghc-9.4.8\\cereal-0.5.8.3-06595b25dcf47d3e8c0b2d76f5a52f2e153c3f27\\share"
libexecdir = "C:\\cabal\\store\\ghc-9.4.8\\cereal-0.5.8.3-06595b25dcf47d3e8c0b2d76f5a52f2e153c3f27\\libexec"
sysconfdir = "C:\\cabal\\store\\ghc-9.4.8\\cereal-0.5.8.3-06595b25dcf47d3e8c0b2d76f5a52f2e153c3f27\\etc"

getBinDir     = catchIO (getEnv "cereal_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "cereal_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "cereal_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "cereal_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cereal_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cereal_sysconfdir") (\_ -> return sysconfdir)



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
