{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_StateVar (
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
version = Version [1,2,2] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\cabal\\store\\ghc-9.4.8\\StateVar-1.2.2-cb4127cf467655a44223029db55b7d20471ce428\\bin"
libdir     = "C:\\cabal\\store\\ghc-9.4.8\\StateVar-1.2.2-cb4127cf467655a44223029db55b7d20471ce428\\lib"
dynlibdir  = "C:\\cabal\\store\\ghc-9.4.8\\StateVar-1.2.2-cb4127cf467655a44223029db55b7d20471ce428\\lib"
datadir    = "C:\\cabal\\store\\ghc-9.4.8\\StateVar-1.2.2-cb4127cf467655a44223029db55b7d20471ce428\\share"
libexecdir = "C:\\cabal\\store\\ghc-9.4.8\\StateVar-1.2.2-cb4127cf467655a44223029db55b7d20471ce428\\libexec"
sysconfdir = "C:\\cabal\\store\\ghc-9.4.8\\StateVar-1.2.2-cb4127cf467655a44223029db55b7d20471ce428\\etc"

getBinDir     = catchIO (getEnv "StateVar_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "StateVar_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "StateVar_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "StateVar_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "StateVar_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "StateVar_sysconfdir") (\_ -> return sysconfdir)



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
