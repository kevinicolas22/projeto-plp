{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_StateVar (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "StateVar"
version :: Version
version = Version [1,2,2] []

synopsis :: String
synopsis = "State variables"
copyright :: String
copyright = "Copyright (C) 2014-2015 Edward A. Kmett, 2009-2021 Sven Panne"
homepage :: String
homepage = "https://github.com/haskell-opengl/StateVar"
