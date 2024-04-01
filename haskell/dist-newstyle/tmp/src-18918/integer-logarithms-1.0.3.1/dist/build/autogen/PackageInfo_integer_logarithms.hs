{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_integer_logarithms (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "integer_logarithms"
version :: Version
version = Version [1,0,3,1] []

synopsis :: String
synopsis = "Integer logarithms."
copyright :: String
copyright = "(c) 2011 Daniel Fischer, 2017-2020 Oleg Grenrus, Andrew Lelechenko"
homepage :: String
homepage = "https://github.com/haskellari/integer-logarithms"
