{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Module      : Basement.Cast
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
--
module Basement.Cast
    ( Cast(..)
    ) where

#include "MachDeps.h"

import qualified Basement.Block.Base as Block
import           Basement.Compat.Base
import           Basement.Compat.Natural
import           Basement.Compat.Primitive
import           Basement.Numerical.Number
import           Basement.Numerical.Conversion
import           Basement.PrimType

import           Data.Proxy (Proxy(..))

import           GHC.Int
import           GHC.Prim
import           GHC.Types
import           GHC.ST
import           GHC.Word

-- | `Cast` an object of type a to b.
--
-- Do not add instance of this class if the source type is not of the same
-- size of the destination type. Also keep in mind this is casting a value
-- of a given type into a destination type. The value won't be changed to
-- fit the destination represention.
--
-- If you wish to convert a value of a given type into another type, look at
-- `From` and `TryFrom`.
--
-- @
-- cast (-10 :: Int) :: Word === 18446744073709551606
-- @
--
class Cast source destination where
    cast :: source -> destination

    default cast :: ( PrimType source
                    , PrimType destination
                    , PrimSize source ~ PrimSize destination
                    )
                 => source -> destination
    cast a = runST $ do
        mba <- Block.new 1
        Block.unsafeWrite mba 0 a
        Block.unsafeRead (Block.unsafeRecast mba) 0

instance Cast Int8  Word8 where
    cast (I8# i) = W8# (wordToWord8# (int2Word# (int8ToInt# i)))
instance Cast Int16 Word16 where
    cast (I16# i) = W16# (wordToWord16# (int2Word# (int16ToInt# i)))
instance Cast Int32 Word32 where
    cast (I32# i) = W32# (wordToWord32# (int2Word# (int32ToInt# i)))
instance Cast Int64 Word64 where
    cast = int64ToWord64
instance Cast Int   Word where
    cast (I# i) = W# (int2Word# i)

instance Cast Word8  Int8 where
    cast (W8# i) = I8# (intToInt8# (word2Int# (word8ToWord# i)))
instance Cast Word16 Int16 where
    cast (W16# i) = I16# (intToInt16# (word2Int# (word16ToWord# i)))
instance Cast Word32 Int32 where
    cast (W32# i) = I32# (intToInt32# (word2Int# (word32ToWord# i)))
instance Cast Word64 Int64 where
    cast = word64ToInt64
instance Cast Word   Int where
    cast (W# w) = I# (word2Int# w)

#if WORD_SIZE_IN_BITS == 64
#if __GLASGOW_HASKELL__ >= 904
instance Cast Word   Word64 where
    cast (W# w) = W64# (wordToWord64# w)
instance Cast Word64 Word where
    cast (W64# w) = W# (GHC.Prim.word64ToWord# w)

instance Cast Word   Int64 where
    cast (W# w) = I64# (intToInt64# (word2Int# w))
instance Cast Int64  Word where
    cast (I64# i) = W# (int2Word# (int64ToInt# i))

instance Cast Int    Int64 where
    cast (I# i) = I64# (intToInt64# i)
instance Cast Int64  Int where
    cast (I64# i) = I# (int64ToInt# i)

instance Cast Int    Word64 where
    cast (I# i) = W64# (wordToWord64# (int2Word# i))
instance Cast Word64 Int where
    cast (W64# w) = I# (word2Int# (GHC.Prim.word64ToWord# w))
#else
instance Cast Word   Word64 where
    cast (W# w) = W64# w
instance Cast Word64 Word where
    cast (W64# w) = W# w

instance Cast Word   Int64 where
    cast (W# w) = I64# (word2Int# w)
instance Cast Int64  Word where
    cast (I64# i) = W# (int2Word# i)

instance Cast Int    Int64 where
    cast (I# i) = I64# i
instance Cast Int64  Int where
    cast (I64# i) = I# i

instance Cast Int    Word64 where
    cast (I# i) = W64# (int2Word# i)
instance Cast Word64 Int where
    cast (W64# w) = I# (word2Int# w)
#endif
#else
instance Cast Word   Word32 where
    cast (W# w) = W32# (wordToWord32# w)
instance Cast Word32 Word where
    cast (W32# w) = W# (word32ToWord# w)

instance Cast Word   Int32 where
    cast (W# w) = I32# (intToInt32# (word2Int# w))
instance Cast Int32  Word where
    cast (I32# i) = W# (int2Word# (int32ToInt# i))

instance Cast Int    Int32 where
    cast (I# i) = I32# (intToInt32# i)
instance Cast Int32  Int where
    cast (I32# i) = I# (int32ToInt# i)

instance Cast Int    Word32 where
    cast (I# i) = W32# (wordToWord32# (int2Word# i))
instance Cast Word32 Int where
    cast (W32# w) = I# (word2Int# (word32ToWord# w))
#endif

instance Cast (Block.Block a) (Block.Block Word8) where
    cast (Block.Block ba) = Block.Block ba
