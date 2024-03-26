{-# LINE 1 "Basement\\Terminal\\Size.hsc" #-}
{-# LANGUAGE CApiFFI #-}
module Basement.Terminal.Size 
    ( getDimensions
    ) where
        
import           Foreign
import           Foreign.C
import           Basement.Compat.Base
import           Basement.Types.OffsetSize
import           Basement.Numerical.Subtractive
import           Basement.Numerical.Additive
import           Prelude (fromIntegral)



{-# LINE 16 "Basement\\Terminal\\Size.hsc" #-}

import           System.Win32.Types (HANDLE, BOOL)
import           Graphics.Win32.Misc (getStdHandle, sTD_OUTPUT_HANDLE, StdHandleId)



{-# LINE 27 "Basement\\Terminal\\Size.hsc" #-}




{-# LINE 33 "Basement\\Terminal\\Size.hsc" #-}


{-# LINE 58 "Basement\\Terminal\\Size.hsc" #-}
type Handle = Ptr CChar  -- void *

data SmallRect = SmallRect 
    { left   :: !Int16
    , top    :: !Int16
    , right  :: !Int16
    , bottom :: !Int16
    } deriving (Show)

instance Storable SmallRect where
    sizeOf _ = (8)
{-# LINE 69 "Basement\\Terminal\\Size.hsc" #-}
    alignment _ = 2
{-# LINE 70 "Basement\\Terminal\\Size.hsc" #-}
    peek ptr = do
        l <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 72 "Basement\\Terminal\\Size.hsc" #-}
        r <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 73 "Basement\\Terminal\\Size.hsc" #-}
        t <- (\hsc_ptr -> peekByteOff hsc_ptr 2) ptr
{-# LINE 74 "Basement\\Terminal\\Size.hsc" #-}
        b <- (\hsc_ptr -> peekByteOff hsc_ptr 6) ptr
{-# LINE 75 "Basement\\Terminal\\Size.hsc" #-}
        return (SmallRect l t r b)
    poke ptr (SmallRect l t r b) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr l
{-# LINE 78 "Basement\\Terminal\\Size.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 2) ptr t
{-# LINE 79 "Basement\\Terminal\\Size.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr r
{-# LINE 80 "Basement\\Terminal\\Size.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 6) ptr b
{-# LINE 81 "Basement\\Terminal\\Size.hsc" #-}
        
data Coord = Coord 
    { x :: !Int16
    , y :: !Int16
    } deriving (Show)

instance Storable Coord where
    sizeOf _ = (4)
{-# LINE 89 "Basement\\Terminal\\Size.hsc" #-}
    alignment _ = 2
{-# LINE 90 "Basement\\Terminal\\Size.hsc" #-}
    peek ptr = do
        x <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 92 "Basement\\Terminal\\Size.hsc" #-}
        y <- (\hsc_ptr -> peekByteOff hsc_ptr 2) ptr
{-# LINE 93 "Basement\\Terminal\\Size.hsc" #-}
        return (Coord x y)
    poke ptr (Coord x y) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr x
{-# LINE 96 "Basement\\Terminal\\Size.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 2) ptr y
{-# LINE 97 "Basement\\Terminal\\Size.hsc" #-}

data ConsoleScreenBufferInfo = ConsoleScreenBufferInfo 
    { dwSize              :: !Coord
    , dwCursorPosition    :: !Coord
    , wAttributes         :: !Word16
    , srWindow            :: !SmallRect
    , dwMaximumWindowSize :: !Coord
    } deriving (Show)

instance Storable ConsoleScreenBufferInfo where
    sizeOf _ = (22)
{-# LINE 108 "Basement\\Terminal\\Size.hsc" #-}
    alignment _ = 2
{-# LINE 109 "Basement\\Terminal\\Size.hsc" #-}
    peek ptr = do
        s <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 111 "Basement\\Terminal\\Size.hsc" #-}
        c <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 112 "Basement\\Terminal\\Size.hsc" #-}
        a <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 113 "Basement\\Terminal\\Size.hsc" #-}
        w <- (\hsc_ptr -> peekByteOff hsc_ptr 10) ptr
{-# LINE 114 "Basement\\Terminal\\Size.hsc" #-}
        m <- (\hsc_ptr -> peekByteOff hsc_ptr 18) ptr
{-# LINE 115 "Basement\\Terminal\\Size.hsc" #-}
        return (ConsoleScreenBufferInfo s c a w m)
    poke ptr (ConsoleScreenBufferInfo s c a w m) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr s
{-# LINE 118 "Basement\\Terminal\\Size.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr c
{-# LINE 119 "Basement\\Terminal\\Size.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr a
{-# LINE 120 "Basement\\Terminal\\Size.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 10) ptr w
{-# LINE 121 "Basement\\Terminal\\Size.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 18) ptr m
{-# LINE 122 "Basement\\Terminal\\Size.hsc" #-}
    
invalidHandleValue :: IntPtr
invalidHandleValue = 18446744073709551615
{-# LINE 125 "Basement\\Terminal\\Size.hsc" #-}

stdOutputHandle :: CULong
stdOutputHandle = 4294967285
{-# LINE 128 "Basement\\Terminal\\Size.hsc" #-}

{-# LINE 129 "Basement\\Terminal\\Size.hsc" #-}
-- defined FOUNDATION_SYSTEM_WINDOWS


{-# LINE 140 "Basement\\Terminal\\Size.hsc" #-}
foreign import ccall "GetConsoleScreenBufferInfo" c_get_console_screen_buffer_info 
  :: HANDLE -> Ptr ConsoleScreenBufferInfo -> IO BOOL

{-# LINE 143 "Basement\\Terminal\\Size.hsc" #-}


{-# LINE 157 "Basement\\Terminal\\Size.hsc" #-}
getConsoleScreenBufferInfo :: HANDLE -> IO (Maybe ConsoleScreenBufferInfo)
getConsoleScreenBufferInfo handle = alloca $ \infoPtr -> do
    status <- c_get_console_screen_buffer_info handle infoPtr
    if status
        then Just <$> peek infoPtr
        else pure Nothing
       
winWinsize :: StdHandleId -> IO (Maybe (CountOf Char, CountOf Char))
winWinsize handleRef = (infoToDimensions <$>) <$>
    (getStdHandle handleRef >>= getConsoleScreenBufferInfo)
  where
    infoToDimensions info =
        let window = srWindow info
            width = Prelude.fromIntegral (right window - left window + 1)
            height = Prelude.fromIntegral (bottom window - top window + 1)
         in (CountOf width, CountOf height)

{-# LINE 174 "Basement\\Terminal\\Size.hsc" #-}
-- defined FOUNDATION_SYSTEM_WINDOWS

-- | Return the size of the current terminal
--
-- If the system is not supported or that querying the system result in an error
-- then a default size of (80, 24) will be given back.
getDimensions :: IO (CountOf Char, CountOf Char)
getDimensions =

{-# LINE 183 "Basement\\Terminal\\Size.hsc" #-}
    maybe defaultSize id <$> winWinsize sTD_OUTPUT_HANDLE

{-# LINE 189 "Basement\\Terminal\\Size.hsc" #-}
  where
    defaultSize = (80, 24)
