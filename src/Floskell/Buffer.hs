{-# LANGUAGE OverloadedStrings #-}

-- | An outout buffer for ByteStrings that keeps track of line and
-- column numbers.
module Floskell.Buffer
    ( Buffer
    , empty
    , newline
    , write
    , line
    , column
    , toLazyByteString
    ) where

import qualified Data.ByteString         as BS
import           Data.ByteString.Builder ( Builder )
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as BL

import           Data.Int                ( Int64 )

data Buffer = Buffer { bufferData   :: !Builder -- ^ The current output.
                     , bufferLine   :: !Int64   -- ^ Current line number.
                     , bufferColumn :: !Int64   -- ^ Current column number.
                     }

-- | An empty output buffer.
empty :: Buffer
empty = Buffer { bufferData = mempty, bufferLine = 0, bufferColumn = 0 }

-- | Append a ByteString to the output buffer.  It is an error for the
-- string to contain newlines.
write :: BS.ByteString -> Buffer -> Buffer
write str buf =
    buf { bufferData = bufferData buf `mappend` BB.byteString str
        , bufferColumn = bufferColumn buf + fromIntegral (BS.length str)
        }

-- | Append a newline to the output buffer.
newline :: Buffer -> Buffer
newline buf = buf { bufferData = bufferData buf `mappend` BB.char7 '\n'
                  , bufferLine = bufferLine buf + 1
                  , bufferColumn = 0
                  }

-- | Return the current line number, counting from 0.
line :: Buffer -> Int64
line = bufferLine

-- | Return the column number, counting from 0.
column :: Buffer -> Int64
column = bufferColumn

-- | Return the contents of the output buffer as a lazy ByteString.
toLazyByteString :: Buffer -> BL.ByteString
toLazyByteString = BB.toLazyByteString . bufferData