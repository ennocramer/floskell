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

data Buffer =
    Buffer { bufferData        :: !Builder -- ^ The current output.
           , bufferDataNoSpace :: !Builder -- ^ The current output without trailing spaces.
           , bufferLine        :: !Int     -- ^ Current line number.
           , bufferColumn      :: !Int     -- ^ Current column number.
           }

-- | An empty output buffer.
empty :: Buffer
empty = Buffer { bufferData        = mempty
               , bufferDataNoSpace = mempty
               , bufferLine        = 0
               , bufferColumn      = 0
               }

-- | Append a ByteString to the output buffer.  It is an error for the
-- string to contain newlines.
write :: BS.ByteString -> Buffer -> Buffer
write str buf =
    buf { bufferData        = newBufferData
        , bufferDataNoSpace =
              if BS.all (== 32) str then bufferData buf else newBufferData
        , bufferColumn      = bufferColumn buf + BS.length str
        }
  where
    newBufferData = bufferData buf `mappend` BB.byteString str

-- | Append a newline to the output buffer.
newline :: Buffer -> Buffer
newline buf = buf { bufferData        = newBufferData
                  , bufferDataNoSpace = newBufferData
                  , bufferLine        = bufferLine buf + 1
                  , bufferColumn      = 0
                  }
  where
    newBufferData = bufferDataNoSpace buf `mappend` BB.char7 '\n'

-- | Return the current line number, counting from 0.
line :: Buffer -> Int
line = bufferLine

-- | Return the column number, counting from 0.
column :: Buffer -> Int
column = bufferColumn

-- | Return the contents of the output buffer as a lazy ByteString.
toLazyByteString :: Buffer -> BL.ByteString
toLazyByteString = BB.toLazyByteString . bufferData
