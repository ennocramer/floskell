-- | An outout buffer for Text that keeps track of line and column
-- numbers.
module Floskell.Buffer
    ( Buffer
    , empty
    , newline
    , write
    , line
    , column
    , toLazyText
    ) where

import           Data.Text              ( Text )
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder ( Builder )
import qualified Data.Text.Lazy.Builder as TB

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

-- | Append a Text to the output buffer.  It is an error for the
-- string to contain newlines.
write :: Text -> Buffer -> Buffer
write str buf =
    buf { bufferData        = newBufferData
        , bufferDataNoSpace =
              if T.all (== ' ') str then bufferData buf else newBufferData
        , bufferColumn      = bufferColumn buf + T.length str
        }
  where
    newBufferData = bufferData buf `mappend` TB.fromText str

-- | Append a newline to the output buffer.
newline :: Buffer -> Buffer
newline buf = buf { bufferData        = newBufferData
                  , bufferDataNoSpace = newBufferData
                  , bufferLine        = bufferLine buf + 1
                  , bufferColumn      = 0
                  }
  where
    newBufferData = bufferDataNoSpace buf `mappend` TB.singleton '\n'

-- | Return the current line number, counting from 0.
line :: Buffer -> Int
line = bufferLine

-- | Return the column number, counting from 0.
column :: Buffer -> Int
column = bufferColumn

-- | Return the contents of the output buffer as a lazy Text.
toLazyText :: Buffer -> TL.Text
toLazyText = TB.toLazyText . bufferData
