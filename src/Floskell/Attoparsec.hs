{-# LANGUAGE CPP #-}

module Floskell.Attoparsec ( Position, parseOnly ) where

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString            as B
import           Data.Semigroup             as Sem
import           Data.Word                  ( Word8 )

data Position = Position { posLine :: !Int, posColumn :: !Int }
    deriving ( Eq, Ord, Show )

instance Sem.Semigroup Position where
    (Position l c) <> (Position l' c') =
        Position (l + l') (if l' > 0 then c' else c + c')

instance Monoid Position where
    mempty = Position 0 0

#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

advance :: Position -> Word8 -> Position
advance (Position l _) 10 = Position (l + 1) 0
advance (Position l c) _ = Position l (c + 1)

position :: B.ByteString -> Position
position = B.foldl' advance mempty

positionAt :: B.ByteString -> B.ByteString -> Position
positionAt l r = position $ B.take (B.length l - B.length r) l

parseOnly :: AP.Parser a -> B.ByteString -> Either String a
parseOnly p b = case AP.feed (AP.parse p b) B.empty of
    AP.Done _ x -> Right x
    AP.Fail r _ err -> Left $ parseError (positionAt b r) err
    AP.Partial _ -> error "impossible"
  where
    parseError (Position l c) err = err ++ ", at line " ++ show (l + 1)
        ++ ", column " ++ show c
