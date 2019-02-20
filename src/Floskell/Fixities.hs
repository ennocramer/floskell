-- DO NOT EDIT
-- Edit and run scripts/update-fixities to update the list of builtin fixities
module Floskell.Fixities ( builtinFixities, packageFixities ) where

import           Language.Haskell.Exts.Fixity
                 ( Fixity, baseFixities, infix_, infixl_, infixr_ )

builtinFixities :: [Fixity]
builtinFixities = concatMap snd packageFixities

packageFixities :: [(String, [Fixity])]
packageFixities = [ ("base", baseFixities)
                  , ("aeson", aesonFixities)
                  , ("conduit", conduitFixities)
                  , ("lens", lensFixities)
                  , ("pipes", pipesFixities)
                  , ("servant", servantFixities)
                  ]

aesonFixities :: [Fixity]
aesonFixities =
    -- Data/Aeson/Types/ToJSON.hs
    infixr_ 8 [ ".=" ]
    -- Data/Aeson/Encoding/Internal.hs
    ++ infixr_ 6 [ ">*<" ] ++ infixr_ 6 [ "><" ]
    -- Data/Aeson/TH.hs
    ++ infixr_ 6 [ "<^>" ] ++ infixr_ 4 [ "<%>" ]

conduitFixities :: [Fixity]
conduitFixities =
    -- src/Data/Conduit/Internal/Conduit.hs
    infixr_ 0 [ "$$" ] ++ infixl_ 1 [ "$=" ] ++ infixr_ 2 [ "=$" ]
    ++ infixr_ 2 [ "=$=" ] ++ infixr_ 0 [ "$$+" ] ++ infixr_ 0 [ "$$++" ]
    ++ infixr_ 0 [ "$$+-" ] ++ infixl_ 1 [ "$=+" ] ++ infixr_ 2 [ ".|" ]
    ++ infixr_ 0 [ "=$$+" ] ++ infixr_ 0 [ "=$$++" ] ++ infixr_ 0 [ "=$$+-" ]
    -- src/Data/Conduit/Internal/Pipe.hs
    ++ infixr_ 9 [ "<+<" ] ++ infixl_ 9 [ ">+>" ]

lensFixities :: [Fixity]
lensFixities =
    -- src/System/FilePath/Lens.hs
    infixr_ 4 [ "</>~", "<</>~", "<<</>~", "<.>~", "<<.>~", "<<<.>~" ]
    ++ infix_ 4 [ "</>=", "<</>=", "<<</>=", "<.>=", "<<.>=", "<<<.>=" ]
    -- src/Data/Bits/Lens.hs
    ++ infixr_ 4 [ ".|.~", ".&.~", "<.|.~", "<.&.~", "<<.|.~", "<<.&.~" ]
    ++ infix_ 4 [ ".|.=", ".&.=", "<.|.=", "<.&.=", "<<.|.=", "<<.&.=" ]
    -- src/Control/Lens/Fold.hs
    ++ infixl_ 8 [ "^..", "^?", "^?!", "^@..", "^@?", "^@?!" ]
    -- src/Control/Lens/Indexed.hs
    ++ infixr_ 9 [ "<.>", "<.", ".>" ]
    -- src/Control/Lens/Setter.hs
    ++ infixr_ 4
               [ "%@~"
               , ".@~"
               , ".~"
               , "+~"
               , "*~"
               , "-~"
               , "//~"
               , "^~"
               , "^^~"
               , "**~"
               , "&&~"
               , "<>~"
               , "||~"
               , "%~"
               , "<.~"
               , "?~"
               , "<?~"
               ]
    ++ infix_ 4
              [ "%@="
              , ".@="
              , ".="
              , "+="
              , "*="
              , "-="
              , "//="
              , "^="
              , "^^="
              , "**="
              , "&&="
              , "<>="
              , "||="
              , "%="
              , "<.="
              , "?="
              , "<?="
              ] ++ infixr_ 2 [ "<~" ]
    -- src/Control/Lens/Cons.hs
    ++ infixr_ 5 [ "<|", "`cons`" ] ++ infixl_ 5 [ "|>", "`snoc`" ]
    ++ infixr_ 5 [ ":<" ] ++ infixl_ 5 [ ":>" ]
    -- src/Control/Lens/Plated.hs
    ++ infixr_ 9 [ "..." ]
    -- src/Control/Lens/Getter.hs
    ++ infixl_ 8 [ "^.", "^@." ]
    -- src/Control/Lens/Zoom.hs
    ++ infixr_ 2 [ "`zoom`", "`magnify`" ]
    -- src/Control/Lens/Lens.hs
    ++ infixl_ 8 [ "^#" ]
    ++ infixr_ 4
               [ "%%@~"
               , "<%@~"
               , "<<%@~"
               , "%%~"
               , "<+~"
               , "<*~"
               , "<-~"
               , "<//~"
               , "<^~"
               , "<^^~"
               , "<**~"
               , "<&&~"
               , "<||~"
               , "<<>~"
               , "<%~"
               , "<<%~"
               , "<<.~"
               , "<<?~"
               , "<#~"
               , "#~"
               , "#%~"
               , "<#%~"
               , "#%%~"
               ]
    ++ infix_ 4
              [ "%%@="
              , "<%@="
              , "<<%@="
              , "%%="
              , "<+="
              , "<*="
              , "<-="
              , "<//="
              , "<^="
              , "<^^="
              , "<**="
              , "<&&="
              , "<||="
              , "<<>="
              , "<%="
              , "<<%="
              , "<<.="
              , "<<?="
              , "<#="
              , "#="
              , "#%="
              , "<#%="
              , "#%%="
              ] ++ infixr_ 2 [ "<<~" ] ++ infixl_ 1 [ "??", "&~" ]
    ++ infixl_ 1 [ "&" ] ++ infixl_ 1 [ "<&>" ]
    -- src/Control/Lens/Review.hs
    ++ infixr_ 8 [ "#" ]
    -- src/Control/Lens/Traversal.hs
    ++ infixl_ 5 [ "`failing`" ]

pipesFixities :: [Fixity]
pipesFixities =
    -- src/Pipes.hs
    infixl_ 4 [ "<~" ] ++ infixr_ 4 [ "~>" ] ++ infixl_ 5 [ "~<" ]
    ++ infixr_ 5 [ ">~" ] ++ infixl_ 7 [ ">->" ] ++ infixr_ 7 [ "<-<" ]
    -- src/Pipes/Core.hs
    ++ infixl_ 3 [ "//>" ] ++ infixr_ 3 [ "<\\\\" ]
    ++ infixr_ 4 [ "/>/", ">\\\\" ] ++ infixl_ 4 [ "\\<\\", "//<" ]
    ++ infixl_ 5 [ "\\>\\" ] ++ infixr_ 5 [ "/</" ] ++ infixl_ 6 [ "<<+" ]
    ++ infixr_ 6 [ "+>>" ] ++ infixl_ 7 [ ">+>", ">>~" ]
    ++ infixr_ 7 [ "<+<", "~<<" ] ++ infixl_ 8 [ "<~<" ] ++ infixr_ 8 [ ">~>" ]

servantFixities :: [Fixity]
servantFixities =
    -- src/Servant/API/Sub.hs
    infixr_ 4 [ ":>" ]
    -- src/Servant/API/Alternative.hs
    ++ infixr_ 3 [ ":<|>" ]
    -- src/Servant/API/Generic.hs
    ++ infixl_ 0 [ ":-" ]
