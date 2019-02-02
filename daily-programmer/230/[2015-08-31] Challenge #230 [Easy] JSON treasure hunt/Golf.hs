    {-# LANGUAGE OverloadedStrings #-}

    import Data.Aeson
    import Control.Monad
    import qualified Data.ByteString.Lazy.Char8 as B
    import qualified Data.HashMap.Strict as M
    import qualified Data.Text as T
    import Data.List
    import qualified Data.Vector as V

    path :: Value -> String
    path = maybe "No path found." (intercalate " -> ") . go
        where go (String "dailyprogrammer") = Just []
              go (Object o) = msum [(k:) <$> go v | (k, v) <- M.toList o]
              go (Array a ) = msum [(k:) <$> go v | (k, v) <- zip keys (V.toList a)]
                                  where keys = map show [0..]
              go _          = Nothing

    main = interact $ maybe "Error decoding input file." path . decode . B.unpack