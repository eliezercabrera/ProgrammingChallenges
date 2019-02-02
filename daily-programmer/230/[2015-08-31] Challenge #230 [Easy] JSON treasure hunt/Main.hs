{-# LANGUAGE OverloadedStrings,
             MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             FlexibleContexts #-}

import Data.Aeson
import Data.List
import Control.Monad
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import Data.ByteString.Lazy.Char8 (pack)

class Keyable a b | a -> b where  -- promise that a uniquely determines b
    toAssocList :: a -> [(T.Text, b)]

instance Keyable (V.Vector a) a where
    toAssocList = zip (T.pack . show <$> [0..]) . V.toList

instance Keyable (M.HashMap T.Text b) b where
    toAssocList = M.toList

search :: T.Text -> Value -> String
search target = maybe "There is no treasure." (intercalate " -> ") . go
  where go (Object m) = search' m
        go (Array  v) = search' v
        go (String s) = guard (s == target) >> return []
        go       _    = Nothing
        search' :: (Keyable t Value) => t -> Maybe [String]
        search' = msum . fmap (\(k, v) -> (T.unpack k :) <$> go v) . toAssocList

main :: IO ()
main = interact $ maybe "Malformed JSON." (search "dailyprogrammer") . decode . pack