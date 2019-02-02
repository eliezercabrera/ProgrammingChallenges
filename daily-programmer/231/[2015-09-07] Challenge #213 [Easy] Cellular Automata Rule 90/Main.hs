{-# LANGUAGE LambdaCase #-}
main=interact$unlines.map((\case{True->'x';_->' '})<$>).take 25.iterate(\t@(_:s)->zipWith(/=)((1<0):init s)(s++[1<0])).map(\case{'0'->1<0;_->1>0})