{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module DoublePair where

import qualified Data.Vector.Generic as G 
import qualified Data.Vector.Generic.Mutable as M 
import Control.Monad (liftM, zipWithM_)
import Data.Vector.Unboxed.Base

data DoublePair = DoublePair {-# UNPACK #-} !Double {-# UNPACK #-} !Double deriving (Eq, Ord)

newtype instance MVector s DoublePair = MV_DoublePair (MVector s Double)
newtype instance Vector    DoublePair = V_DoublePair  (Vector    Double)
instance Unbox DoublePair

instance M.MVector MVector DoublePair where 
  basicLength (MV_DoublePair v) = M.basicLength v `quot` 2
  basicUnsafeSlice a b (MV_DoublePair v) = MV_DoublePair $ M.basicUnsafeSlice (a*2) (b*2) v 
  basicOverlaps (MV_DoublePair v0) (MV_DoublePair v1) = M.basicOverlaps v0 v1
  basicUnsafeNew n = liftM MV_DoublePair (M.basicUnsafeNew (2*n))
  basicUnsafeRead (MV_DoublePair v) n = do 
    [a,b] <- mapM (M.basicUnsafeRead v) [2*n,2*n+1]
    return $ DoublePair a b
  basicUnsafeWrite (MV_DoublePair v) n (DoublePair a b)
    = zipWithM_ (M.basicUnsafeWrite v) [2*n,2*n+1] [a,b]

instance G.Vector Vector DoublePair where 
  basicUnsafeFreeze (MV_DoublePair v) = liftM V_DoublePair (G.basicUnsafeFreeze v)
  basicUnsafeThaw (V_DoublePair v) = liftM MV_DoublePair (G.basicUnsafeThaw v)
  basicLength (V_DoublePair v) = G.basicLength v `quot` 2
  basicUnsafeSlice a b (V_DoublePair v) = V_DoublePair $ G.basicUnsafeSlice (a*2) (b*2) v
  basicUnsafeIndexM (V_DoublePair v) n = do 
    [a,b] <- mapM (G.basicUnsafeIndexM v) [2*n,2*n+1]
    return $ DoublePair a b 