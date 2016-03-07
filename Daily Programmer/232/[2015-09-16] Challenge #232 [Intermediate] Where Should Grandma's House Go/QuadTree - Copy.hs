module QuadTree where

import qualified Data.Set as Set
import qualified Data.List as List
import Data.Ord

type Size = Int
type Distance = Double
type Coordinate = (Distance, Distance)

euclideanDistance :: Coordinate -> Coordinate -> Distance
euclideanDistance (x1, y1) (x2, y2) = sqrt $ (x1 - x2)^(2 :: Int) + (y1 - y2)^(2 :: Int)

euclideanDistanceApprox :: Coordinate -> Coordinate -> Distance
euclideanDistanceApprox (x1, y1) (x2, y2) = (x1 - x2)^(2 :: Int) + (y1 - y2)^(2 :: Int)

data Quadrant = Q1 | Q2 | Q3 | Q4 deriving (Eq, Read, Show)
                                  -- Q2 | Q1
                                  -- ---+---
                                  -- Q3 | Q4

comparePoints :: Coordinate -> Coordinate -> Quadrant
comparePoints (x1, y1) (x2, y2) = case (x1 <= x2, y1 <= y2) of
                                    (True , True ) -> Q1
                                    (True , False) -> Q4
                                    (False, True ) -> Q2
                                    (False, False) -> Q3

data QuadTreeInternal = Branch !Coordinate !Distance !Size !QuadTreeInternal !QuadTreeInternal !QuadTreeInternal !QuadTreeInternal
                        | Leaf !Coordinate !Distance !(Set.Set Coordinate)
                        deriving (Eq, Read, Show)

empty :: Coordinate -> Distance -> QuadTreeInternal
empty c d = Leaf c d (Set.empty)

eDToLeaf :: Coordinate -> QuadTreeInternal -> Distance
eDToLeaf c (Leaf cl _ _) = euclideanDistanceApprox c cl

getPoints :: QuadTreeInternal -> Set.Set Coordinate
getPoints (Leaf _ _ s) = s
getPoints (Branch _ _ _ q1 q2 q3 q4) = getPoints q1 `Set.union` getPoints q2 `Set.union` getPoints q3 `Set.union` getPoints q4

getSize :: QuadTreeInternal -> Int
getSize (Leaf _ _ s) = Set.size s
getSize (Branch _ _ s _ _ _ _) = s

insertPoint :: Size -> Coordinate -> QuadTreeInternal -> QuadTreeInternal
insertPoint size c (Leaf ce@(x, y) hd s)
                                  | Set.size s < size = Leaf ce hd (Set.insert c s)
                                  | otherwise = insertPoint size c branch
                                      where e1 = empty (x + 0.5*hd, y + 0.5*hd) (0.5*hd)
                                            e2 = empty (x - 0.5*hd, y + 0.5*hd) (0.5*hd)
                                            e3 = empty (x - 0.5*hd, y - 0.5*hd) (0.5*hd)
                                            e4 = empty (x + 0.5*hd, y - 0.5*hd) (0.5*hd)
                                            branch = Set.foldl (flip $ insertPoint size) (Branch ce hd 0 e1 e2 e3 e4) s
insertPoint size c (Branch ce hd count q1 q2 q3 q4)
        = case comparePoints ce c of
            Q1 -> Branch ce hd (count + 1) (insertPoint size c q1) q2 q3 q4
            Q2 -> Branch ce hd (count + 1) q1 (insertPoint size c q2) q3 q4
            Q3 -> Branch ce hd (count + 1) q1 q2 (insertPoint size c q3) q4
            Q4 -> Branch ce hd (count + 1) q1 q2 q3 (insertPoint size c q4)
            
deletePoint :: Size -> Coordinate -> QuadTreeInternal -> QuadTreeInternal
deletePoint size c (Leaf ce@(x, y) hd s) = Leaf ce hd (Set.delete c s)
deletePoint size c branch@(Branch ce hd count q1 q2 q3 q4)
    | count <= size = deletePoint size c $ Set.foldl (flip $ insertPoint size) (empty ce hd) (getPoints branch)
    | otherwise = case comparePoints ce c of
                    Q1 -> Branch ce hd (count - 1) (deletePoint size c q1) q2 q3 q4
                    Q2 -> Branch ce hd (count - 1) q1 (deletePoint size c q2) q3 q4
                    Q3 -> Branch ce hd (count - 1) q1 q2 (deletePoint size c q3) q4
                    Q4 -> Branch ce hd (count - 1) q1 q2 q3 (deletePoint size c q4)

containingLeaf :: Coordinate -> QuadTreeInternal -> QuadTreeInternal
containingLeaf _ leaf@(Leaf _ _ _) = leaf
containingLeaf c branch@(Branch ce _ _ q1 q2 q3 q4)
        = case comparePoints ce c of
            Q1 -> if getSize (containingLeaf c q1) == 0 then branch else (containingLeaf c q1)
            Q2 -> if getSize (containingLeaf c q2) == 0 then branch else (containingLeaf c q2)
            Q3 -> if getSize (containingLeaf c q3) == 0 then branch else (containingLeaf c q3)
            Q4 -> if getSize (containingLeaf c q4) == 0 then branch else (containingLeaf c q4)

isInRange :: Coordinate -> Distance -> QuadTreeInternal -> Bool
isInRange c@(x1, y1) d (Leaf ce@(x2, y2) hd _)
        = case comparePoints c ce of
            Q1 -> x1 + d >= x2 - hd && y1 + d >= y2 - hd
            Q2 -> x1 - d <= x2 + hd && y1 + d >= y2 - hd
            Q3 -> x1 - d <= x2 + hd && y1 - d <= y2 + hd
            Q4 -> x1 + d >= x2 - hd && y1 - d <= y2 + hd
isInRange c@(x1, y1) d (Branch ce@(x2, y2) hd _ _ _ _ _)
        = case comparePoints c ce of
            Q1 -> x1 + d >= x2 - hd && y1 + d >= y2 - hd
            Q2 -> x1 - d <= x2 + hd && y1 + d >= y2 - hd
            Q3 -> x1 - d <= x2 + hd && y1 - d <= y2 + hd
            Q4 -> x1 + d >= x2 - hd && y1 - d <= y2 + hd
            
leavesInRange :: Coordinate -> Distance -> QuadTreeInternal -> [QuadTreeInternal]
leavesInRange c d leaf@(Leaf ce hd _) = [leaf]
leavesInRange c d branch@(Branch ce hd _ q1 q2 q3 q4)
        = concatMap (leavesInRange c d) $ filter (isInRange c d) [q1, q2, q3, q4]
            
findClosestPoint :: Coordinate -> QuadTreeInternal -> Coordinate
findClosestPoint c qt = let pointsInLeaf = (Set.toList . getPoints) (containingLeaf c qt)
                            closestPointInLeaf = List.minimumBy (comparing $ euclideanDistanceApprox c) pointsInLeaf
                            pointsInRange = concatMap (Set.toList . getPoints) $ leavesInRange c (euclideanDistanceApprox c closestPointInLeaf) qt
                        in List.minimumBy (comparing $ euclideanDistanceApprox c) (closestPointInLeaf:pointsInRange)
                        
data QuadTree = QuadTree Size QuadTreeInternal deriving (Eq, Show, Read)

isEmpty :: QuadTree -> Bool
isEmpty (QuadTree _ qt) = getSize qt == 0

insert :: Coordinate -> QuadTree -> QuadTree
insert c (QuadTree size qt) = QuadTree size (insertPoint size c qt)

delete :: Coordinate -> QuadTree -> QuadTree
delete c (QuadTree size qt) = QuadTree size (deletePoint size c qt)

findClosest :: Coordinate -> QuadTree -> Coordinate
findClosest c (QuadTree _ qt) = findClosestPoint c qt

size :: QuadTree -> Int
size (QuadTree _ qt) = getSize qt