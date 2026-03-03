module Main where

import qualified Data.Array as A (Array, listArray, (!))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (findIndex, transpose)
import qualified Data.Map as Map
import qualified Data.Matrix as M
import qualified Data.Vector as Vec

type Point = (Double, Double, Double)

type V = Int

type Edge = (V, V)

type Tri = (V, V, V)

type Row = [Z2]

type Mat = [Row]

newtype Z2 = Z2 Bool
  deriving (Eq)

instance Show Z2 where
  show (Z2 False) = "0"
  show (Z2 True) = "1"

instance Num Z2 where
  (Z2 x) + (Z2 y) = Z2 (x /= y)
  (Z2 x) * (Z2 y) = Z2 (x && y)
  negate x = x
  abs x = x
  signum x = x
  fromInteger z = Z2 (odd z)

eps :: Double
eps = 1

r0, r1 :: Double
r0 = 1
r1 = 2

torus :: Double -> Double -> Point
torus u v =
  ( (r1 + r0 * cos u) * sin v,
    (r1 + r0 * cos u) * cos v,
    r0 * sin u
  )

points :: [Point]
points = [torus s t | s <- [0, 0.2 .. 2 * pi], t <- [0, 0.2 .. 2 * pi]]

n :: Int
n = length points

points' :: A.Array Int Point
points' = A.listArray (0, n - 1) points

distance :: Point -> Point -> Double
distance (x1, y1, z1) (x2, y2, z2) = sqrt (dx * dx + dy * dy + dz * dz)
  where
    dx = x2 - x1
    dy = y2 - y1
    dz = z2 - z1

adj :: IM.IntMap IS.IntSet
adj = foldl' addEdge IM.empty edges
  where
    addEdge m (u, v) =
      IM.insertWith IS.union u (IS.singleton v) $
        IM.insertWith IS.union v (IS.singleton u) m

neighbors :: V -> IS.IntSet
neighbors v = IM.findWithDefault IS.empty v adj

vertices :: [Int]
vertices = [0 .. n - 1]

vertices' :: Map.Map Point Int
vertices' = foldl' (\m v -> Map.insert (points' A.! v) v m) Map.empty vertices

edges :: [Edge]
edges = [(i, j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1], distance (points' A.! i) (points' A.! j) < eps]

tris :: [Tri]
tris =
  [ (i, j, k)
  | i <- [0 .. n - 1],
    j <- IS.toAscList (neighbors i),
    j > i,
    k <- IS.toAscList (IS.intersection (neighbors i) (neighbors j)),
    k > j
  ]

v2plex :: Edge -> [Z2]
v2plex (p, q) = [Z2 ((v == p) /= (v == q)) | v <- vertices]

normEdge :: Edge -> Edge
normEdge (a, b) = (min a b, max a b)

v3plex :: Tri -> [Z2]
v3plex (p, q, l) = [Z2 (e == normEdge (p, q) || e == normEdge (p, l) || e == normEdge (q, l)) | e <- edges]

boundary1 :: Map.Map Edge [Z2]
boundary1 = foldl' (\m e -> Map.insert e (v2plex e) m) Map.empty edges

boundary2 :: Map.Map Tri [Z2]
boundary2 = foldl' (\m t -> Map.insert t (v3plex t) m) Map.empty tris

d1 :: M.Matrix Z2
d1 = transpose [boundary1 Map.! e | e <- edges]

d2 :: M.Matrix Z2
d2 = M.transpose $ M.fromLists [boundary2 Map.! t | t <- tris]

-- findPivot :: Vec.Vector Z2 -> Int
-- findPivot v = case Vec.findIndex (== 1) v of
--  (Just i) -> i
--  Nothing -> length v

swapAt :: Int -> Int -> [a] -> [a]
swapAt i j xs
  | i == j = xs
  | otherwise = [pick k x | (k, x) <- zip [0 ..] xs]
  where
    xi = xs !! i
    xj = xs !! j
    pick k x
      | k == i = xj
      | k == j = xi
      | otherwise = x

rankZ2 :: Mat -> Int
rankZ2 m0 = go 0 0 m0
  where
    nRows = length m0
    nCols = if null m0 then 0 else length (head m0)
    go r c m
      | r >= nRows || c >= nCols = r
      | otherwise =
          case findPivot r c m of
            Nothing -> go r (c + 1) m
            Just p ->
              let m1 = swapAt r p m
                  pivot = m1 !! r
                  m2 =
                    [ if i /= r && (row !! c)
                        then zipWith (+) row pivot
                        else row
                    | (i, row) <- zip [0 ..] m1
                    ]
               in go (r + 1) (c + 1) m2

    findPivot r c m =
      (+ r) <$> findIndex (\row -> row !! c) (drop r m)

-- rref :: M.Matrix Z2 -> M.Matrix Z2

-- where r = nrows M
--       c = ncols M

-- vertices a b c d e
-- edges ab bc ac cd de ce
-- tris abc cde
--
-- ∂1 = ab bc ac cd de ce
--     a 1  0  1  0  0  0
--     b 1  1  0  0  0  0
--     c 0  1  1  1  0  1
--     d 0  0  0  1  1  0
--     e 0  0  0  0  1  1
--
-- ∂2 =
--    abc  cde
--  ab 1    0
--  bc 1    0
--  ac 1    0
--  cd 0    1
--  de 0    1
--  ce 0    1
--
--  >>> a = M.fromLists [[1,1,1],[2,2,2],[3,3,3]]
--  >>>M.getMatrixAsVector a
-- [1,1,1,2,2,2,3,3,3]
main :: IO ()
main = putStrLn "done"
