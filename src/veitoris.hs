module Main where

import qualified Data.Array as A (Array, listArray, (!))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl', sort, zip4)
import qualified Data.Map as Map
import Text.Printf (printf)
import Beta0 (beta0)

type Point = (Double, Double, Double)

type V = Int

type Edge = (V, V)

type Tri = (V, V, V)

type Quad = (V, V, V, V)

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

r0, r1 :: Double
r0 = 1
r1 = 2

torus :: Double -> Double -> Point
torus u v =
  ( (r1 + r0 * cos u) * sin v,
    (r1 + r0 * cos u) * cos v,
    r0 * sin u
  )

ps :: [Point]
ps= [torus s t | s <- [0, 0.1 .. 2 * pi], t <- [0, 0.1 .. 2 * pi]]

distance :: Point -> Point -> Double
distance (x1, y1, z1) (x2, y2, z2) = sqrt (dx * dx + dy * dy + dz * dz)
  where
    dx = x2 - x1
    dy = y2 - y1
    dz = z2 - z1

normEdge :: Edge -> Edge
normEdge (a, b) = (min a b, max a b)

beta1 :: Double -> [Point] -> Int
beta1 eps points = dC1 - rankZ2Sparse d1Cols - rankZ2Sparse d2Cols
  where
    n :: Int
    n = length points

    points' :: A.Array Int Point
    points' = A.listArray (0, n - 1) points
    
    adj :: IM.IntMap IS.IntSet
    adj = foldl' addEdge IM.empty edges
        where
            addEdge m (u, v) =
                IM.insertWith IS.union u (IS.singleton v) $
                    IM.insertWith IS.union v (IS.singleton u) m

    neighbors :: V -> IS.IntSet
    neighbors v = IM.findWithDefault IS.empty v adj

    edges :: [Edge]
    edges = [(i, j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1], distance (points' A.! i) (points' A.! j) < eps]

    tris :: [Tri]
    tris =
        [ (i, j, k) |
            i <- [0 .. n - 1],
            j <- IS.toAscList (neighbors i),
            j > i,
            k <- IS.toAscList (IS.intersection (neighbors i) (neighbors j)),
            k > j
        ]

    quads :: [Quad]
    quads = 
        [ (i, j, k, l)
        | i <- [0 .. n - 1]
        , j <- IS.toAscList (neighbors i)
        , j > i
        , k <- IS.toAscList (IS.intersection (neighbors i) (neighbors j))
        , k > j
        , l <- IS.toAscList (IS.intersection (IS.intersection (neighbors i) (neighbors j)) (neighbors k))
        , l > k
        ]
 
    d1Cols :: [Col]
    d1Cols = [IS.fromList [u, v] | (u, v) <- edges]

    edgeIx :: Map.Map Edge Int
    edgeIx = Map.fromList (zip edges [0 ..])

    d2Cols :: [Col]
    d2Cols =
        [ IS.fromList
            [ edgeIx Map.! normEdge (a, b)
            , edgeIx Map.! normEdge (a, c)
            , edgeIx Map.! normEdge (b, c)
            ]
        | (a, b, c) <- tris
        ]

    dC1 :: Int
    dC1 = length edges 

beta2 :: Double -> [Point] -> Int
beta2 eps points = dC2 - rankZ2Sparse d2Cols - rankZ2Sparse d3Cols
  where
    n :: Int
    n = length points

    points' :: A.Array Int Point
    points' = A.listArray (0, n - 1) points
    
    adj :: IM.IntMap IS.IntSet
    adj = foldl' addEdge IM.empty edges
        where
            addEdge m (u, v) =
                IM.insertWith IS.union u (IS.singleton v) $
                    IM.insertWith IS.union v (IS.singleton u) m

    neighbors :: V -> IS.IntSet
    neighbors v = IM.findWithDefault IS.empty v adj

    edges :: [Edge]
    edges = [(i, j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1], distance (points' A.! i) (points' A.! j) < eps]

    tris :: [Tri]
    tris =
        [ (i, j, k) |
            i <- [0 .. n - 1],
            j <- IS.toAscList (neighbors i),
            j > i,
            k <- IS.toAscList (IS.intersection (neighbors i) (neighbors j)),
            k > j
        ]

    quads :: [Quad]
    quads = 
        [ (i, j, k, l)
        | i <- [0 .. n - 1]
        , j <- IS.toAscList (neighbors i)
        , j > i
        , k <- IS.toAscList (IS.intersection (neighbors i) (neighbors j))
        , k > j
        , l <- IS.toAscList (IS.intersection (IS.intersection (neighbors i) (neighbors j)) (neighbors k))
        , l > k
        ]
    
    norm3plex :: Tri -> Tri 
    norm3plex (x,y,z) = let [a,b,c] = sort [x,y,z] in (a,b,c)

    edgeIx :: Map.Map Edge Int
    edgeIx = Map.fromList (zip edges [0 ..])

    d2Cols :: [Col]
    d2Cols =
        [ IS.fromList
            [ edgeIx Map.! normEdge (a, b)
            , edgeIx Map.! normEdge (a, c)
            , edgeIx Map.! normEdge (b, c)
            ]
        | (a, b, c) <- tris
        ]
    triIx :: Map.Map Tri Int 
    triIx = Map.fromList  (zip tris [0 ..])

    d3Cols :: [Col]
    d3Cols = 
        [ IS.fromList 
            [ triIx Map.! norm3plex (a, b, c)
            , triIx Map.! norm3plex (a, b, d)
            , triIx Map.! norm3plex (a, c, d)
            , triIx Map.! norm3plex (b, c, d)
            ]
        | (a,b,c,d) <- quads
        ]
   
    dC2 :: Int 
    dC2 = length tris

type Col = IS.IntSet

xorCol :: Col -> Col -> Col
xorCol a b = IS.union (IS.difference a b) (IS.difference b a)

lead :: Col -> Maybe Int
lead c = fst <$> IS.maxView c

rankZ2Sparse :: [Col] -> Int
rankZ2Sparse cols = IM.size pivots
  where
    pivots = foldl' insertCol IM.empty cols

    insertCol :: IM.IntMap Col -> Col -> IM.IntMap Col
    insertCol p c0 = go c0
      where
        go c =
          case lead c of
            Nothing -> p
            Just l ->
              case IM.lookup l p of
                Nothing -> IM.insert l c p
                Just base -> go (xorCol c base)

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
-- Not in scope: `M.fromLists'
-- NB: no module named `M' is imported.
main :: IO ()
main = do 
  let eps = [0,0.05..0.4]
      b0s = map (`beta0` ps) eps
      b1s = map (`beta1` ps) eps
      b2s = map (`beta2` ps) eps
      strings = map (\(e,b0,b1,b2) -> printf "eps: %.2f β0 =%d β1 =%d β2 =%d" e b0 b1 b2) $ zip4 eps b0s b1s b2s
  mapM_ putStrLn strings
