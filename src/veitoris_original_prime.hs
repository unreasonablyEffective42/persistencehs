module Main where

import Beta0 (beta0)
import qualified Data.Array as A (Array, listArray, (!))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl')
import qualified Data.Map as Map
import Text.Printf (printf)

type Point = (Double, Double, Double, Double, Double, Double)

type V = Int

type Edge = (V, V)

type Tri = (V, V, V)

type Quad = (V, V, V, V)

type Pent = (V, V, V, V, V)

type Col = IS.IntSet

data Complex = Complex
  { cxEdges :: [Edge],
    cxTris :: [Tri],
    cxQuads :: [Quad],
    cxPents :: [Pent],
    cxEdgeIx :: Map.Map Edge Int,
    cxTriIx :: Map.Map Tri Int,
    cxQuadIx :: Map.Map Quad Int
  }

ru, rv, rw :: Double
ru = 2
rv = 1
rw = 1

torus :: Double -> Double -> Double -> Point
torus u v w =
  ( ru * cos u,
    ru * sin u,
    rv * cos v,
    rv * sin v,
    rw * cos w,
    rw * sin w
  )

ps :: [Point]
ps = [torus r s t | r <- angles, s <- angles, t <- angles]
  where
    samples :: Int
    samples = 15
    angles = [2 * pi * fromIntegral i / fromIntegral samples | i <- [0 .. samples - 1]]

distance :: Point -> Point -> Double
distance (x1, x2, x3, x4, x5, x6) (y1, y2, y3, y4, y5, y6) = sqrt (dx1 * dx1 + dx2 * dx2 + dx3 * dx3 + dx4 * dx4 + dx5 * dx5 + dx6 * dx6)
  where
    dx1 = y1 - x1
    dx2 = y2 - x2
    dx3 = y3 - x3
    dx4 = y4 - x4
    dx5 = y5 - x5
    dx6 = y6 - x6

normEdge :: Edge -> Edge
normEdge (a, b) = (min a b, max a b)

complexAt :: Double -> [Point] -> Complex
complexAt eps points =
  Complex
    { cxEdges = edges,
      cxTris = tris,
      cxQuads = quads,
      cxPents = pents,
      cxEdgeIx = Map.fromList (zip edges [0 ..]),
      cxTriIx = Map.fromList (zip tris [0 ..]),
      cxQuadIx = Map.fromList (zip quads [0 ..])
    }
  where
    n :: Int
    n = length points

    points' :: A.Array Int Point
    points' = A.listArray (0, n - 1) points

    edges :: [Edge]
    edges = [(i, j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1], distance (points' A.! i) (points' A.! j) <= eps]

    adj :: IM.IntMap IS.IntSet
    adj = foldl' addEdge IM.empty edges
      where
        addEdge m (u, v) =
          IM.insertWith IS.union u (IS.singleton v) $
            IM.insertWith IS.union v (IS.singleton u) m

    neighbors :: V -> IS.IntSet
    neighbors v = IM.findWithDefault IS.empty v adj

    tris :: [Tri]
    tris =
      [ (i, j, k)
      | i <- [0 .. n - 1],
        j <- IS.toAscList (neighbors i),
        j > i,
        k <- IS.toAscList (IS.intersection (neighbors i) (neighbors j)),
        k > j
      ]

    quads :: [Quad]
    quads =
      [ (i, j, k, l)
      | i <- [0 .. n - 1],
        j <- IS.toAscList (neighbors i),
        j > i,
        k <- IS.toAscList (IS.intersection (neighbors i) (neighbors j)),
        k > j,
        l <- IS.toAscList (IS.intersection (IS.intersection (neighbors i) (neighbors j)) (neighbors k)),
        l > k
      ]

    pents :: [Pent]
    pents =
      [ (i, j, k, l, m)
      | i <- [0 .. n - 1],
        j <- IS.toAscList (neighbors i),
        j > i,
        k <- IS.toAscList (IS.intersection (neighbors i) (neighbors j)),
        k > j,
        l <- IS.toAscList (IS.intersection (IS.intersection (neighbors i) (neighbors j)) (neighbors k)),
        l > k,
        m <- IS.toAscList (IS.intersection (IS.intersection (IS.intersection (neighbors i) (neighbors j)) (neighbors k)) (neighbors l)),
        m > l
      ]

d1Cols :: Complex -> [Col]
d1Cols cx = [IS.fromList [u, v] | (u, v) <- cxEdges cx]

d2Cols :: Complex -> [Col]
d2Cols cx =
  [ IS.fromList
      [ ix Map.! normEdge (a, b),
        ix Map.! normEdge (a, c),
        ix Map.! normEdge (b, c)
      ]
  | (a, b, c) <- cxTris cx
  ]
  where
    ix = cxEdgeIx cx

d3Cols :: Complex -> [Col]
d3Cols cx =
  [ IS.fromList
      [ ix Map.! (a, b, c),
        ix Map.! (a, b, d),
        ix Map.! (a, c, d),
        ix Map.! (b, c, d)
      ]
  | (a, b, c, d) <- cxQuads cx
  ]
  where
    ix = cxTriIx cx

d4Cols :: Complex -> [Col]
d4Cols cx =
  [ IS.fromList
      [ ix Map.! (a, b, c, d),
        ix Map.! (a, b, c, e),
        ix Map.! (a, b, d, e),
        ix Map.! (a, c, d, e),
        ix Map.! (b, c, d, e)
      ]
  | (a, b, c, d, e) <- cxPents cx
  ]
  where
    ix = cxQuadIx cx

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

beta1From :: Complex -> Int
beta1From cx = length (cxEdges cx) - rankZ2Sparse (d1Cols cx) - rankZ2Sparse (d2Cols cx)

beta2From :: Complex -> Int
beta2From cx = length (cxTris cx) - rankZ2Sparse (d2Cols cx) - rankZ2Sparse (d3Cols cx)

beta3From :: Complex -> Int
beta3From cx = length (cxQuads cx) - rankZ2Sparse (d3Cols cx) - rankZ2Sparse (d4Cols cx)

beta1 :: Double -> [Point] -> Int
beta1 eps points = beta1From (complexAt eps points)

beta2 :: Double -> [Point] -> Int
beta2 eps points = beta2From (complexAt eps points)

beta3 :: Double -> [Point] -> Int
beta3 eps points = beta3From (complexAt eps points)

main :: IO ()
main = do
  let eps = [0, 0.05 .. 1.1]
      row e =
        let cx = complexAt e ps
         in (e, beta0 e ps, beta1From cx, beta2From cx, beta3From cx)
      rows = map row eps
      sep = replicate 54 '-'
  putStrLn sep
  putStrLn $ printf "| %6s | %8s | %8s | %8s | %8s |" ("ε" :: String) ("β0" :: String) ("β1" :: String) ("β2" :: String) ("β3" :: String)
  putStrLn sep
  mapM_ (\(e, b0, b1, b2, b3) -> putStrLn $ printf "| %6.2f | %8d | %8d | %8d | %8d |" e b0 b1 b2 b3) rows
  putStrLn sep
