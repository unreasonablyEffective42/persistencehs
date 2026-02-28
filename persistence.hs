import Data.List (sortOn, tails)
import Data.Map qualified as Map

type Point = (Double, Double)

type Edge = (Point, Point)

data DSU = DSU
  { parent :: Map.Map Point Point,
    size :: Map.Map Point Int
  }

find :: DSU -> Point -> (Maybe Point, DSU)
find dsu p =
  case Map.lookup p (parent dsu) of
    Nothing -> (Nothing, dsu)
    Just par ->
      if par == p
        then (Just p, dsu)
        else
          let (rootM, dsu') = find dsu par
              dsu'' = case rootM of
                Nothing -> dsu'
                Just root -> dsu' {parent = Map.insert p root (parent dsu')}
           in (rootM, dsu'')

union :: Point -> Point -> DSU -> DSU
union p q dsu =
  case (rp, rq) of
    (Just rp, Just rq) ->
      if rp == rq
        then dsu2
        else dsu2 {parent = Map.insert rq rp (parent dsu2)}
    (Nothing, Just rq) -> dsu
    (Just rp, Nothing) -> dsu
    (Nothing, Nothing) -> dsu
  where
    (rp, dsu1) = find dsu p
    (rq, dsu2) = find dsu1 q

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt (dx * dx + dy * dy)
  where
    dx = x2 - x1
    dy = y2 - y1

uniquePairs :: [a] -> [(a, a)]
uniquePairs xs = [(x, y) | (x : rest) <- tails xs, y <- rest]

edgesWithValue :: [Point] -> [(Double, Edge)]
edgesWithValue pts = sortOn fst [(distance a b, (a, b)) | (a, b) <- uniquePairs pts]

edgesAtScale :: Double -> [(Double, Edge)] -> [(Double, Edge)]
edgesAtScale eps = filter (\(d, _) -> d <= eps)

h0sAtScales :: [Double] -> [Point] -> [(Double, Int)]
h0sAtScales epses pts = map (\eps -> (eps, length (edgesAtScale eps $ edgesWithValue pts))) epses

pointcloud :: [Point]
pointcloud =
  [ (1, 1.1),
    (1, 2),
    (1.5, 1.5),
    (2, 1.5),
    (2.25, 2.55),
    (1.5, 3.25),
    (2.5, 2),
    (1, 3.5),
    (1.5, 4),
    (2, 3.6),
    (1.5, 2.7),
    (0, 0)
  ]

roots :: DSU -> [Point] -> [Maybe Point]
roots dsu pts = map (fst . find dsu) pts

components :: DSU -> [Point] -> Map.Map Point [Point]
components dsu pts = Map.fromListWith (++) [(r, [p]) | p <- pts, Just r <- [fst (find dsu p)]]

dsu0 :: DSU
dsu0 = DSU {parent = Map.empty, size = Map.empty}

dsu1 = foldl (\dsu p -> dsu {parent = Map.insert p p (parent dsu)}) dsu0 pointcloud

dsu2 = foldl (\dsu (_, (p1, p2)) -> union p1 p2 dsu) dsu1 $ edgesAtScale 1.4 $ edgesWithValue pointcloud

-- >>>components dsu2 pointcloud
-- fromList [((0.0,0.0),[(0.0,0.0)]),((1.0,2.0),[(1.5,2.7),(2.0,3.6),(1.5,4.0),(1.0,3.5),(2.5,2.0),(1.5,3.25),(2.25,2.55),(2.0,1.5),(1.5,1.5),(1.0,2.0),(1.0,1.1)])]
-- >>> Map.size (components dsu2 pointcloud)
-- 2
