--All units SI base units
import Control.Monad (replicateM)
import Data.List (insert)
import System.IO
import System.Random

data Point = Point {x :: Double, y :: Double} deriving (Eq, Ord, Show)
data PointMass = PointMass {pos :: Point, vel :: Point} deriving (Eq, Ord, Show)

instance Num Point where
    (Point x y) + (Point x' y') = Point (x+x') (y+y')
    negate (Point x y) = Point (negate x) (negate y)
    (Point x y) * (Point x' y') = Point (x*x') (y*y')
    abs (Point x y) = Point (abs x) (abs y)
    signum (Point x y) = Point (signum x) (signum y)
    fromInteger n = Point (fromInteger n) (fromInteger n)

scale :: Double -> Point -> Point
scale s (Point x y) = Point (s*x) (s*y)

g = 6.67384e-11

step :: Double -> Double -> [PointMass] -> [PointMass]
step time gmassmass ps = fmap go ps
    where
        go (PointMass pos vel) = PointMass (pos+(scale time vel)) (vel+(scale time (sum (fmap (accel pos) ps))))
        accel p@(Point x y) (PointMass p'@(Point x' y') _) = let unit = scale (recip rad2) (p - p')
                                                                 rad2 = (x-x')^2 + (y-y')^2
                                                             in if p == p' then 0 else negate $ scale (gmassmass / rad2) unit

main = do
    hSetBuffering stdout NoBuffering
    putStr "Mass (kg) "
    mass <- readLn
    putStr "Quantity (even) "
    qty <- readLn
    putStr "FrameTime (s) "
    time <- readLn
    putStr "Max x and y position (m) "
    posMax <- readLn
    putStr "Max x and y velocity (m/s) "
    velMax <- readLn
    exportTimes <- let loop = do
                            putStr "Time For Export (s) (0 for no more) "
                            time <- readLn
                            if time == 0
                                then return []
                                else fmap (insert time) loop
                    in loop
    go mass time posMax velMax exportTimes qty

go :: Double -> Double -> Double -> Double -> [Double] -> Int -> IO ()
go mass time posMax velMax exportTimes qty = do
    points <- fmap concat $ replicateM (qty `div` 2) randPoints
    let frames = iterate (step time (g*mass*mass)) points
    let ourFrames = map (\t -> (t,(frames !!) . round . (/time)) exportTimes
    forM_ exportTimes $ \t -> do
        writeFile (show t) (format (frames !! (round $ t/time)))
        putStrLn $ "Done with " ++ (show t)
        where
            randPoints = do
                [x, y, x', y'] <- replicateM 4 (randomRIO (-posMax, posMax))
                [dx, dy] <- replicateM 2 (randomRIO (-velMax, velMax))
                return [PointMass (Point x y) (Point dx dy), PointMass (Point x' y') (Point (negate dx) (negate dy))]

format :: [PointMass] -> String
format = unlines $ map (\(PointMass (Point x y) _) -> (show x) ++ ',' ++ (show y))