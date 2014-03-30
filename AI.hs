module AI where

import Prelude hiding (Left, Right)
import System.IO.Unsafe
import Data.List
import Data.Maybe
import System.Random
import Control.Monad.State
import Rules

type Rnd = State StdGen

instance Random Move where
    random g = let (i, ng) = randomR (0,3) g
        in ([Up, Down, Left, Right] !! i, ng)

type Strategy = StdGen -> Position -> (Move, StdGen)

moveRandom :: Strategy
moveRandom g p = random g
{-moveRandom g0 p = let
    moves = legalMoves p
    (i, g1) = randomR (0, length moves-1) g0
    in if null moves then (Up, g0) else (moves !! i, g1)
--}

-- proxyFullMove: Like fullMove, except that you are allowed to make a move that
-- doesn't change the position of any tile.
-- Justification: The amount of proxy moves until you randomly make an illegal
-- move should be a better measure of how good a position is than with regular
-- moves, since it doesn't give penalties for monotonicly tiled positions.
proxyFullMove :: RandomGen g => g -> Move -> Position -> (Maybe Position, g)
proxyFullMove g m p = addRandom g $ applyMove m p

monteCarloTrial :: Strategy -> StdGen -> Position -> (Int, StdGen)
monteCarloTrial s g0 p =
    let (m, g1) = s g0 p
        (mp, g2) = proxyFullMove g1 m p
    in case mp of
        Nothing -> (0, g2)
        Just p' -> let
            (i, g3) = monteCarloTrial s g2 p'
            in (1+i, g3)

sum' = sumAcc 0
    where
        sumAcc a [] = a
        sumAcc a (i:xs) = sumAcc (a+i) xs

-- Precondition: Whenever the function is called, m is a legal move in position
-- p.
monteCarloRating :: Int -> Position -> Move -> Rnd Double
monteCarloRating n p m = state $ \g-> let (s, gn) = sampleN n g in (avg s, gn)
    where
        avg l = sum (map fromIntegral l) / fromIntegral (length l)
        sampleN 0 g = ([], g)
        sampleN n g0 = let
            (mp', g1) = fullMove g0 m p
            p' = case mp' of {Just a -> a; _ -> error "fromJust in monteCarlo";}
            (s0, g2) = monteCarloTrial moveRandom g1 p'
            (ss, g3) = sampleN (n-1) g2
            in
            (s0:ss, g3)

isFull = all (all isJust) . unMkPos

ratingToStrategy :: (Position -> Move -> Rnd Double) -> Strategy
ratingToStrategy f g0 p | null $ legalMoves p = ({-Give-} Up, g0)
                        | otherwise = let
    moves = legalMoves p
    (ratings, g1) = ratings' g0 moves
    ratings' g [] = ([], g)
    ratings' tg0 (m:ms) = let
        (r, tg1) = runState (f p m) tg0
        (rs, tg2) = ratings' tg1 ms
        in
        (r:rs, tg2)
    m = maximum ratings
    i = case elemIndex m ratings of {Just a-> a; _ -> error (show ratings);}
    in unsafePerformIO (print ratings) `seq`
        (moves !! i, g1)

rateWithLookahead :: (Position -> Move -> Rnd Double) -> Position -> Move ->
    Rnd Double
rateWithLookahead f p m = let
    p' = applyMove m p
    n = blankSpaces p'
    in if n == 0 then return (-10000) else do
    p2s <- mapM ratePosition $ map (\i-> addToBoard 2 i p') [0..n-1]
    --unsafePerformIO (print p2s) `seq` return ()
    p4s <- mapM ratePosition $ map (\i-> addToBoard 4 i p') [0..n-1]
    return $ (1/fromIntegral n) * (0.1 * sum p4s + 0.9 * sum p2s)
    --return $ minimum (p2s ++ p4s)
    where
    ratePosition :: Position -> Rnd Double
    ratePosition p' = do
        let moves = legalMoves p'
        if null moves then return (-10000) else do
            l <- mapM (monteCarloRating 20 p') moves
            --unsafePerformIO (print l) `seq` return ()
            return (maximum l)

showAIGameFromPos :: Strategy -> Position -> IO ()
showAIGameFromPos s p = do
    print p
    --getLine
    m <- getStdRandom (flip s p)
    mp <- getStdRandom (\g -> fullMove g m p)
    maybe (do
        putStrLn "Game over: The AI lost")
        (showAIGameFromPos s)
        mp

showAIGame :: Strategy -> IO ()
showAIGame s = getStdRandom mkInitPos >>= showAIGameFromPos s

main = showAIGame mainStrategy
    where
        mainStrategy = ratingToStrategy mainRating
        mainRating = rateWithLookahead (monteCarloRating 20)
        --mainRating = monteCarloRating 100
