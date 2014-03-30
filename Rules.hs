module Rules where

import Prelude hiding (Left, Right)
import Data.List
import Data.Maybe
import System.Random

-- Helper function
ljust, rjust :: a -> Int -> [a] -> [a]
ljust b n xs = replicate (n-length xs) b ++ xs
rjust b n xs = reverse $ ljust b n $ reverse xs

newtype Position = MkPos {unMkPos :: [[Maybe Int]]}
    deriving Eq

boardSize :: Int
boardSize = 4

instance Show Position where
    show (MkPos p) = let
        showTile Nothing = "    "
        showTile (Just x) = if x < 100
                            then ljust ' ' 3 (show x) ++ " "
                            else ljust ' ' 4 (show x)
        showLine xs = "|" ++ intercalate "|" (map showTile xs) ++ "|"
        interLine = "\n" ++
                    "|    |    |    |    |\n" ++
                    "+----+----+----+----+\n" ++
                    "|    |    |    |    |\n"
        startBoard = "+----+----+----+----+\n" ++
                     "|    |    |    |    |\n"
        endBoard = reverse startBoard
      in
        startBoard ++ intercalate interLine (map showLine p) ++ endBoard

blankPosition :: Position
blankPosition = MkPos $ replicate boardSize $ replicate boardSize Nothing

collapseLine :: [Maybe Int] -> [Maybe Int]
collapseLine xs = rjust Nothing boardSize $ map Just $ collapseInts $
    catMaybes xs
    where
    collapseInts (a:b:l) =
        if a == b
            then (a+b) : collapseInts l
            else a : collapseInts (b:l)
    collapseInts x = x

data Move = Up | Down | Left | Right

applyMove :: Move -> Position -> Position
applyMove m = let
    orientBoard = case m of
        Up -> transpose
        Down -> transpose
        _ -> id
    invOrientBoard = orientBoard
    orientLine = case m of
        Right -> reverse
        Down -> reverse
        _ -> id
    invOrientLine = orientLine
    in
    MkPos . invOrientBoard . map (invOrientLine . collapseLine . orientLine) .
        orientBoard . unMkPos

blankSpaces :: Position -> Int
blankSpaces (MkPos p) = sum $ map (sum . map (toInt . isNothing)) $ p
    where toInt b = if b then 1 else 0

addToBoard :: Int -> Int -> Position -> Position
addToBoard n i (MkPos p) = MkPos (addToBoard' n i p)
    where
        lineSize = sum.map (toInt.(==Nothing))
        toInt b = if b then 1 else 0
        addToLine n 0 (Nothing:l) = Just n : l
        addToLine n i (Nothing:l) = Nothing : addToLine n (i-1) l
        addToLine n i (Just a:l) = Just a : addToLine n i l
        addToLine n i [] = error "Impossible placement."
        addToBoard' n i (x:xs) | i < lineSize x = addToLine n (i :: Int) x : xs
                               | otherwise = x : addToBoard' n (i - lineSize x)
                                                    xs

addRandom :: RandomGen g => g -> Position -> (Maybe Position, g)
addRandom g0 p =
    let size = blankSpaces p
        (rand, g1) = randomR (0.0 :: Double, 1.0) g0
        tileNum = if rand < 0.1 then 4 else 2
    in if size == 0
            then (Nothing, g1)
            else let (i, g2) = randomR (0, size-1) g1
                in (Just $ addToBoard tileNum i p, g2)

mkInitPos :: RandomGen g => g -> (Position, g)
mkInitPos g0 = let
    (p0, g1) = randomR (0, boardSize^2-1) g0
    (p1, g2) = randomR (0, boardSize^2-2) g1
    in (addToBoard 2 p0 $ addToBoard 2 p1 $ blankPosition, g2)

legalMoveQ :: Position -> Move -> Bool
legalMoveQ p m = p /= applyMove m p
                && blankSpaces (applyMove m p) /= 0

legalMoves :: Position -> [Move]
legalMoves p = filter (legalMoveQ p) [Left, Right, Up, Down]

fullMove :: RandomGen g => g -> Move -> Position -> (Maybe Position, g)
fullMove g m p = if legalMoveQ p m
                then addRandom g $ applyMove m p
                else (Nothing, g)

game0, game1 :: RandomGen g => g -> Position -> IO ()
game0 g p = do
    print p
    if null $ legalMoves p
        then putStrLn "Game Over!"
        else game1 g p
game1 g p = do
    inp <- getLine
    let mm = case inp of
                "h" -> Just Left
                "j" -> Just Down
                "k" -> Just Up
                "l" -> Just Right
                _ -> Nothing
    maybe (do
            putStrLn "Invalid input: enter one of 'h', 'j', 'k', or 'l'."
            game1 g p)
        (\m->
            if legalMoveQ p m then do
                let (mp, ng) = fullMove g m p
                maybe (error "This is strange") (game0 ng) mp
            else do
                putStrLn "Illegal move"
                game1 g p)
        mm

main = do
    initPos <- getStdRandom mkInitPos
    g <- getStdGen
    game0 g initPos
