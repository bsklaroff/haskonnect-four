import System.IO

data Piece = X | O deriving (Show, Eq)
data Col = Col [Piece] deriving (Eq)
data Board = Board {width :: Int, height :: Int, cols :: [Col]} deriving (Eq)
data Dir = Up | Right | RightUp | RightDown deriving (Eq)

otherPiece :: Piece -> Piece
otherPiece p = if p == X then O else X

default_board :: Board
default_board = Board {width = 7, height = 6, cols = take 7 $ repeat (Col [])}

board :: Int -> Int -> Board
board w h = Board {width = w, height = h, cols = take w $ repeat (Col [])}

boardPlay :: Piece -> Board -> Int -> Board
boardPlay p b c = Board {width = width b, height = height b, cols = f c (cols b) }
  where f k [] = []
        f 1 (Col c:cs) = if length c < height b then (Col (c ++ [p])):cs else (Col c):cs
        f k (c:cs) = c:f (k-1) cs

instance Show Board where
    show (Board _ 0 _) = []
    show (Board w h c) = concatMap (f h) c ++ "\n" ++ show (Board w (h-1) c)
      where f h (Col xs) = if h <= length xs then show (xs !! (h-1)) else "-"

gameOver :: Board -> (Bool, Piece)
gameOver (Board _ _ []) = (False, X)
gameOver b@(Board w h ((Col c):cs)) =
    let res = gameOver' b in
      if fst res then res else gameOver (Board (w-1) h cs)
    where gameOver' b@(Board w h ((Col c):cs)) = foldl (cw b c) (False, X) [(x, y) | x <- [0..length c - 1], y <- [Up, Main.Right, RightUp, RightDown]]
          cw b c res@(rb, _) (i, d) = if rb then res else checkWin b i d (c !! i) 4

checkWin :: Board -> Int -> Dir -> Piece -> Int -> (Bool, Piece)
checkWin (Board _ _ []) _ _ p _ = (False, p)
checkWin b@(Board w h ((Col c):cs)) r d p k
  | r < 0 || r >= length c = (False, p)
  | c !! r /= p = (False, p)
  | k == 1 = (True, p)
  | d == Up = checkWin b (r+1) d p (k-1)
  | d == Main.Right = checkWin (Board (w-1) h cs) r d p (k-1)
  | d == RightUp = checkWin (Board (w-1) h cs) (r+1) d p (k-1)
  | d == RightDown = checkWin (Board (w-1) h cs) (r-1) d p (k-1)

gameLoop :: Piece -> Board -> IO ()
gameLoop p b = do
    putStr ((show p) ++ " to play. Enter a number between 1 and " ++ (show $ width b) ++ ": ")
    hFlush stdout
    col <- getLine
    case reads col of
      [(intCol, "")] -> do
        let newB = boardPlay p b intCol
        putStr $ show newB
        if newB == b then do
                     putStrLn "Illegal Move"
                     gameLoop p newB
                     else do
                       putStrLn ((show p) ++ " dropped in column " ++ col)
                       let gameRes = gameOver newB
                       if fst gameRes then putStrLn ((show . snd $ gameRes) ++ " won!")
                                      else gameLoop (otherPiece p) newB
      _ -> do putStrLn "Please enter a valid integer"
              gameLoop p b

main = gameLoop X default_board
