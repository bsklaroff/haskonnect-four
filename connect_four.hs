import System.IO

data Piece = X | O deriving (Show, Eq)
data Col = Col [Piece] deriving (Eq)
data Board = Board {width :: Int, height :: Int, cols :: [Col]} deriving (Eq)

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

gameLoop :: Piece -> Board -> IO ()
gameLoop p b = do
    putStr ((show p) ++ " to play. Enter a number between 1 and " ++ (show $ width b) ++ ": ")
    hFlush stdout
    col <- getLine
    case reads col of
      [(intCol, "")] -> let newB = boardPlay p b intCol in do
        putStr $ show newB
        if newB == b then do
                     putStrLn "Illegal Move"
                     gameLoop p newB
                     else do
                       putStrLn ((show p) ++ " dropped in column " ++ col)
                       gameLoop (otherPiece p) newB
      _ -> do putStrLn "Please enter a valid integer"
              gameLoop p b

main = gameLoop X default_board
