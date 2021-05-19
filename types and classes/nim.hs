-- Initial board status
-- * * * * * 
-- * * * * 
-- * * *
-- * *
-- *

type Board = [Int]

original_board :: Board
original_board = [5, 4, 3, 2, 1]

-- prints the board status in form of stars, 
-- where each number in list represents number of stars in that row
print_board :: Board -> IO ()
print_board [] = do putChar '\0'
print_board (row:board) = do putStrLn(concat (replicate row "* "))
                             print_board board

-- removes `rem` stars from `row`th row in `board`
performOP :: Board -> Int -> Int -> Board
performOP board row rem = [if i==row then (v - rem) else v| (i, v)<-(zip[1..] board)]

playplayer  :: Int -> Board -> IO ()
playplayer playerID board =  do putStr "Player "
                                putStr (show playerID)
                                putStrLn " Enter the row and number of stars you wish to remove: "
                                (row, rem) <- getRowandCnt
                                putStrLn (show (row, rem))
                                if validateRowandCnt board row rem then
                                        do let newBoard = performOP board row rem
                                           print_board newBoard
                                           if checkIfWon newBoard then
                                                do putStr "Congratulations! Player "
                                                   putStr (show playerID)
                                                   putStrLn " won"
                                                   return ()
                                           else if playerID == 1 then
                                                do playplayer 2 newBoard
                                           else 
                                                do playplayer 1 newBoard
                                else
                                        do putStrLn "Wrong Moves, Enter the row and remove cnt once more:"
                                           playplayer playerID board


getRowandCnt :: IO (Int, Int)
getRowandCnt = do row <- getChar
                  getChar
                  cnt <- getChar
                  getChar
                  return (read [row], read [cnt])

validateRowandCnt :: Board -> Int -> Int -> Bool
validateRowandCnt board row cnt = (row > 0) && (row < (length board) + 1) && (cnt > 0) && ((board !! (row - 1))>= cnt)

checkIfWon :: Board -> Bool
checkIfWon = all (== 0)

nim :: Board -> IO ()
nim board =  do putStrLn "Which player would like to go first?(1/2)"
                option <- getChar
                getChar
                if option == '1' then 
                    do playplayer 1 board
                else 
                    do playplayer 2 board


main :: IO()
main = do print_board original_board
          nim original_board
          putStr "Do you want to play again? (y/n)"
          option <- getChar
          getChar
          if option == 'y' then 
              main
          else 
              do putStrLn "Thank you for playing the game!!"
                 return ()