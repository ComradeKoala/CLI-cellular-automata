--Jørgen Lohne, Group 1
-- 10 (s 0 0, b 2 2) 1 1 4 1 2 2 3 2

import Data.List
import Data.Char 
import System.Directory
import Control.Concurrent

--Oblig 2


--------
--MAIN--
--------

main = do --starts program and deals with initialastion
    putStrLn "cmds avialable: 'c <n>' to create board, 'quit' to exit" --TODO
    putStr "Command: "
    input <- getUsedInput

    if evalCreate (parseInput input) then do --Create Board from terminal
        
        drawBoard (Size (createToInt (parseInput input)))
        gameLoop (createToInt (parseInput input)) [] (2,2) (0,0) 0

    
    else if evalRulesFromFile (parseInput input) then do --load from file
        let filename = fileToFileName (parseInput input) --TODO check if file exists and is of correct format
        exists <- doesFileExist filename
        
        if not exists then do
            putStr "No such file / incorrect file format (r command format: r filename)"
            main
            
        else do --if file exist
            putStr "Try after initalising a board"
            main
            {-
            fileContet <- readFile filename 
            --extract String from IO String
              

            if 'b' `elem`  (take 3 $ drop 3 fileContet) && 's' `elem` (drop 10 $ take 3 fileContet) then --Checks if format is (b x y, s a b)
                if 'b' `elem`  (take 3 $ drop 3 fileContet) do --checks if size <10
                    let tuple =  take 14 $ drop 2 fileContet --extracts tuple
                        size = take 2 fileContet 
                        coords = cnv (map read [drop 16 fileContet]) in
                    --render board
                    gameLoop (read size) coords (read (drop 5 $ take 1 tuple),read (drop 7 $ take 1 tuple) (read (drop 12 $ take 1 tuple),read (drop 14 $ take 1 tuple) 0
                else do
                    let tuple =  take 14 $ drop 2 fileContet --extracts tuple
                    let size = take 2 fileContet 
                    let coords = cnv (map read [drop 16 fileContet])
                    gameLoop (read size) coords (read (drop 5 $ take 1 tuple),read (drop 7 $ take 1 tuple) (read (drop 12 $ take 1 tuple),read (drop 14 $ take 1 tuple) 0

                --Render board 
                --n (LivingCells (read size))
                --drawBoard (LivingCells (read size))
                --gameLoop size coords (read (drop 5 $ take 1 tuple),read (drop 7 $ take 1 tuple) (read (drop 12 $ take 1 tuple),read (drop 14 $ take 1 tuple) 0 --check size | tupple only works for size < 10
                
            
            --else if 's' `elem` (take 3 $ drop 3 fileContet) && 'b' `elem` (drop 10 $ take 3 fileContet) then
              
            else do
                putStr "incorrect file format"
                main
        -}

    else if parseInput input == Quit then --Quit program
        putStrLn "Exited program"

    else
        main



gameLoop :: Int -> [(Int, Int)] -> (Int, Int) -> (Int, Int) -> Int -> IO ()
gameLoop boardsize livingcells birthThreshold survivalThreshold liveMode = do

    if liveMode > 0 then  --livemode Handler
        if not (sort livingcells == sort ((survivingCells livingcells livingcells survivalThreshold) ++ survivingCells ((allboardCoords boardsize) \\ livingcells) livingcells birthThreshold)) then do
            
            --UI--
            goto(7, boardsize + 3)
            putStr "\ESC[K"
            putStr "generations remaining: "
            putStr (show (liveMode -1))

            --Render board
            e (LivingCells ((allboardCoords boardsize)  \\ ((survivingCells livingcells livingcells survivalThreshold) ++ survivingCells ((allboardCoords boardsize) \\ livingcells) livingcells birthThreshold))) --All dead cells = allcoords - cellesAliveNextGen
            n (LivingCells ((survivingCells livingcells livingcells survivalThreshold) ++ survivingCells ((allboardCoords boardsize) \\ livingcells) livingcells birthThreshold))
            threadDelay 500000
            gameLoop boardsize ((survivingCells livingcells livingcells survivalThreshold) ++ survivingCells ((allboardCoords boardsize) \\ livingcells) livingcells birthThreshold) birthThreshold survivalThreshold (liveMode -1)
       
        else do --A stable generation has been reached
            goto(7, boardsize + 3)
            putStr "\ESC[K"
            putStr "a stable generation has been reached"
            gameLoop boardsize livingcells birthThreshold survivalThreshold 0


    else do
        
        --User info, GUI
        goto (0, boardsize + 3)
        putStr "Info: "
        goto (0, boardsize + 4)
        putStr "Command: "
        putStr "\ESC[K"
        input <- getUsedInput

        if input == "debug" then do --debuger
            putStr "cells that will survive: "
            print (survivingCells livingcells livingcells survivalThreshold)
            putStr "cells that will be born: "
            print (survivingCells ((allboardCoords boardsize) \\ livingcells) livingcells birthThreshold)
            gameLoop boardsize livingcells birthThreshold survivalThreshold 0

        else if input == "" then do --performs one turn

            --A stable generation is reached if the curent living cells are the same as the living cells in the next generetion
            if sort livingcells == sort ((survivingCells livingcells livingcells survivalThreshold) ++ survivingCells ((allboardCoords boardsize) \\ livingcells) livingcells birthThreshold) then do
                goto(7, boardsize + 3)
                putStr "\ESC[K"
                putStrLn "a stable generation has been reached"
                gameLoop boardsize livingcells birthThreshold survivalThreshold 0
                
            else do
                --Render board
                e (LivingCells ((allboardCoords boardsize)  \\ ((survivingCells livingcells livingcells survivalThreshold) ++ survivingCells ((allboardCoords boardsize) \\ livingcells) livingcells birthThreshold))) --All dead cells = allcoords - cellesAliveNextGen
                n (LivingCells ((survivingCells livingcells livingcells survivalThreshold) ++ survivingCells ((allboardCoords boardsize) \\ livingcells) livingcells birthThreshold))
                gameLoop boardsize ((survivingCells livingcells livingcells survivalThreshold) ++ survivingCells ((allboardCoords boardsize) \\ livingcells) livingcells birthThreshold) birthThreshold survivalThreshold 0

        else if evalNew (parseInput input) then do --Add cells
            n (drawCells (parseInput input))
            gameLoop boardsize (livingcells ++ newToList (parseInput input)) birthThreshold survivalThreshold 0

        else if evalEmpty (parseInput input) then do --Remove cells
            e (drawCells (parseInput input)) 
            gameLoop boardsize (removeLivingCells livingcells (emptyToList (parseInput input))) birthThreshold survivalThreshold 0

        else if evalBorn (parseInput input) then do --Change threshold for Cells born
            goto(7, boardsize + 3)
            putStr "\ESC[K"
            print "changed birth threshold" 
            gameLoop boardsize livingcells (fst (bornToTup (parseInput input)), snd ((bornToTup (parseInput input)))) survivalThreshold 0

        else if evalSurvie (parseInput input) then do --Change threshold for Cells that survive
            goto(7, boardsize + 3)
            putStr "\ESC[K"
            print "changed survivel threshold" 
            gameLoop boardsize livingcells birthThreshold (fst (surviveToTup (parseInput input)), snd ((surviveToTup (parseInput input)))) 0

        else if parseInput input == Help then do --Show current rules
            goto(7, boardsize + 3)
            putStr "\ESC[K"
            putStr "Rules: b "
            putStr (show birthThreshold)
            putStr ", s "
            putStr (show survivalThreshold)
            gameLoop boardsize livingcells birthThreshold survivalThreshold 0

        else if evalLive (parseInput input) then do --Live mode
            goto(7, boardsize + 3)
            putStr "\ESC[K"
            putStr "entered live mode: "
            gameLoop boardsize livingcells birthThreshold survivalThreshold (liveToInt (parseInput input))

        else if evalRulesFromFile (parseInput input) then do --TODO Show rules from file
            goto(7, boardsize + 3)
            putStr "\ESC[K"
            let filename = fileToFileName (parseInput input) --TODO check if file exists and is of correct format
            exists <- doesFileExist filename
            if not exists then do
                putStr "No such file / incorrect file format (r command format: r filename)"
                gameLoop boardsize livingcells birthThreshold survivalThreshold 0
            else do
                fileContet <- readFile filename
                putStr "Rules in "
                putStr filename
                putStr ": "
                putStr (take 14 $ drop 2 fileContet) --prints the tuple
                gameLoop boardsize livingcells birthThreshold survivalThreshold 0




        else if (parseInput input) == Quit then --Quit program
            putStrLn "Exited program"

        else if (parseInput input) == GoToMain then do --Tell user they input something invalid
            goto(7, boardsize + 3)
            putStr "\ESC[K"
            putStrLn "not a valid/incorrect us of command"
            gameLoop boardsize livingcells birthThreshold survivalThreshold 0

        else if (parseInput input) == WriteLiving then do --Prints out living cells to user
            goto (7, boardsize + 3)
            putStr "\ESC[K"
            putStr ("livinng cells: " ++ (concat (map show livingcells)))
            gameLoop boardsize livingcells birthThreshold survivalThreshold 0

        else do
            goto(7, boardsize + 3)
            putStr "\ESC[K"
            putStrLn "not a valid command"
            gameLoop boardsize livingcells birthThreshold survivalThreshold 0



{--------
--Helpers
---------}

getUsedInput :: IO String
getUsedInput = do 
    xs <- getLine
    return xs


removeLivingCells :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
removeLivingCells from rem = from \\ rem


{--------------------
User command handlers
---------------------}

--c n – create and show a new, empty board of size n × n.
--UpdateBoard | move to logic? make sure it updates board and renders new board
c :: Int -> IO ()
c x = do
    drawBoard (Size x)



--Visualy adds livingcells to board
n :: Board -> IO ()
n (LivingCells x) = do 
    saveCurPos
    place x "O"
    loadCurPos



--Visulay removes cells from board
e (LivingCells x) = do
    saveCurPos
    place x "."
    loadCurPos


--Helper -convert input of coords to coords
cnv :: [Int] -> [(Int, Int)]
cnv [] = []
cnv [x] = []
cnv (k:v:t) = (k,v) : cnv t

---------
--BOARD--
---------
--Board from lecture with improvemnts

data Board
    = Size Int
    | LivingCells [(Int, Int)]
    deriving (Eq, Ord, Show, Read)


-- creates board of size x*x
drawBoard :: Board -> IO ()
drawBoard (Size x) = do
    clr
    writeTop x
    mapM_ (\i -> writeTop x) [1..x]
    return ()
    wr 1 x


--Takes a list of coords and places string a at each coord
place :: [(Int, Int)] -> String -> IO ()
place [] _ = return ()
place (x:xs) a = do
    writeAt (coordConverter (x)) a
    place xs a



getCellType (x,y) = do
     goto (coordConverter(x,y))
     getChar 



{---------------
Helper functions
----------------}


--save current cursor postion
saveCurPos :: IO ()
saveCurPos = putStr "\ESC[s"

--Load cursor postion
loadCurPos :: IO ()
loadCurPos = putStr "\ESC[u"



--Clear screen
clr :: IO ()
clr = putStr "\ESC[2J"


--Moves cursor to coordinates x,y
goto :: (Int, Int) -> IO ()
goto (x,y) = putStr("\ESC["++ show y ++ ";" ++ show x ++ "H") 


--Writes top row 
writeTop x = 
     writeAt (lft + 2, 0)   (concat  [(show (i))++ "  " | i <- takeWhile (<10) [1..x]]  ++ (concat [(show(j))++ " "  | j <- takeWhile (>9) [10..x]]) ++ "\n")



wr :: Int -> Int -> IO ()
wr n x = if n == (x+1) then return ()
         else do writeRow n x
                 wr (n+1) x


lft :: Int
lft = 3


writeAt :: (Int, Int) -> String -> IO ()
writeAt (x,y) xs = do
    goto (x,y)
    putStr xs


writeRow :: Int -> Int -> IO ()
writeRow i x = do
    writeAt (if i > 9 then (lft -2) else lft -1, 1+i) (show i)
    putStrLn (concat ( take x (repeat "  .")))


--Flip x and y?
coordConverter :: (Int, Int) -> (Int, Int)
coordConverter (x,y) = (xoffset + 5, y + yoffset)
    where 
        xoffset = if x > 1 then (3 * (x-1)) else 0
        yoffset = 1 




----------
--PARSER--
----------

data Command
  = Create Int 
  | New [(Int, Int)] 
  | Empty [(Int, Int)]
  | BecomeAlive (Int, Int) 
  | Survive (Int, Int)
  | Help 
  | WriteLiving 
  | RulesFromFile String
  | Enter 
  | Live Int
  | Quit 
  | GoToMain
  deriving (Eq, Ord, Show, Read)


--TODO remove error msgs and with GoToMain, give error feedback in main instead
parseInput :: String -> Command --Parses user input
parseInput str
  | head str == 'c' =
    if length split == 2
    && all isDigit (last split)
    && read (last split) > 0
    && read (last split) < 100
    then (Create (read (last split)))
    else GoToMain

  | head str == 'n' = --Needs work
    if all (all isDigit) (tail split) 
    && even (length (tail split))
    then (New (listToTupleList (tail split)))
    else GoToMain

  | head str == 'e' = --Needs work
    if all (all isDigit) (tail split) --same as n
    && even (length (tail split))
    then (Empty (listToTupleList (tail split)))
    else GoToMain

  | head str == 'b' =
    if length (words str) == 3
    && all (all isDigit) (tail split)
    && read ((tail split) !! 0) > -1
    && read ((tail split) !! 1) > -1
    then (BecomeAlive (read ((tail split) !! 0), read ((tail split) !! 1)))
    else GoToMain

  | head str == 's' =
    if length (words str) == 3
    && all (all isDigit) (tail split)
    && read ((tail split) !! 0) > -1
    && read ((tail split) !! 1) > -1
    then (Survive (read ((tail split) !! 0), read ((tail split) !! 1)))
    else GoToMain

  | head str == '?' = Help
  | head str == 'w' = WriteLiving
  | head str == 'r' = RulesFromFile (last split)
  | head str == 'l' =
    if length split == 2
    && all isDigit (last split)
    && read (last split) > 0
    then (Live (read (last split)))
    else GoToMain
  | str == []       = Enter
  | str == "quit"   = Quit
  | otherwise       = GoToMain
  where
    split = words str




listToTupleList :: (Read a, Read b) => [String] -> [(a, b)]
listToTupleList [] = []
listToTupleList (x:y:xs) = (read x, read y) : listToTupleList xs



--TODO: Parse file input


{---------------
--Eval functions
----------------}


evalCreate :: Command -> Bool
evalCreate (Create x) = True
evalCreate _ = False


evalNew :: Command -> Bool
evalNew (New xs) = True
evalNew _ = False


evalEmpty :: Command -> Bool
evalEmpty (Empty xs) = True
evalEmpty _ = False

evalBorn :: Command ->Bool
evalBorn (BecomeAlive x) = True
evalBorn _ = False


evalSurvie :: Command -> Bool
evalSurvie (Survive x) = True
evalSurvie _ = False

evalLive :: Command -> Bool
evalLive (Live x) = True
evalLive _ = False

evalRulesFromFile :: Command -> Bool
evalRulesFromFile (RulesFromFile x) = True
evalRulesFromFile _ = False


{-----------------
--Helper functions
------------------}


createToInt :: Command -> Int
createToInt (Create x) =  x


newToString :: Command -> [Char]
newToString (New x) = do
  newToString' (head x)
  newToString (New (tail x))

newToString' :: (Show a1, Show a2) => (a1, a2) -> [Char]
newToString' (x,y) = show x ++ show y



emptyToString :: Command -> [Char]
emptyToString (Empty x) = do
  emptyToString' (Empty x)
  emptyToString (New (tail x))


emptyToString' :: Command -> [Char]
emptyToString' (Empty [(x,y)]) = show x ++ show y



emptyToList :: Command -> [(Int, Int)]
emptyToList (Empty x) = x


newToList :: Command -> [(Int, Int)]
newToList (New x ) = x


surviveToTup :: Command -> (Int, Int)
surviveToTup (Survive x) = x

bornToTup :: Command -> (Int, Int)
bornToTup (BecomeAlive x) = x

liveToInt :: Command -> Int
liveToInt (Live x) = x

--rulesFromFileToFile :: Command -> String
fileToFileName :: Command -> String
fileToFileName (RulesFromFile x) = x




drawCells :: Command -> Board
drawCells (New x) = (LivingCells x) 
drawCells (Empty x) = (LivingCells x)

---------------
--FILEHANDLER--
---------------

data FileInput
  = FileSize [String]
  | Born (Int, Int)
  | Survival (Int, Int)
  | Coords [Int]
  deriving (Eq, Ord, Show, Read)



{-------------------
--Get file contents
--------------------}



--Tokenize



--"5 (s 2 3, b 3 3) 3 3 1 3 2 1 3 2 2 3"
--"15 (s 2 3, b 3 3) 3 3 1 3 2 1 3 2 2 3"




sizeFromFile :: FilePath -> IO ()
sizeFromFile file = do 
   FileSize xs <- readIO =<< readFile file
   _ <- traverse print (zip [0 :: Int ..] xs)
   _ <- getLine
   print ()
  


  {-
  | isAlpha x = (takeWhile (isAlpha) (x:xs)) : (tokenize (dropWhile (isAlpha) (x:xs)))
  | isDigit x = (takeWhile (isDigit) (x:xs)) : (tokenize (dropWhile (isDigit) (x:xs)))
  | elem x "+*-" = [x] : (tokenize xs)
  | otherwise = []
  
  -}



fileContent :: FilePath -> IO String
fileContent file = do
  content <- readFile file
  return content



getTuple :: FilePath -> IO [Char]
getTuple fileName = do
    contents <- readFile fileName
    let tuple = take 14 $ drop 2 $ contents
    return tuple




getSize :: FilePath -> IO ()
getSize fileName = do
    contets <- readFile fileName
    let boardSize = head contets
    drawBoard (read [boardSize])
    print (boardSize)
    --return boardSize



getCoords :: FilePath -> IO ()
getCoords fileName = do
    contents <- readFile fileName
    let coords = parseInput ("n" ++ drop 17 contents)
    print coords




--Checks if file exists and is of correct format
fileCheck :: FilePath -> IO Bool
fileCheck file = doesFileExist file
    


---------
--LOGIC--
---------
--Logic for how game is played




--list of coords of all neighbours regrdles if they are valid 
allNeighbours :: (Int, Int) -> [(Int, Int)]
allNeighbours (x,y) = [topLeft, straighAbove, topRight, right, bottomRight, straighBellow, bottomLeft, left] where 
    topLeft       = (x-1, y-1) 
    straighAbove  = (x, y-1)
    topRight      = (x+1, y-1)
    right         = (x+1, y)
    bottomRight   = (x+1, y+1)
    straighBellow = (x, y+1)
    bottomLeft    =  (x-1, y+1)
    left          = (x-1, y)
    


--allboardCoords
allboardCoords :: Int -> [(Int, Int)]
allboardCoords x =  [(a, b) | a <- [1..x], b <- [1..x]]


--List of coords of neighbours alive to cell
neighBoursAlive :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)] --cell = cell whos neighbours you'd like to check, cellsAlive are all cells alvie
neighBoursAlive cell cellsAlive = intersect (allNeighbours cell) cellsAlive






{-----------
--Cell Birth
------------}


--For hver tomme celle på brettet hvis cellen har minst m levende og maks n levende celler som nabo så blir den født 



--PROBEBlY redundant, can use checkSurvival
--Checks if a cell will be born, |takes a cell and returns Bool
checkBirth :: (Int,Int) -> (Int,Int) -> [(Int, Int)] -> Bool
checkBirth cell bornThreshold cellsAlive = if length (neighBoursAlive cell cellsAlive) >= fst bornThreshold
    then length (neighBoursAlive cell cellsAlive) <= snd bornThreshold
    else False


--PROBEBlY redundant, can use survingCells
bornCells ::  [(Int, Int)] -> [(Int, Int)] -> (Int, Int) -> [(Int,Int)]
bornCells [] _ _ = []
bornCells (cell:cellsToBeChecked) livingCells bornThreshold = 
    if checkSurvival cell bornThreshold livingCells then 
        (bornCells cellsToBeChecked livingCells bornThreshold) ++ [cell]
    else bornCells cellsToBeChecked livingCells bornThreshold 


{--------------
--Cell Survival
---------------}



--gives back cells that will survive to next turn | takes livingcells survivaelthreshold
survivingCells :: [(Int, Int)] -> [(Int, Int)] -> (Int, Int) -> [(Int,Int)]
survivingCells [] _ _ = []
survivingCells (cell:cellsToBeChecked) livingCells survivalThreshold = 
    if checkSurvival cell survivalThreshold livingCells then 
        (survivingCells cellsToBeChecked livingCells survivalThreshold) ++ [cell]
    else survivingCells cellsToBeChecked livingCells survivalThreshold 



--returns bool based on if the cell survives
--survivingCells


--Rename to checkThreshold?
checkSurvival :: (Int, Int) -> (Int, Int) -> [(Int,Int)] -> Bool
checkSurvival cell survivalThreshold cellsAlive = if length (neighBoursAlive cell cellsAlive) >= fst survivalThreshold
    then length (neighBoursAlive cell cellsAlive) <= snd survivalThreshold
    else False



isAlive :: (Int,Int) -> [(Int,Int)] -> Bool
isAlive cell cellsAlive = cell `elem` cellsAlive