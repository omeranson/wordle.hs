import Data.Char
import Data.List
import System.IO
import System.Random

wordleFrame :: String
wordleFrame = "+---+---+---+---+---+"

data WordleColour = Miss | Yellow | Green
        deriving Eq
data WordleLetter = Blank | Letter Char WordleColour
        deriving Eq

yellow :: String -> String
yellow s = "\ESC[43m" ++ s ++ "\ESC[0m"        

green :: String -> String
green s = "\ESC[42m" ++ s ++ "\ESC[0m"        

instance Show WordleLetter where
        -- show :: WordleLetter -> String
        show Blank = " "
        show (Letter c Miss) = [c]
        show (Letter c Yellow) = yellow [c]
        show (Letter c Green) = green [c]

newtype WordleRow = WordleRow [WordleLetter]
instance Show WordleRow where
        -- show :: WordleRow -> String
        show (WordleRow row) = "| " ++ (intercalate " | " (map show row)) ++ " |"
wordleEmptyRow :: WordleRow
wordleEmptyRow = WordleRow (replicate 5 Blank)

data Wordle = Wordle { word :: String,
                       dictionary :: [String],
                       rows :: [WordleRow],
                       rowCount :: Int,
                       currentRow :: String,
                       message :: String
                     }
wordleNewGame :: String -> [String] -> Wordle
wordleNewGame w dict = Wordle w dict (replicate 6 wordleEmptyRow) 0 "" ""

showRows :: Wordle -> [String]
showRows w = map show (rows w)

showWordle :: Wordle -> String
showWordle w = 
        unlines . concat $ [[wordleFrame],
                            intersperse wordleFrame (showRows w),
                            [wordleFrame],
                            [""],
                            [intersperse ' ' . currentRow $ w],
                            [intersperse ' ' . replicate 5 $ '‾'],
                            [""],
                            [message w]]

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

goto :: Int -> Int -> IO ()
goto x y = putStr ("\ESC[" ++ show x ++ ";" ++ show y ++ "H")

putWordle w = do
        clearScreen
        goto 0 0
        putStrLn . showWordle $ w

getCh :: IO Char
getCh = do
        hSetEcho stdin False
        x <- getChar
        hSetEcho stdin True
        return (toUpper x)

wordleAddLetter :: Wordle -> Char -> Wordle
wordleAddLetter w c = Wordle (word w) (dictionary w) (rows w) (rowCount w) (currentRow w ++ [c]) ""

wordleDelLetter :: Wordle -> Char -> Wordle
wordleDelLetter w c = Wordle (word w) (dictionary w) (rows w) (rowCount w) (init . currentRow $ w) ""

wordleWithMessage :: Wordle -> String -> Wordle
wordleWithMessage w s = Wordle (word w) (dictionary w) (rows w) (rowCount w) (currentRow w) s

wordleCheckLetter :: String -> String -> (Int, Char) -> WordleColour
wordleCheckLetter w cr (idx, c) | w !! idx == c = Green
                                | elem c w = Yellow
                                | otherwise = Miss

wordleNewCurrentRow :: String -> String -> WordleRow
wordleNewCurrentRow w cr = WordleRow . map (\(n, c) -> Letter c . wordleCheckLetter w cr $ (n, c)) . zip [0..] $ cr

replaceRow :: [a] -> Int -> a -> [a]
replaceRow xs n x = take n xs ++ [x] ++ drop (n+1) xs

wordleAddCurrentRow :: Wordle -> Wordle
wordleAddCurrentRow w | not . elem (currentRow w) $ (dictionary w) = wordleWithMessage w "Word not in dictionary"
                      | otherwise = Wordle (word w) (dictionary w) (replaceRow (rows w) (rowCount w) (wordleNewCurrentRow (word w) (currentRow w))) (rowCount w + 1) "" ""

wordleWon :: Wordle -> Bool
wordleWon w = currentRow w == word w

wordleLost :: Wordle -> Bool
wordleLost w = rowCount w >= 6

wordleCheck :: Wordle -> IO ()
wordleCheck w | wordleWon w = putWordle (wordleWithMessage w' "Well Done!") >> return ()
              | wordleLost w' = putWordle (wordleWithMessage w' ("The word was: " ++ (word w))) >> return ()
              | otherwise = wordleLoop w'
              where w' = wordleAddCurrentRow $ w

wordleHandleInput :: Wordle -> Char -> IO ()
wordleHandleInput w c | isAlpha c && length (currentRow w) < 5 = wordleLoop (wordleAddLetter w c)
                      | isAlpha c && length (currentRow w) >= 5 = wordleLoop w
                      | c == '\DEL' && length (currentRow w) > 0 = wordleLoop (wordleDelLetter w c)
                      | c == '\DEL' && length (currentRow w) == 0 = wordleLoop w
                      | c == '\ESC' = return ()
                      | c == '\n' && length (currentRow w) < 5 = wordleLoop w
                      | c == '\n' && length (currentRow w) == 5 = wordleCheck w
                      | otherwise = wordleLoop (wordleWithMessage w "Illegal input")

wordleLoop :: Wordle -> IO ()
wordleLoop w = do
        putWordle w
        c <- getCh
        wordleHandleInput w c

choose :: [a] -> IO a
choose xs = System.Random.randomRIO (0, length xs - 1) >>= return . (xs !!)

selectWord :: [String] -> IO String
selectWord = choose

dictionaryFilename :: String 
dictionaryFilename = "dict.txt"

readDictionary :: IO [String]
readDictionary = readFile dictionaryFilename >>= (return . lines)

wordle :: IO ()
wordle = do
        dictionary <- readDictionary
        word <- selectWord dictionary
        wordleLoop (wordleNewGame word dictionary)

main :: IO ()
main = wordle
