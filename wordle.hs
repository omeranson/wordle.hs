import Data.Char
import Data.List
import Data.Maybe
import System.IO
import System.Random

wordleFrame :: String
wordleFrame = "+---+---+---+---+---+"

data WordleColour = None | Miss | Yellow | Green
        deriving (Eq, Ord)
data WordleLetter = Blank | Letter Char WordleColour
        deriving Eq

wordleLetterAsPair :: WordleLetter -> (Char, WordleColour)
wordleLetterAsPair Blank = (' ', None)
wordleLetterAsPair (Letter c wc) = (c, wc)

wordleLetterFromPair :: (Char, WordleColour) -> WordleLetter
wordleLetterFromPair (c, wc) = Letter c wc

yellow :: String -> String
yellow s = "\ESC[43m" ++ s ++ "\ESC[0m"

green :: String -> String
green s = "\ESC[42m" ++ s ++ "\ESC[0m"

grey :: String -> String
grey s = "\ESC[40;37m" ++ s ++ "\ESC[0m"

instance Show WordleLetter where
        -- show :: WordleLetter -> String
        show Blank = " "
        show (Letter c None) = [c]
        show (Letter c Miss) = grey [c]
        show (Letter c Yellow) = yellow [c]
        show (Letter c Green) = green [c]

newtype WordleRow = WordleRow [WordleLetter]
instance Show WordleRow where
        -- show :: WordleRow -> String
        show (WordleRow row) = "| " ++ (intercalate " | " (map show row)) ++ " |"

letters :: WordleRow -> [WordleLetter]
letters (WordleRow ls) = ls

newtype WordleKeyboard = WordleKeyboard [WordleLetter]

showKey :: [WordleLetter] -> Char -> String
showKey keys k = show (Letter k wc)
        where wc = fromMaybe None . lookup k . map wordleLetterAsPair $ keys

instance Show WordleKeyboard where
        -- show :: WordleKeyboard -> String
        show (WordleKeyboard keys) = unlines [
                intercalate " " [showKey keys k | k <- "QWERTYUIOP"],
                " " ++ (intercalate " " [showKey keys k | k <- "ASDFGHJKL"]),
                "   " ++ (intercalate " " [showKey keys k | k <- "ZXCVBNM"])]

wordleEmptyRow :: WordleRow
wordleEmptyRow = WordleRow (replicate 5 Blank)

data Wordle = Wordle { word :: String,
                       dictionary :: [String],
                       rows :: [WordleRow],
                       rowCount :: Int,
                       currentRow :: String,
                       message :: String,
                       keyboard :: WordleKeyboard
                     }
wordleNewGame :: String -> [String] -> Wordle
wordleNewGame w dict = Wordle w dict (replicate 6 wordleEmptyRow) 0 "" "" (WordleKeyboard [])

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
                            [show . keyboard $ w],
                            [""],
                            [message w]
                            ]

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

goto :: Int -> Int -> IO ()
goto x y = putStr ("\ESC[" ++ show x ++ ";" ++ show y ++ "H")

putWordle :: Wordle -> IO ()
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
wordleAddLetter w c = Wordle (word w) (dictionary w) (rows w) (rowCount w) (currentRow w ++ [c]) "" (keyboard w)

wordleDelLetter :: Wordle -> Char -> Wordle
wordleDelLetter w c = Wordle (word w) (dictionary w) (rows w) (rowCount w) (init . currentRow $ w) "" (keyboard w)

wordleWithMessage :: Wordle -> String -> Wordle
wordleWithMessage w s = Wordle (word w) (dictionary w) (rows w) (rowCount w) (currentRow w) s (keyboard w)

-- If a letter is where it should be, it's green
-- If a letter is in the wrong place (and hasn't been used yet), it's yellow
-- otherwise, miss
wordleCheckLetter :: String -> String -> (Int, Char) -> WordleColour
wordleCheckLetter w cr (idx, c) | w !! idx == c = Green
                                | elem c w = Yellow
                                | otherwise = Miss

wordleCheckLetter2 :: ([(Char, Bool)], [(Char, WordleColour)]) -> (Int, Char) -> ([(Char, Bool)], [(Char, WordleColour)])
wordleCheckLetter2 (w, cr) (cn,c) = case elemIndex (c, False) w of
        Nothing -> (w, cr)
        Just wn -> (replaceIndex w wn (c, True), replaceIndex cr cn (c, Yellow))

wordleNewCurrentRow :: String -> String -> WordleRow
--wordleNewCurrentRow w cr = WordleRow . map (\(n, c) -> Letter c . wordleCheckLetter w cr $ (n, c)) . zip [0..] $ cr
wordleNewCurrentRow w cr = WordleRow . map wordleLetterFromPair $ cr3
        where w1 = zip w (repeat False)
              (w2, cr2) = unzip . map (\((c1, p), c2) ->
                                       if c1 == c2 then ((c1, True), (c2, Green)) else ((c1, False), (c2, Miss))) . zip w1 $ cr
              cr3 = snd . foldl wordleCheckLetter2 (w2, cr2) . zip [0..] . map fst $ cr2


replaceIndex :: [a] -> Int -> a -> [a]
replaceIndex xs n x = take n xs ++ [x] ++ drop (n+1) xs

replace :: Eq a => a -> b -> [(a, b)] -> [(a,b)]
replace k v m = [(k', if (k == k') then v else v') | (k', v') <- m]

insert :: Eq a => a -> b -> [(a, b)] -> [(a,b)]
insert k v m = case lookup k m of
        Nothing -> (k,v):m
        (Just _) -> replace k v m

replaceIf :: Eq a => (b -> Bool) -> a -> b -> [(a, b)] -> [(a,b)]
replaceIf p k v m = [(k', if (k == k' && p v') then v else v') | (k', v') <- m]

insertIf :: Eq a => (b -> Bool) -> a -> b -> [(a, b)] -> [(a,b)]
insertIf p k v m = case lookup k m of
        Nothing -> (k,v):m
        (Just _) -> replaceIf p k v m

wordleUpdateKeyboard :: WordleKeyboard -> WordleLetter -> WordleKeyboard
wordleUpdateKeyboard kb Blank = kb  -- Should never happen
wordleUpdateKeyboard (WordleKeyboard ls) (Letter c wc) = WordleKeyboard . map wordleLetterFromPair . insertIf (<wc) c wc . map wordleLetterAsPair $ ls

wordleAddCurrentRow :: Wordle -> Wordle
wordleAddCurrentRow w | not . elem (currentRow w) $ (dictionary w) = wordleWithMessage w "Word not in dictionary"
                      | otherwise = Wordle (word w) (dictionary w) (replaceIndex (rows w) (rowCount w) row) (rowCount w + 1) "" "" (keyboard')
                      where row = wordleNewCurrentRow (word w) (currentRow w)
                            keyboard' = foldl wordleUpdateKeyboard (keyboard w) (letters row)

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
        oldbuff <- hGetBuffering stdin
        hSetBuffering stdin NoBuffering
        dictionary <- readDictionary
        word <- selectWord dictionary
        wordleLoop (wordleNewGame word dictionary)
        hSetBuffering stdin oldbuff

main :: IO ()
main = wordle
