import Data.Char (ord)
import Data.Int (Int8)
import Data.String (IsString (fromString))
import System.Environment (getArgs, getProgName)
import Text.Parsec (between, char, choice, many, noneOf, parse)
import Text.Parsec.ByteString (GenParser)

--  Brainfuck interpreter
--  author: Brent Pappas

-- See:
--  http://brainfuck.org/
--  http://www.iwriteiam.nl/Ha_BF.html
--  http://www.muppetlabs.com/~breadbox/bf/

data BFCommand
  = IncCell
  | DecCell
  | MvRight
  | MvLeft
  | Loop [BFCommand]
  | PrintCell
  | ReadCell
  | Comment Char

literalsBF :: [Char]
literalsBF = "+-><[].,"

parseBF :: GenParser String st [BFCommand]
parseBF =
  many $
    choice
      [ IncCell <$ char '+',
        DecCell <$ char '-',
        MvRight <$ char '>',
        MvLeft <$ char '<',
        PrintCell <$ char '.',
        ReadCell <$ char ',',
        Loop <$> between (char '[') (char ']') parseBF,
        Comment <$> noneOf literalsBF
      ]

data Tape = Tape [Int8] Int8 [Int8]

newBFTape :: Tape
newBFTape = Tape (repeat 0) 0 (repeat 0)

incCell :: Tape -> Tape
incCell (Tape ls x rs) = Tape ls (x + 1) rs

decCell :: Tape -> Tape
decCell (Tape ls x rs) = Tape ls (x - 1) rs

mvRight :: Tape -> Tape
mvRight (Tape ls x (r : rs)) = Tape (x : ls) r rs
mvRight _ = error "Error: Can't move right"

mvLeft :: Tape -> Tape
mvLeft (Tape (l : ls) x rs) = Tape ls l (x : rs)
mvLeft _ = error "Error: Can't move left"

loopBF :: Tape -> [BFCommand] -> IO Tape
loopBF t@(Tape ls 0 rs) _ = return t
loopBF t coms = do
  t' <- evalBF t coms
  loopBF t' coms

printCell :: Tape -> IO Tape
printCell t@(Tape ls x rs) = do
  putChar $ toEnum $ fromEnum x
  return t

readCell :: Tape -> IO Tape
readCell t@(Tape ls x rs) = do
  c <- getChar
  return (Tape ls (toEnum $ ord c) rs)

evalBF :: Tape -> [BFCommand] -> IO Tape
evalBF t coms = do
  case coms of
    [] -> return t
    bc : bcs -> case bc of
      IncCell -> evalBF (incCell t) bcs
      DecCell -> evalBF (decCell t) bcs
      MvRight -> evalBF (mvRight t) bcs
      MvLeft -> evalBF (mvLeft t) bcs
      Loop bcs' -> do
        t' <- loopBF t bcs'
        evalBF t' bcs
      PrintCell -> do
        t' <- printCell t
        evalBF t' bcs
      ReadCell -> do
        t' <- readCell t
        evalBF t' bcs
      Comment s -> evalBF t bcs

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fn] -> do
      contents <- readFile fn
      case parse parseBF "main" (fromString contents) of
        Left pe -> print pe
        Right bcs -> do evalBF newBFTape bcs; putStrLn ""
    _ -> do
      progName <- getProgName
      putStrLn $ "USAGE: " ++ progName ++ " " ++ "FILENAME"
