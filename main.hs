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

loopBF :: String -> Tape -> [BFCommand] -> IO (String, Tape)
loopBF s t@(Tape ls 0 rs) _ = return (s, t)
loopBF s t bfcs = do
  (s', t') <- evalBF s t bfcs
  loopBF s' t' bfcs

printCell :: String -> Tape -> (String, Tape)
printCell s t@(Tape ls x rs) = (s ++ [toEnum $ fromEnum x], t)

readCell :: Tape -> IO Tape
readCell t@(Tape ls x rs) = do
  c <- getChar
  return (Tape ls (toEnum $ ord c) rs)

evalBF :: String -> Tape -> [BFCommand] -> IO (String, Tape)
evalBF s t bfcs = do
  case bfcs of
    [] -> return (s, t)
    bc : bcs -> case bc of
      IncCell -> evalBF s (incCell t) bcs
      DecCell -> evalBF s (decCell t) bcs
      MvRight -> evalBF s (mvRight t) bcs
      MvLeft -> evalBF s (mvLeft t) bcs
      Loop bcs' -> do
        (s', t') <- loopBF s t bcs'
        evalBF s' t' bcs
      PrintCell -> do
        let (s', t') = printCell s t
         in evalBF s' t' bcs
      ReadCell -> do
        t' <- readCell t
        evalBF s t' bcs
      Comment com -> evalBF s t bcs

tests :: [(String, String)]
tests =
  [ ("+[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++[.>]<<<<.+++.------.<<-.>>>>+.", "Hello, World!"),
    ("++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.", "Hello World!\n"),
    ("++++[>++++++<-]>[>+++++>+++++++<<-]>>++++<[[>[[>>+<<-]<]>>>-]>-[>+>+<<-]>]+++++[>+++++++<<++>-]>.<<.", "#\n"),
    ("[]++++++++++[>>+>+>++++++[<<+<+++>>>-]<<<<-]\nA*$\";?@![#>>+<<]>[>>]<<<<[>++<[-]]>.>.", "H\n")
  ]

runTest :: String -> String -> IO String
runTest input expected = case parse parseBF "test" (fromString input) of
  Left pe -> return "Failed parsing"
  Right bcs -> do
    (result, _) <- evalBF "" newBFTape bcs
    return $ if result == expected then "Pass" else "Fail"

runTests :: IO [String]
runTests = mapM (uncurry runTest) tests

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["test"] -> do
      ss <- runTests
      print ss
    [fn] -> do
      contents <- readFile fn
      case parse parseBF "main" (fromString contents) of
        Left pe -> print pe
        Right bcs -> do
          (s, _) <- evalBF "" newBFTape bcs
          putStrLn s
    _ -> do
      progName <- getProgName
      putStrLn $ "USAGE: " ++ progName ++ " " ++ "FILENAME"
