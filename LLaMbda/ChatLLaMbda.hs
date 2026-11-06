import Data.Char
import System.IO
import Control.Exception

import LTypes
import qualified Lambek

import Parser
import PySupport
import LExp
import Ambiguity

-- "standardize" the capitalization of a word by making the first
-- letter uppercase and the following letters lowercase.
-- (This will make it easier to look up words in the lexicon.)
stdize (c:cs) = toUpper c : map toLower cs

handle_ambiguity :: PythonHandle -> [(LExp, [(Var, PythonCode)])] -> IO ()
handle_ambiguity phandle alternatives = 
  case unique_derivations alternatives of
    [] -> return ()

    [(expression, meanings)] -> do
      result <- evaluateMeaning phandle expression meanings
      putStrLn result

    derivations -> do
      putStrLn "That input is ambiguous"
      printReadings phandle derivations



toPython_helper :: [(Var, PythonCode)] -> Int -> LExp -> Maybe PythonCode

toPython_helper [] i (V x) = Nothing

toPython_helper ((name, translation):rest) i (V x)
  | x == name = Just translation
  | otherwise = toPython_helper rest i (V x)

toPython_helper meanings i (A e1 e2) = do
  functionCode <- toPython_helper meanings i e1
  argumentCode <- toPython_helper meanings i e2

  return (PCode ("(" ++ text functionCode ++ ")("++ text argumentCode ++ ")"))

toPython_helper meanings i (L x e) = do
  bodyCode <- toPython_helper ((x, PCode name) : meanings) (i + 1) e

  return (PCode ("(lambda " ++ name ++ ": " ++ text bodyCode ++ ")" ) )
    where name = "arg" ++ show i

toPython :: [(Var, PythonCode)] -> LExp -> Maybe PythonCode
toPython meanings expression = toPython_helper meanings 0 expression

evaluateMeaning :: PythonHandle -> LExp -> [(Var, PythonCode)] -> IO String
evaluateMeaning phandle expression meanings =
  case toPython meanings expression of

    Nothing -> return "Could not interpret the expression: missing variable meaning."

    Just code -> runPythonCode phandle (PCode ("print(" ++ text code ++ ")"))

printReadings :: PythonHandle -> [(LExp, [(Var, PythonCode)])] -> IO ()

printReadings phandle [] = return ()

printReadings phandle ((expression, meanings):rest) = do
  result <- evaluateMeaning phandle expression meanings

  putStrLn ("Sense [" ++ prettyLExp expression ++ "]: " ++ result)

  printReadings phandle rest




-- Convert simple yes/no questions into statements
questionToStatement :: [String] -> Maybe [String]

questionToStatement ["Does", person, "Study?"] =
    Just [person, "Studies"]

questionToStatement ["Does", person, "Teach", object] =
    if not (null object) && last object == '?'
    then Just [person, "Teaches", init object]
    else Nothing

questionToStatement ["Is", person, "Tired?"] =
    Just [person, "Is", "Tired"]

questionToStatement _ = Nothing


-- read-eval-print-loop

-- Read-eval-print-loop
repl :: Lexicon PythonCode -> PythonHandle -> IO ()
repl elx phandle = do

  putStr "> "
  line <- getLine

  let ws = map stdize (words line)

  case questionToStatement ws of

    Just statement -> do
      putStrLn "Interpreting question:"
      evaluateSentence elx phandle statement

    Nothing ->
      evaluateSentence elx phandle ws

  repl elx phandle


evaluateSentence :: Lexicon PythonCode
                 -> PythonHandle
                 -> [String]
                 -> IO ()

evaluateSentence elx phandle ws =
  case Lambek.new_derivations elx ws of

    [] ->
      putStrLn "That is not a well-formed sentence!"

    [(expression, meanings)] -> do
      result <- evaluateMeaning phandle expression meanings
      putStrLn result

    alternatives -> do
      putStrLn "That sentence is ambiguous"
      printReadings phandle alternatives


main = do
  -- send console output immediately to the user
  hSetBuffering stdout NoBuffering
  -- input a lexicon file
  elx <- getLex
  -- input a database of facts
  db <- getDB
  -- launch a Python interpreter
  phandle <- launchPython db
  -- start the read-eval-print loop
  putStrLn "Starting chat..."
  repl elx phandle
  where
    getDB :: IO PythonCode
    getDB = do
      putStr "Facts about the world: "
      dname <- getLine
      mdb <- try (readFile dname) :: IO (Either IOError String)
      case mdb of
        Left err -> putStrLn ("Error loading file " ++ show err) >> getDB
        Right db -> return (PCode db)
      
    getLex :: IO (Lexicon PythonCode)
    getLex = do
      putStr "Lexicon file: "
      lname <- getLine
      res <- parseFile lexicon lname
      case res of
        Left err -> putStrLn ("Error loading file " ++ show err) >> getLex
        Right (Left err) -> putStrLn ("parse error at " ++ lname ++ ":" ++ show err) >> getLex
        Right (Right lex) -> return lex
