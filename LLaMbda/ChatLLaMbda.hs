import Data.Char
import System.IO
import Control.Exception

import LTypes
import qualified Lambek

import Parser
import PySupport
import LExp

{-
Definition of mapM_ from the prelude, notice that unlike map or mapM it discards elements hence good for printing

mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f [] = return ()
mapM_ f (x:xs) = do
  f x
  mapM_ f xs



-}

-- "standardize" the capitalization of a word by making the first
-- letter uppercase and the following letters lowercase.
-- (This will make it easier to look up words in the lexicon.)
stdize (c:cs) = toUpper c : map toLower cs

toPython_helper :: [(Var, PythonCode)] -> Int -> LExp
                -> Maybe PythonCode

toPython_helper [] i (V x) = Nothing

toPython_helper ((name, meaning):rest) i (V x)
  | x == name = Just meaning
  | otherwise = toPython_helper rest i (V x)

toPython_helper meanings i (A e1 e2) = do
  functionCode <- toPython_helper meanings i e1
  argumentCode <- toPython_helper meanings i e2

  return (PCode ("(" ++ text functionCode ++ ")("++ text argumentCode ++ ")"))

toPython_helper meanings i (L x e) = do

  bodyCode <- toPython_helper ((x, PCode name) : meanings) (i + 1) e

  return (PCode ("(lambda " ++ name ++ ": " ++ text bodyCode ++ ")" ) )
    where name = "__arg__" ++ show i

toPython :: [(Var, PythonCode)] -> LExp -> Maybe PythonCode
toPython meanings expression = toPython_helper meanings 0 expression

evaluateMeaning :: PythonHandle -> LExp -> [(Var, PythonCode)] -> IO String
evaluateMeaning phandle expression meanings =
  case toPython meanings expression of

    Nothing ->
      return "Could not interpret the expression: missing variable meaning."

    Just code ->
      runPythonCode phandle
        (PCode ("print(" ++ text code ++ ")"))

printReadings :: PythonHandle -> [(LExp, [(Var, PythonCode)])] -> IO ()

printReadings phandle [] = return ()

printReadings phandle ((expression, meanings):rest) = do
  result <- evaluateMeaning phandle expression meanings

  putStrLn ("Sense [" ++ prettyLExp expression ++ "]: " ++ result)

  printReadings phandle rest

-- read-eval-print-loop
repl :: Lexicon PythonCode -> PythonHandle -> IO ()
repl elx phandle = do
  -- print the prompt
  putStr "> "
  -- get a line of input
  line <- getLine
  -- split it up into "standardized" words
  let ws = map stdize $ words line
  -- test if the string of words is a grammatical sentence according to the lexicon
  case Lambek.derivations elx ws of
    [] -> putStrLn "That not is a well-formed sentence!"

    [(expression, meanings)] -> do
      result <- evaluateMeaning phandle expression meanings
      putStrLn result

    alternatives -> do
      putStrLn "That sentence is ambiguous"
      printReadings phandle alternatives

  repl elx phandle
      
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
