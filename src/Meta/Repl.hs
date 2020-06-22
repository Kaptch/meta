module Meta.Repl where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Data.List              (isPrefixOf)
import           Data.Void              (Void)
import           Meta                   (Env, Eval, defaultEnv, eval, lexer,
                                         parser, printTree)
import           System.Console.Repline (CompleterStyle (..), HaskelineT,
                                         WordCompleter, abort, evalRepl)
import           Text.Megaparsec        (ErrorFancy (..), Parsec (..),
                                         parseMaybe, (<|>))
import           Text.Megaparsec.Char   (printChar, space1, string)

type ParseError = ErrorFancy Void

data Mode = Lexer
          | Parser
          | Runner
          deriving (Show, Eq)

data St = St { mode :: Mode, env :: Env }

type Repl a = (HaskelineT (StateT St IO)) a

parseLexer :: Parsec ParseError String Mode
parseLexer = do
  string "lexer"
  pure Lexer

parseParser :: Parsec ParseError String Mode
parseParser = do
  string "parser"
  pure Parser

parseRunner :: Parsec ParseError String Mode
parseRunner = do
  string "runner"
  pure Runner

parseMode :: Parsec ParseError String Mode
parseMode = parseLexer <|> parseParser <|> parseRunner

process :: String -> Repl ()
process input = do
  st <- lift $ get
  case mode st of
    Lexer  -> (liftIO $ print $ lexer input)
    Parser -> (liftIO $ print $ parser input)
    Runner -> case eval input (env st) of
      Left err -> liftIO $ print err
      Right (exp, env) -> do
        lift $ put $ St (mode st) env
        liftIO $ print $ printTree exp

completer :: Monad m => WordCompleter m
completer n = do
  let builtins = ["def!", "defmacro!", "quote!", "unquote!"]
  return $ filter (isPrefixOf n) builtins

help :: [String] -> Repl ()
help _ = liftIO $ putStr helpString
  where
    helpString :: String
    helpString = " :help :?     - outputs this message" ++ "\n"
              ++ " :quit        - exits the interpreter" ++ "\n"
              ++ " :md          - outputs the current mode" ++ "\n"
              ++ " :chmd <mode> - changes the interpreter's mode" ++ "\n"

quit :: [String] -> Repl ()
quit _ = abort

printMode :: [String] -> Repl ()
printMode _ = do
  st <- lift get
  liftIO $ print (mode st)

changeMode :: [String] -> Repl ()
changeMode [] = liftIO $ print "Couldn't parse a mode"
changeMode (str:_) = case parseMaybe parseMode str of
  Nothing -> liftIO $ print "Couldn't parse a mode"
  Just md -> do
    st <- lift $ get
    lift $ put $ St md (env st)

opts :: [(String, [String] -> Repl ())]
opts = [
    ("?", help)
  , ("help", help)
  , ("quit", quit)
  , ("md", printMode)
  , ("chmd", changeMode)
  ]

ini :: Repl ()
ini = liftIO $ print $ "meta, version " ++ "0.1.0.0" ++ " :? for help"

startRepl :: IO ()
startRepl = evalStateT
  (evalRepl (pure "meta> ") process opts (Just ':') (Word completer) ini)
  (St Runner defaultEnv)
