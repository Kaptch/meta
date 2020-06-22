module Meta (lexer, parser, eval, Token, Error, Exp, Eval, defaultEnv, Env, printTree) where

import           Meta.Backend  (Env, Eval, EvalErr (..), continue, defaultEnv)
import           Meta.Frontend (Err (..), Exp, Token, pExp, printTree, tokens)

data Error = ParseError String | EvalError EvalErr
  deriving (Show, Eq)

lexer :: String -> [Token]
lexer str = tokens str

parser :: String -> Either Error Exp
parser str = case pExp $ lexer str of
  Bad str -> Left $ ParseError str
  Ok a    -> Right a

eval :: String -> Env -> Either Error (Exp, Env)
eval str env = case pExp $ lexer str of
  Bad str -> Left $ ParseError str
  Ok a    -> case continue a env of
    Left err -> Left $ EvalError err
    Right b  -> Right b
