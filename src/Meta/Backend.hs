{-# LANGUAGE KindSignatures #-}

module Meta.Backend where

-- TODO:
-- defun
-- lambda
-- systemcalls

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Functor.Identity
import qualified Data.List             as List
import qualified Data.Map              as Map
import           Meta.Frontend         (Exp (..), Ident (..))

type Fn = [Exp] -> Eval Exp

data Env = Env (Map.Map Ident Fn)

data EvalErr = UnknownSymbol | IllegalCall | BindedIdent | UnknownError String
  deriving (Show, Eq)

type EvalT m = StateT Env (ExceptT EvalErr m)

type Eval a = EvalT Identity a

apply :: Fn -> [Exp] -> Eval Exp
apply f args = f args

illegalCall :: Eval a
illegalCall = lift $ throwError IllegalCall

unknownSymbol :: Eval a
unknownSymbol = lift $ throwError UnknownSymbol

bindedIdent :: Eval a
bindedIdent = lift $ throwError BindedIdent

defaultEnv :: Env
defaultEnv = Env $ Map.mapKeys Ident $ Map.fromList $
    [("nil", nil)
   , ("car", car)
   , ("cdr", cdr)
   , ("cons", cons)
   , ("list", list)
   , ("eval", ev)
   , ("cond", cond)
   , ("bind", bind)]
  where
    isAtom :: Exp -> Bool
    isAtom (StringExp _) = True
    isAtom (IntExp _)    = True
    isAtom (DoubleExp _) = True
    isAtom (QListExp _)  = True
    isAtom (SymbolExp _) = True
    isAtom _             = False

    nil :: Fn
    nil = const $ return $ SListExp []

    car :: Fn
    car [QListExp lst] = return $ head lst
    car _              = illegalCall

    cdr :: Fn
    cdr [QListExp lst] = return $ SListExp $ tail lst
    cdr _              = illegalCall

    cons :: Fn
    cons [fst, QListExp lst] = if isAtom fst
      then return $ SListExp $ fst : lst
      else illegalCall
    cons _ = illegalCall

    list :: Fn
    list lst = return $ SListExp lst

    ev :: Fn
    ev [QListExp lst]     = eval (SListExp lst)
    ev [QSymbolExp ident] = eval (SymbolExp ident)
    ev _                  = illegalCall

    cond :: Fn
    cond [] = return $ SListExp []
    cond ((QListExp [IntExp n, e]) : rest) = if n /= 0
      then return e
      else cond rest
    cond _ = illegalCall

    bind :: Fn
    bind [QSymbolExp ident, val@(QListExp lst)] = do
      fl <- bindSymbol ident $ const $ return val
      if fl
        then return $ SListExp []
        else bindedIdent
    bind _ = illegalCall

emptyEnv :: Env
emptyEnv = Env $ Map.empty

bindSymbol :: Ident -> Fn -> Eval Bool
bindSymbol id f = do
  (Env st) <- get
  case Map.lookup id st of
    Nothing -> (put $ Env $ Map.insert id f st) >> return True
    Just _  -> (put $ Env st) >> return False

getSymbol :: Ident -> Eval Fn
getSymbol id = do
  (Env st) <- get
  case Map.lookup id st of
    Nothing  -> unknownSymbol
    Just exp -> return exp

continue :: Exp -> Env -> (Either EvalErr (Exp, Env))
continue exp env = runExcept $ runStateT (eval exp) env

eval :: Exp -> Eval Exp
eval val@(StringExp str)    = return val
eval val@(IntExp int)       = return val
eval val@(DoubleExp double) = return val
eval val@(QListExp qlist)   = return val
eval val@(SymbolExp ident)  = getSymbol ident >>= (\f -> apply f [])
eval val@(QSymbolExp ident) = return val
eval val@(SListExp [])      = return val
eval (SListExp [exp])       = eval exp
eval (SListExp ((SymbolExp ident) : args)) = do
  tail <- mapM eval args
  f <- getSymbol ident
  apply f tail
eval (SListExp _)           = illegalCall
