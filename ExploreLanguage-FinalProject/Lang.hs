module Lang where

import Parsing
--import Lang (Expr(Let))

--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

type Identifier = String

data Value = NumV Int
           | BoolV Bool
           | StrV String               -- string values
           | ListV [Value]            -- list values
           | FnV [Identifier] Expr Env -- Parameters, Body, Closure
           deriving (Show,Eq)

type Env = [(Identifier,Value)]

data Expr = NumE Int
          | TrueE
          | FalseE
          | StrE String                      -- string literal
          | Op Operator Expr Expr
          | If Expr Expr Expr
          | FnDef [Identifier] Expr -- Parameters, Body
          | FnApp Expr [Expr]       -- Function expression
          | Id Identifier
          | Let [(Identifier, Expr)] Expr -- (let ((c ex1) (y e2)) body ))
          | Print Expr  -- print expression
          | ListE [Expr]          -- list expression
          deriving (Show, Eq)

data Operator = Plus
              | Mult
              | Equal
              | LessThan
              deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------
 
parens :: Parser a -> Parser a
parens p = do symbol "("
              exp <- p
              symbol ")"
              return exp

pExpr :: Parser Expr
pExpr = parens (pBuiltIn <|> pFnApp) <|> pLiteral <|> pId
  where pFnApp   = FnApp <$> pExpr <*> many pExpr
        pLiteral = pNum <|> pTrue <|> pFalse <|> pStr
        pNum     = NumE <$> natural
        pTrue    = symbol "#t" >> return TrueE
        pFalse   = symbol "#f" >> return FalseE
        pStr     = StrE <$> stringLiteral
        pId      = Id <$> identifier


-- parse a string like "hello world"
stringLiteral :: Parser String
stringLiteral = token $ do
  char '"'                                -- opening "
  content <- many (sat (/= '"'))          -- all chars until next "
  char '"'                                -- closing "
  return content


pBuiltIn :: Parser Expr
pBuiltIn =
  do sym <- identifier
     case sym of
       "if"     -> If <$> pExpr <*> pExpr <*> pExpr
       "lambda" -> FnDef <$> parens (many identifier) <*> pExpr
       "not"    -> If <$> pExpr <*> return FalseE <*> return TrueE
       "cond"   -> pCondCases
       "let"    -> pLet                   -- NEW
       "print"   -> Print <$> pExpr  
       "list"   -> ListE <$> many pExpr  
       _        -> if sym `elem` binarySymbols
                   then binaryParseTable sym <$> pExpr <*> pExpr
                   else failure

pCondCases :: Parser Expr
pCondCases =
  do symbol "("
     cnd <- pExpr
     thn <- pExpr
     symbol ")"
     if cnd == Id "else"
     then return thn
     else If cnd thn <$> pCondCases

-- simple let:
-- (let ((x e1) (y e2) ...) body)
-- each (x e1) is a "binding"

pLet :: Parser Expr
pLet = do
  symbol "("              -- start of bindings list
  binds <- many pBinding  -- read (x e) pairs
  symbol ")"              -- end of bindings list
  body <- pExpr           -- the body expression
  return (Let binds body)


pBinding :: Parser (Identifier, Expr)
pBinding = do
  symbol "("
  name <- identifier
  e    <- pExpr
  symbol ")"
  return (name, e)


binarySymbols :: [String]
binarySymbols = ["+", "*", "=", "<", "-", "and", "or", "<=", ">=", ">"]

binaryParseTable :: String -> Expr -> Expr -> Expr
binaryParseTable sym l r = case sym of
  "+"   -> Op Plus     l r
  "*"   -> Op Mult     l r
  "="   -> Op Equal    l r
  "<"   -> Op LessThan l r
  "-"   -> Op Plus l (Op Mult r (NumE (-1)))
  "and" -> If l r FalseE
  "or"  -> If l TrueE r
  "<="  -> If (Op LessThan l r) TrueE (Op Equal l r)
  ">="  -> If (Op LessThan r l) TrueE (Op Equal l r)
  ">"   -> Op LessThan r l

parseString :: String -> Expr
parseString s = case runParser pExpr s of
  [(x,"")] -> x
  _        -> error "*** not parsable"


--------------------------------------------------------------------------------
-- Interpreter
--------------------------------------------------------------------------------

opTable :: Operator -> Value -> Value -> Value
opTable Plus     (NumV x) (NumV y) = NumV  (x + y)
opTable Mult     (NumV x) (NumV y) = NumV  (x * y)
opTable LessThan (NumV x) (NumV y) = BoolV (x < y)
opTable Equal    x        y        = BoolV (x == y)
opTable op x y =
  error ("*** " ++ show op ++ " is incompatible with "
                  ++ show x ++ " and " ++ show y)

interp :: Env -> Expr -> Value
interp env expr =
  case expr of

    -- number literal
    NumE n ->
      NumV n

    -- boolean literals
    TrueE  ->
      BoolV True
    FalseE ->
      BoolV False

    -- string literal
    StrE s ->
      StrV s

    -- list expression
    ListE es ->
      ListV (map (interp env) es)


    -- identifier (variable or function name)
    Id x ->
      case lookup x env of
        Just v  -> v
        Nothing -> error ("undefined identifier: " ++ x)

    -- arithmetic / comparison / boolean ops
    Op op e1 e2 ->
      let v1 = interp env e1   -- evaluate left side
          v2 = interp env e2   -- evaluate right side
      in opTable op v1 v2      -- use the table we already have

    -- conditional
    If c t e ->
      case interp env c of
        BoolV True  -> interp env t
        BoolV False -> interp env e
        _           -> error "if: condition must be Bool"

    -- anonymous function: (lambda (x y ...) body)
    FnDef params body ->
      -- we do NOT run the body yet, we just package it
      -- together with the environment = closure
      FnV params body env

    
        -- list operations: head, tail, empty?, cons
    FnApp (Id "head") [e] ->
      case interp env e of
        ListV (x:_) -> x
        ListV []    -> error "head: empty list"
        v           -> error ("head: not a list: " ++ show v)

    FnApp (Id "tail") [e] ->
      case interp env e of
        ListV (_:xs) -> ListV xs
        ListV []     -> error "tail: empty list"
        v            -> error ("tail: not a list: " ++ show v)

    FnApp (Id "empty?") [e] ->
      case interp env e of
        ListV [] -> BoolV True
        ListV _  -> BoolV False
        v        -> error ("empty?: not a list: " ++ show v)

    FnApp (Id "cons") [e1, e2] ->
      let v1 = interp env e1
          v2 = interp env e2
      in case v2 of
           ListV xs -> ListV (v1 : xs)
           v        -> error ("cons: second arg must be list, got " ++ show v)

    -- generic function application 
    FnApp fnExpr argExprs ->
      let fnVal   = interp env fnExpr
          argVals = map (interp env) argExprs
      in case fnVal of
           FnV params body closureEnv ->
             if length params /= length argVals
               then error "wrong number of arguments to function"
               else
                 let newEnv = zip params argVals ++ closureEnv
                 in interp newEnv body
           _ ->
             error "trying to call a non-function value"
    
    -- let: evaluate bindings one-by-one, extending the env
    Let bindings body ->
      let
        -- extend environment with ONE binding at a time
        extendEnv currentEnv (name, expr') =
          let val = interp currentEnv expr'
          in (name, val) : currentEnv

        -- fold over all bindings in order
        env' = foldl extendEnv env bindings
      in interp env' body

    -- print expression
    Print e ->
     interp env e


 -- small helper so I can test my language from GHCi
run :: String -> Value
run s = interp [] (parseString s)

