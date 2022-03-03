module LambdaCalculator(subst, freeVars, alphaEq, reduceOnce, nf, betaEq) where
  import Data.List
  import Control.Monad.Identity
  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Language
  import qualified Text.ParserCombinators.Parsec.Token as Token


  type Symb = String
  infixl 2 :@

  data Expr = Var Symb
            | Expr :@ Expr
            | Lam Symb Expr
            deriving Eq

  subst :: Symb -> Expr -> Expr -> Expr
  subst v n m = let
      freeN = freeVars n
      helper (Var s) linkM | s == v && not (s `elem` linkM) = n
                           | s /= v && s `elem` linkM && s `elem` freeN = Var $ s ++ "'"
                           | otherwise = Var s
      helper (a :@ b) linkM = helper a linkM :@ helper b linkM
      helper (Lam l e) linkM | v `elem` fLam && l `elem` freeN = Lam (l ++ "'") $ helper e (l:linkM)
                             | v `elem` fLam = Lam l $ helper e (l:linkM)
                             | otherwise = Lam l e
          where fLam = freeVars (Lam l e)
      in helper m []

  freeVars :: Expr -> [Symb]
  freeVars (Var x) = [x]
  freeVars (n :@ m) = freeVars n `union` freeVars m
  freeVars (Lam s e) = freeVars e \\ [s]

  makeNew :: Symb -> Expr -> Symb
  makeNew s e = helper s $ freeVars e
      where helper x l | x `elem` l = helper (x ++ "'") l
                       | otherwise = x

  alphaEq :: Expr -> Expr -> Bool
  alphaEq (Var a) (Var b) = a == b
  alphaEq (a :@ b) (n :@ m) = alphaEq a n && alphaEq b m
  alphaEq (Lam x e1) (Lam y e2) = alphaEq (subst x (Var z) e1) (subst y (Var z) e2)
      where 
          z = makeNew x (e1 :@ e2)
  alphaEq a b = False

  reduceOnce :: Expr -> Maybe Expr
  reduceOnce (Lam x e :@ n) = Just $ subst x n e
  reduceOnce (Lam x e) = case reduceOnce e of
      Nothing -> Nothing
      Just n -> Just $ Lam x n
  reduceOnce (a :@ b) = case reduceOnce a of
      Just m -> Just $ m :@ b
      Nothing -> case reduceOnce b of
          Nothing -> Nothing
          Just w -> Just $ a :@ w
  reduceOnce (Var x) = Nothing

  nf :: Expr -> Expr
  nf e = case reduceOnce e of
      Nothing -> e
      Just x -> nf x

  betaEq :: Expr -> Expr -> Bool
  betaEq e1 e2 = nf e1 `alphaEq` nf e2

  instance Show Expr where
      showsPrec _ (Var x) = showString x
      showsPrec _ (Var x :@ Var y) = showString x . showString " " . showString y
      showsPrec _ (Var x :@ n) = showString (x ++ " (") . shows n . showString ")"
      showsPrec _ (n :@ Var x) = shows n . showString (" " ++ x)
      showsPrec _ (n :@ m) = showString "(" . shows n . showString ") (" . shows m . showString ")"
      showsPrec _ (Lam x (Lam y n)) = showString ("\\" ++ x ++ " ") . showLambda (Lam y n)
          where showLambda (Lam x (Lam y n)) = showString (x ++ " ") . showLambda (Lam y n)
                showLambda (Lam y n) = showString (y ++ " -> ") . shows n
      showsPrec _ (Lam x n) = showString ("\\" ++ x ++ " -> ") . shows n

  instance Read Expr where
    readsPrec _ s =  [(parseExpr s, "")]

  languageDefinition :: GenLanguageDef String u Identity
  languageDefinition =
       emptyDef { Token.nestedComments = False,
                     Token.caseSensitive = True,
                     Token.identStart = noneOf "\\-> ()\n\t",
                     Token.identLetter = noneOf "\\-> ()\n\t",
                     Token.reservedOpNames = [ "\\", "->"]
                   }

  tokenParser = Token.makeTokenParser languageDefinition

  identifier = Token.identifier tokenParser
  reservedOp = Token.reservedOp tokenParser
  parens = Token.parens tokenParser
  whitespace = Token.whiteSpace tokenParser

  some v = (:) <$> v <*> many v

  parseExpr s = case parse parser "" s of
      Left t -> error $ show t
      Right expr -> expr

  parser :: Parser Expr
  parser = parseApplication <|> parseAbstraction

  parseApplication = do
      whitespace
      lstTerms <- some $ parseVars <|> parseParens
      whitespace
      return $ foldl1 (:@) lstTerms

  parseAbstraction = do
      reservedOp "\\"
      argLst <- parseVarLst
      reservedOp "->"
      expr <- parser
      return $ foldr (\(Var x) y -> Lam x y) expr $ Var <$> argLst

  parseParens = do
      term <- parens parser
      return term

  parseVars = do
      lstVar <- parseVarLst
      return $ foldl1 (\x y -> x :@ y) $ Var <$> lstVar

  parseVarLst = do
      lst <- some identifier
      return lst

