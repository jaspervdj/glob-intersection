{-# LANGUAGE LambdaCase #-}
module Data.GlobIntersection
    ( Pattern
    , parse
    , intersects
    ) where

import           Data.Maybe (fromMaybe)

newtype Pattern a = Pattern [Token a] deriving (Show)

data Elem a
    = Singleton a
    | Range a a
    deriving (Eq, Show)

data Atom a
    = Dot
    | Char a
    | Set [Elem a]
    deriving (Eq, Show)

data Token a
    = Plus (Atom a)
    | Star (Atom a)
    | Atom (Atom a)
    deriving (Eq, Show)

unsafeAtom :: Token a -> Atom a
unsafeAtom = \case
    Plus a -> a
    Star a -> a
    Atom a -> a

-- | Parse a pattern from a string.
parse :: String -> Either String (Pattern Char)
parse = fmap (Pattern . simplify) . expr []
  where
    expr :: [Token Char] -> String -> Either String [Token Char]
    expr stack = \case
        [] -> pure $ reverse stack
        '\\' : [] -> Left "unexpected \\ at end"
        '\\' : c : t -> expr (Atom (Char c) : stack) t
        '*' : t -> case stack of
            Atom p : stack' -> expr (Star p : stack') t
            _               -> Left "unexpected *"
        '+' : t -> case stack of
            Atom p : stack' -> expr (Plus p : stack') t
            _               -> Left "unexpected +"
        '.' : t -> expr (Atom Dot : stack) t
        '[' : t -> do
            (c, t') <- classes [] t
            expr (Atom c : stack) t'
        x : t -> expr (Atom (Char x) : stack) t

    classes acc = \case
         []                            -> Left "expected ]"
         ']' :                       t -> Right (Set acc, t)
         '\\' : x : '-' : '\\' : y : t -> classes (Range x y : acc) t
         x        : '-' : '\\' : y : t -> classes (Range x y : acc) t
         '\\' : x : '-' :        y : t -> classes (Range x y : acc) t
         '\\' : x :                  t -> classes (Singleton x : acc) t
         x        : '-' :        y : t -> classes (Range x y : acc) t
         x    :                      t -> classes (Singleton x : acc) t

simplify :: Ord a => [Token a] -> [Token a]
simplify = \case
    Plus x : Star y : t | x == y -> simplify (Plus x : t)
    Star x : Plus y : t | x == y -> simplify (Plus x : t)
    Star x : Star y : t | x == y -> simplify (Star x : t)
    Plus x : Plus y : t | x == y -> Atom x : simplify (Plus y : t)
    x : t                        -> x : simplify t
    []                           -> []

elemsIntersect :: Ord a => Elem a -> Elem a -> Bool
elemsIntersect (Singleton x)   (Singleton y)   = x == y
elemsIntersect (Singleton x)   (Range lo hi)   = x >= lo && x <= hi
elemsIntersect (Range lo hi)   (Singleton y)   = y >= lo && y <= hi
elemsIntersect (Range lo0 hi0) (Range lo1 hi1) = hi0 >= lo1 && lo0 <= hi1

atomsIntersect :: Ord a => Atom a -> Atom a -> Bool
atomsIntersect Dot       _        = True
atomsIntersect _         Dot      = True
atomsIntersect (Char x)  (Set s)  = any (elemsIntersect (Singleton x)) s
atomsIntersect (Set s)   (Char x) = any (elemsIntersect (Singleton x)) s
atomsIntersect (Set s)   (Set k)  = any (\e -> any (elemsIntersect e) s) k
atomsIntersect (Char x)  (Char y) = x == y

-- Trim patterns for knownTokenEq tokens left and right.  Returns Nothing if
-- there is no match.
trimTokens :: Ord a => [Token a] -> [Token a] -> Maybe ([Token a], [Token a])
trimTokens ls0 rs0 = do
    (ls1, rs1) <- go ls0 rs0
    (ls2, rs2) <- go (reverse ls1) (reverse rs1)
    pure (reverse ls2, reverse rs2)
  where
    go (Atom x : xs) (Atom y : ys)
        | atomsIntersect x y = go xs ys
        | otherwise          = Nothing
    go xs             ys     = pure (xs, ys)

tokensIntersect :: Ord a => [Token a] -> [Token a] -> Bool
tokensIntersect [] [] = True
tokensIntersect [] (Atom _ : _) = False
tokensIntersect (Atom _ : _) [] = False
tokensIntersect (Atom x : xs) (Atom y : ys) =
    atomsIntersect x y && tokensIntersect xs ys
tokensIntersect (Plus _ : _) [] = False
tokensIntersect (Plus x : xs) (t : ts) =
    atomsIntersect x (unsafeAtom t) && tokensIntersect (Star x : xs) ts
tokensIntersect (Star _ : xs) [] =
    tokensIntersect xs []
tokensIntersect [Star x] (t : ts) =
    atomsIntersect x (unsafeAtom t) && tokensIntersect [Star x] ts
tokensIntersect (Star x : y : ys) (t : ts) =
    (atomsIntersect (unsafeAtom y) (unsafeAtom t) &&
        tokensIntersect (y : ys) (t : ts)) ||
    (atomsIntersect x (unsafeAtom t) && tokensIntersect (Star x : y : ys) ts)

tokensIntersect xs ys@(Star _ : _) = tokensIntersect ys xs
tokensIntersect xs ys@(Plus _ : _) = tokensIntersect ys xs

-- | Checks if there is an intersection between the two patterns.
intersects :: Ord a => Pattern a -> Pattern a -> Bool
intersects (Pattern l0) (Pattern r0) = fromMaybe False $ do
    (l1, r1) <- trimTokens l0 r0
    pure $ tokensIntersect l1 r1
