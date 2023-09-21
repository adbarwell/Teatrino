module IR where

import BaseUtils ( firstUpper, ErrOr(..), fromInt )
import ParserUtils ( reserved, ident )
import Core
    ( G(..), Choice(Choice), Label(..), Role(..), Reliability(U, R), B (..) )

import Numeric.Natural (Natural)
import Data.List (sort, nub, elemIndex)
import Data.Bifunctor (second)
import Text.ParserCombinators.Parsec (parse, try, (<|>))
import Data.Functor ((<&>))

-- [IR] -----------------------------------------------------------------------

data PRole = Reliable String | Unreliable String deriving Show
data PStmt = Rec String [PStmt]
           | Comm String String String String
           | Var String
           | IRChoice String [[PStmt]]
           deriving Show
data PG = MkPG String [PRole] [PStmt] deriving Show

-- [Conversion] ---------------------------------------------------------------

lookupLabel :: String -> [Label] -> ErrOr Label
lookupLabel "crash" _ = Ok CrashLab
lookupLabel str [] = Err ("can't find label: " ++ str)
lookupLabel str (l@(MkLabel _ s) : _) | str == s = Ok l
lookupLabel str (_ : ls) = lookupLabel str ls

parsePayload :: [(String, Natural)] -> String -> ErrOr B
parsePayload _ "" = Ok BUnit
parsePayload tys str = case parse payload' "" str of
  Left err -> Err (show err)
  Right ty -> Ok ty
  where
    payload' = do
      try (reserved "Int" >> return BInt)
      <|>
      try (reserved "Double" >> return BReal)
      <|>
      try (reserved "Real" >> return BReal)
      <|>
      try (reserved "String" >> return BString)
      <|>
      try (reserved "Bool" >> return BBool)
      <|>
      (ident <&> f)

    f str' = case lookup str' tys of
      Nothing -> error "custom type not found"
      Just i -> BType i str'

lookupRole' :: String -> [Role] -> ErrOr Role
lookupRole' str [] = Err ("can't find role: " ++ str)
lookupRole' str (p@(MkRole _ s _) : ps) | str == s = Ok p
                                        | otherwise = lookupRole' str ps

stripPRole :: PRole -> String
stripPRole (Reliable str) = str
stripPRole (Unreliable str) = str

lookupRole :: PRole -> [Role] -> ErrOr Role
lookupRole pr = lookupRole' (stripPRole pr)

getDst :: String -> [Role] -> [PStmt] -> ErrOr Role
getDst p rs xs = foldr1 g (map f xs) where
  f (Comm _ _ p' q) | p == p' = lookupRole' q rs
  f _ = Err ("first statement in choice block must be a communication "
              ++ show xs)


  g (Ok x) (Ok z) | x == z = Ok z
                  | otherwise = Err "mixed choice not permitted"
  g _ (Err err) = Err err
  g (Err err) _ = Err err

toKS' :: [Role] -> [Label] -> [(String,Natural)] -> [String]
      -> [PStmt] -> ErrOr (Choice G ())
toKS' rs ls tys env (Comm l b _ _ : bs) = do
  label <- lookupLabel l ls
  payload <- parsePayload tys b
  fmap (Choice label payload) (toG' rs ls tys env bs)
toKS' _ _ _ _ _ =
  Err "first statement in choice block must be a communication"

toG' :: [Role] -> [Label] -> [(String,Natural)] -> [String] -> [PStmt]
     -> ErrOr (G ())
toG' _ _ _ _ [] = Ok GEnd
toG' rs ls tys env [Rec x b] = fmap (GRec ()) (toG' rs ls tys (x : env) b)
toG' _ _ _ env [Var x] = case elemIndex x env of
  Nothing ->
    Err ("Parser: can't find recursion variable in env: " ++ show (x, env))
  Just i -> Ok (GVar (fromInt i) ())
toG' rs ls tys env (Comm l b p q : bs) = do
  label <- lookupLabel l ls
  payload <- parsePayload tys b
  src <- lookupRole' p rs
  dst <- lookupRole' q rs
  k <- toG' rs ls tys env bs
  Ok (GComm src dst () [Choice label payload k])
toG' _ _ _ _ [IRChoice _ []] = error "no choice branches"
toG' rs ls tys env [IRChoice p bss] = do
  -- choice at P {choice at P {} or {}} or {} is not yet permitted
  -- assume normalised
  src <- lookupRole' p rs
  dst <- getDst p rs (map head bss)
  fmap (GComm src dst ()) (mapM (toKS' rs ls tys env) bss)

toG' _ _ _ _ bs = error ("Parser: invalid body: " ++ show bs)

toRoles :: [PRole] -> [Role]
toRoles = reverse . snd . foldl f (0, [])  where
  f (i, z) (Reliable r) = (i + 1, MkRole (fromInt i) r R : z)
  f (i, z) (Unreliable r) = (i + 1, MkRole (fromInt i) r U : z)

toLabels :: [String] -> [Label]
toLabels = reverse. snd . foldl f (0, []) where
  f (i, z) l = (i + 1, MkLabel i (firstUpper l) : z)

getStrLabels :: [PStmt] -> [String]
getStrLabels = sort . nub . concatMap f where
  f (Rec _ b) = getStrLabels b
  f (Comm l _ _ _) = [l]
  f (Var _) = []
  f (IRChoice _ bss) = concatMap getStrLabels bss

getStrCustomPayloads :: [PStmt] -> [String]
getStrCustomPayloads = sort . nub . concatMap f where
  f (Rec _ b) = getStrCustomPayloads b
  f (Comm _ b _ _) = [b]
  f (Var _) = []
  f (IRChoice _ bss) = concatMap getStrCustomPayloads bss

fromPG :: PG -> ErrOr (G ())
fromPG (MkPG _ rs b) =
  let roles = toRoles rs
      labs = toLabels (getStrLabels b)
      ctypes0 = getStrCustomPayloads b
      ctypes = zipWith (curry (second fromInt)) ctypes0 [0..length ctypes0]
  in toG' roles labs ctypes [] b
