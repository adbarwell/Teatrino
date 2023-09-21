module Projection (proj, projAllRoles, Annot(..)) where

import Core ( Label(CrashLab), Choice(..), G(..), Role, S(..) )
import Utils ( isCrashBranch, participants, isReliable )

import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Bifunctor (second)
import BaseUtils (dup)

-- A type that is an annotation; mergeAnnot defines how annotations are merged 
class Annot a where
  mergeAnnot :: a -> a -> a

instance Annot () where
  mergeAnnot = const

proj :: (Annot a, Show a) => [Role] -> Role -> G a -> S a
proj res p g@(GComm q r v ks) | p == q = case mapMaybe f ks of
  [] -> error ("sends must have at least one continuation" ++ show g)
  ks' -> SSend r v ks'
  where
    f k | isCrashBranch k = Nothing
        | otherwise =
          case proj res p (cont k) of
            gi' -> Just (Choice (label k) (payload k) gi')
proj res p (GComm q r v ks) | p == r && prop = SRecv q v (map f ks) where
  prop = elem q res || elem CrashLab (map label ks)
  f k = Choice (label k) (payload k) (proj res p (cont k))
proj res p (GComm q r _ ks) | p /= q && p /= r =
  fullMerge (map (proj res p . cont) ks)
proj res p (GRec v g) = case proj res p g of
  SEnd -> SEnd
  g' -> SRec v g'
proj _res _p (GVar v t) = SVar v t
proj _res _p GEnd = SEnd
proj res p g = error ("undefined proj: " ++ show (res,p,g))

fullMerge :: (Annot a, Show a) => [S a] -> S a
fullMerge [] = error "Cannot merge an empty list of continuations."
fullMerge ks = foldr1 merge' ks where
  merge' :: (Annot a, Show a) => S a -> S a -> S a
  merge' (SRecv p v xs) (SRecv q w ys)
    | p == q = SRecv p (mergeAnnot v w) (both++xonly++yonly)
    where
      xonly = filter ((`notElem` map label ys) . label) xs
      yonly = filter ((`notElem` map label xs) . label) ys
      comp t u = compare (label t) (label u)
      both0 = sortBy comp (filter ((`elem` map label ys) . label) xs)
      both = map (f ys) both0

      find :: Label -> [Choice S a] -> Choice S a
      find l [] = error ("couldn't find label " ++ show l)
      find l (y : ks') | l == label y = y
                       | otherwise = find l ks'

      -- what happens when the basic type isn't the same?
      f :: (Annot a, Show a) => [Choice S a] -> Choice S a -> Choice S a
      f ys' x = let y = find (label x) ys' in
        if payload x /= payload y
          then error ("payload type differs: " ++ show (x,y))
          else Choice (label x) (payload x) (merge' (cont x) (cont y))
  merge' (SSend p v xs) (SSend q w ys) | p == q && prop =
    SSend p (mergeAnnot v w) zs where
      assoc x = (label x, payload x)
      comp x y = compare (fst x) (fst y)
      comp3 x y = compare (label x) (label y)
      prop = length xs == length ys
          && sortBy comp (map assoc xs) == sortBy comp (map assoc ys)
      zs = zipWith f (sortBy comp3 xs) (sortBy comp3 ys)
      f x y = Choice (label x) (payload y) (merge' (cont x) (cont y))
  merge' (SRec v s) (SRec w t) = SRec (mergeAnnot v w) (merge' s t)
  merge' (SVar n v) (SVar m w) | n == m = SVar n (mergeAnnot v w)
  merge' SEnd SEnd = SEnd
  merge' s t = error ("undefined case in merge: " ++ show (s,t))

projAllRoles :: (Annot a, Show a) => G a -> [(Role, S a)]
projAllRoles g = let rs = filter isReliable (participants g) in
  map (second (flip (proj rs) g) . dup) (participants g)
