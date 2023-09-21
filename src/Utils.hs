module Utils where

import Core
    ( B(BType),
      Label(..),
      Choice(label, payload, cont),
      G(GRec, GComm),
      Reliability(R),
      Role(..) )
import BaseUtils ( firstUpper )

import Data.List (sort,sortBy,nub)

isReliable :: Role -> Bool
isReliable (MkRole _ _ R) = True
isReliable _ = False

participants :: G a -> [Role]
participants = sort . nub . roles where
  roles (GComm p q _ ks) = p : q : concatMap (roles . cont) ks
  roles (GRec _ k) = roles k
  roles _ = []

isCrashLabel :: Label -> Bool
isCrashLabel CrashLab = True
isCrashLabel _ = False

isCrashBranch :: Choice a b -> Bool
isCrashBranch = isCrashLabel . label

labelsAndPayloads :: G a -> [(Label, B)]
labelsAndPayloads = sortBy g . nub . lAPs where
  g x y = compare (fst x) (fst y)
  f k | isCrashBranch k = lAPs (cont k)
      | otherwise = (label k, payload k) : lAPs (cont k)

  lAPs (GComm _ _ _ ks) = concatMap f ks
  lAPs (GRec _ k) = lAPs k
  lAPs _ = []

isCustomType :: B -> Bool
isCustomType (BType _ _) = True
isCustomType _ = False

payloads :: G a -> [B]
payloads = sort . nub . payloads' where
  payloads' (GComm _ _ _ ks) =
    concatMap (\k -> payload k : payloads' (cont k)) ks
  payloads' (GRec _ k) = payloads' k
  payloads' _ = []

toCClass :: Label -> Label
toCClass = mapLabel firstUpper where
  mapLabel :: (String -> String) -> Label -> Label
  mapLabel f (MkLabel i s) = MkLabel i (f s)
  mapLabel _ CrashLab = CrashLab

rmCrashLabs :: [Label] -> [Label]
rmCrashLabs = filter (not . isCrashLabel) 

setK :: Choice ty a -> ty b -> Choice ty b
setK x k' = x {cont = k'}

contM' :: Monad m => (ty a -> m (ty a)) -> Choice ty a -> m (Choice ty a)
contM' f x = fmap (setK x) (f (cont x))
