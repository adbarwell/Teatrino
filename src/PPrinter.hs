module PPrinter ( ppG, ppS, ppRSList ) where

import Core ( Choice(Choice), G(..), Role, S(..) )
import BaseUtils (pfxStr, inc)

import Numeric.Natural (Natural)

-- [Core Functions] -----------------------------------------------------------

ppG' :: Show a => Natural -> G a -> IO ()
ppG' _ g@(GComm _ _ _ []) = error ("Invalid G: " ++ show g)
ppG' pfx (GComm p q _ [Choice l b k]) =
  let str = concat [show p, "->", show q, ":", show l, "<", show b, "> . "]
  in putStrLn (pfxStr pfx ++ str) >> ppG' pfx k
ppG' pfx (GComm p q _ ks) =
  let str = concat [show p, "->", show q, ":{"] in do
    putStrLn (pfxStr pfx ++ str)
    mapM_ (\(Choice l b k) ->
      let str' = concat [show l, "<", show b, "> . "] in do
        putStrLn (pfxStr (inc pfx) ++ str')
        ppG' (inc pfx) k
        putStrLn (pfxStr pfx ++ ",")
      ) ks
    putStrLn (pfxStr pfx ++ "}")
ppG' pfx (GRec _ k) = do
  putStrLn (pfxStr pfx ++ "μ {")
  ppG' (pfx+1) k
  putStrLn (pfxStr pfx ++ "}")
ppG' pfx (GVar n _) = do
  putStrLn (pfxStr pfx ++ "\\" ++ show n)
ppG' pfx GEnd = putStrLn (pfxStr pfx ++ "end")

ppS' :: Natural -> S () -> IO ()
ppS' _ s@(SRecv _ _ []) = error ("Invalid S: " ++ show s)
ppS' i (SRecv p _ [Choice l b k]) =
  let str = concat [show p, "&", show l, "<", show b, "> . "]
  in do
    putStrLn (pfxStr i ++ str)
    ppS' i k
ppS' i (SRecv p _ ks) =
  let str = show p ++ "&{" in do
    putStrLn (pfxStr i ++ str)
    mapM_ (\(Choice l b k) ->
      let str' = concat [show l, "<", show b, "> . "] in do
        putStrLn (pfxStr (i+1) ++ str')
        ppS' (i+1) k
        putStrLn (pfxStr i ++ ",")
      ) ks
    putStrLn (pfxStr i ++ "}")
ppS' _ s@(SSend _ _ []) = error ("Invalid S: " ++ show s)
ppS' i (SSend p _ [Choice l b k]) =
  let str = concat [show p, "!", show l, "<", show b, "> . "]
  in do
    putStrLn (pfxStr i ++ str)
    ppS' (i+1) k
ppS' i (SSend p _ ks) =
  let str = show p ++ "!{" in do
    putStrLn (pfxStr i ++ str)
    mapM_ (\(Choice l b k) ->
      let str' = concat [show l, "<", show b, "> . "] in do
        putStrLn (pfxStr (i+1) ++ str')
        ppS' (i+2) k
        putStrLn (pfxStr i ++ ",")
      ) ks
    putStrLn (pfxStr i ++ "}")
ppS' i (SRec _ k) = do
  putStrLn (pfxStr i ++ "μ {")
  ppS' (i+1) k
  putStrLn (pfxStr i ++ "}")
ppS' i (SVar n _) = do
  putStrLn (pfxStr i ++ "\\" ++ show n)
ppS' i SEnd = putStrLn (pfxStr i ++ "end")

-- [Interface Functions] ------------------------------------------------------

ppG :: G () -> IO ()
ppG = ppG' 0

ppS :: S () -> IO ()
ppS = ppS' 0

ppRSList :: [(Role, S ())] -> IO ()
ppRSList = mapM_ f where
  f (p, s) = do
    putStrLn ("Projecting on Role " ++ show p)
    ppS s
    putStrLn ""


