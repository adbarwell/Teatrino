{-# LANGUAGE DeriveGeneric #-}
module Main where

import Parser (parseFile)

import BaseUtils ( dup, fromInt, ppPrint, secondM, ErrOr(Ok, Err) )
import Core ( Choice(label, cont), G(..) )
import Utils ( isCrashLabel )
import EffpiIR
    ( Effpi(mainf, types), MainFn(MkMain), RoleTy(MkRoleTy) )
import Effpi ( effpiG, effpiGIO, Verbosity(..))
import Scala ( toScala )

import Criterion.Main
    ( defaultMainWith,
      defaultConfig,
      bench,
      bgroup,
      nf,
      nfIO,
      Benchmark )
import Criterion.Types ( Config(resamples, csvFile, reportFile) )

import Numeric.Natural (Natural)
import System.FilePath ((</>))
import Control.Monad (void, (<=<))
import Data.Bifunctor (bimap)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as ByteString
import Data.Csv.Incremental
    (encodeDefaultOrderedByName, encodeNamedRecord) 
import Data.Csv (FromNamedRecord, ToNamedRecord, DefaultOrdered)
import System.Directory (createDirectoryIfMissing)

allExamples :: [String]
allExamples = [
    "a_PingPongAll",
    "b_PingPongNone",
    "c_AdderAll",
    "d_AdderNone",
    "e_TwoBuyerAll",
    "f_TwoBuyerR",
    "g_OAuthAll",
    "h_OAuthSA",
    "i_OAuthS",
    "j_OAuthNone",
    "k_TravelAll",
    "l_TravelAS",
    "m_TravelA",
    "n_LoggerAll",
    "o_LoggerIC",
    "p_LoggerI",
    "q_CBreakerAll",
    "r_CBreakerAS",
    "s_CBreakerASRec"
  ]

mkScrPath :: String -> FilePath
mkScrPath name' = "scribble" </> (name' ++ ".nuscr")

allGlobalTypes :: IO [(String, G ())]
allGlobalTypes =
  mapM (secondM (f <=< parseFile . mkScrPath) . dup) allExamples where
    f (Err err) = error ("Error getting global type: " ++ err)
    f (Ok g) = return g

allEffpi :: IO [(String, Effpi)]
allEffpi = map f <$> allGlobalTypes where
  f (name', g) = case effpiG name' g of
    Err err -> error ("Error generating Effpi: " ++ err)
    Ok epi -> (name', epi)

-- [Benchmarking] -------------------------------------------------------------

-- | Parsing benchmark.
mkParseBench :: String -> Benchmark
mkParseBench name' =
  let path = mkScrPath name' in
    bench name' (nfIO (void (parseFile path)))

-- | EffpiIR Generation Benchmark. Includes projection.
mkEffpiBench :: String -> G () -> Benchmark
mkEffpiBench name' g = bench name' (nf (effpiG name') g)

-- | Code generation benchmark. Does not include IO out operation.
mkScalaBench :: String -> Effpi -> Benchmark
mkScalaBench name' epi = bench name' (nf toScala epi)

-- | Full benchmark -- from parsing to writing Scala
mkTogetherBench :: String -> Benchmark
mkTogetherBench name' =
  bench name' (nfIO (parseFile (mkScrPath name') >>= f)) where
  f (Err err) = error ("Error parsing file: " ++ err)
  f (Ok g) = effpiGIO Silent name' g

-- defaultConfig = Config {
--       confInterval = cl95
--     , timeLimit    = 5
--     , resamples    = 1000
--     , regressions  = []
--     , rawDataFile  = Nothing
--     , reportFile   = Nothing
--     , csvFile      = Nothing
--     , jsonFile     = Nothing
--     , junitFile    = Nothing
--     , verbosity    = Normal
--     , template     = "default"
--     }
config :: Config
config = defaultConfig {resamples = 100,
                        csvFile = Just "benchmark/results.csv",
                        reportFile = Just "benchmark/report.html"}

-- [Metrics] ------------------------------------------------------------------

data Results = Results {
  name             :: String,  -- example
  numInteractions  :: Natural, -- no. of unique interactions in G
  numChans         :: Natural, -- no. of channels declared in main
  numFuns          :: Natural, -- no. of functions generated
  numCrashBranches :: Natural, -- no. of (nested) crash branches in G
  maxDepth         :: Natural  -- length of the longest continuation in G
} deriving (Show, Eq, Ord, Generic)

instance FromNamedRecord Results
instance ToNamedRecord Results
instance DefaultOrdered Results

getNumChans :: Effpi -> Natural
getNumChans epi = let MkMain cs _ = mainf epi in fromInt (length cs)

getNumCrashBranches :: G () -> Natural
getNumCrashBranches = fromInt . length . filter isCrashLabel . labels' where
  labels' (GComm _ _ _ ks) = concatMap f ks where
    f = uncurry (:) . bimap label (labels' . cont) . dup
  labels' (GRec _ k) = labels' k
  labels' _ = []

getMaxDepth :: G () -> Natural
getMaxDepth (GComm _ _ _ ks) = 1 + maximum (map (getMaxDepth . cont) ks)
getMaxDepth (GRec _ k) = 1 + getMaxDepth k
getMaxDepth (GVar _ _) = 1
getMaxDepth GEnd = 1

getNumInters :: G () -> Natural
getNumInters (GComm _ _ _ ks) = 1 + sum (map (getNumInters . cont) ks)
getNumInters (GRec _ k) = getNumInters k
getNumInters (GVar _ _) = 0
getNumInters GEnd = 0

getNumFuns :: Effpi -> Natural
getNumFuns epi =
  sum $ map (\(MkRoleTy _ _ _ xs) -> fromInt (length xs) + 1) (types epi)

getBasicMetrics :: IO [Results]
getBasicMetrics = map f <$> allGlobalTypes where
  f :: (String, G ()) -> Results
  f (n, g) = case effpiG n g of
    Err err -> error ("Error generating Effpi for metrics: " ++ err)
    Ok epi ->
      Results n (getNumInters g) (getNumChans epi) (getNumFuns epi) (getNumCrashBranches g) (getMaxDepth g) 

writeBasicMetrics :: FilePath -> [Results] -> IO ()
writeBasicMetrics path rs =
  let body = encodeDefaultOrderedByName (foldMap encodeNamedRecord rs)
  in ByteString.writeFile path body

-- [Entry Point] --------------------------------------------------------------

main :: IO ()
main = do
  gts <- allGlobalTypes
  eps <- allEffpi
  rs <- getBasicMetrics
  ppPrint rs
  writeBasicMetrics "benchmark/metrics.csv" rs
  createDirectoryIfMissing False "scala"
  defaultMainWith config [
      bgroup "ReadFile" (map mkParseBench allExamples),
      bgroup "EffpiGen" (map (uncurry mkEffpiBench) gts),
      bgroup "ScalaGen" (map (uncurry mkScalaBench) eps),
      bgroup "Together" (map mkTogetherBench allExamples)
    ]
