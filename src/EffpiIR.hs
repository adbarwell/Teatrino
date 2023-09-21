{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module EffpiIR where

import Numeric.Natural ( Natural )
import Data.List ( sort )

import Core ( B, Label )
import Projection ( Annot(..) )

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- [Type Aliases] -------------------------------------------------------------

type ProtocolName = String
type RVarSeed = Natural
type ChanSeed = Natural
type ChanID = Natural
type Prefix = Natural
type LambdaSeed = Natural
type BaseName = String

-- [Annotations] --------------------------------------------------------------

-- | Annotations on recursive variables
data RAnnot = RAChan | RARVar Natural deriving Show

-- | Annotations on channels (atomic- and set-) and recursive variables
data AnnotG = AChan ChanID [ChanID] -- ^ atomic-annotation
            | AMChan [ChanID] -- ^ set-annotation
            | ARVar Natural deriving Show

instance Eq AnnotG where
  (==) (AChan i is) (AChan j js) = i == j && sort is == sort js
  (==) (AMChan is) (AMChan js) = sort is == sort js
  (==) (ARVar i) (ARVar j) = i == j
  (==) _ _ = False

instance Annot AnnotG where
  mergeAnnot (AChan x []) (AChan y []) = AMChan [x,y]
  mergeAnnot (AMChan xs) (AChan y []) = AMChan (y : xs)
  mergeAnnot (AChan x []) (AMChan ys) = AMChan (x : ys)
  mergeAnnot (ARVar x) (ARVar y) | x == y = ARVar x
  mergeAnnot x y = error $ "error merging annotations: " ++ show (x,y)

isAChan :: AnnotG -> Bool
isAChan (AChan {}) = True
isAChan _ = False

isAMChan :: AnnotG -> Bool
isAMChan (AMChan {}) = True
isAMChan _ = False

isARVar :: AnnotG -> Bool
isARVar (ARVar _) = True
isARVar _ = False

-- | Atomic-annotations only
data AnnotS = ASChan ChanID [ChanID] (Maybe Natural)
            | ASRVar Natural
            deriving Show

isASChan :: AnnotS -> Bool
isASChan (ASChan {}) = True
isASChan _ = False

isASRVar :: AnnotS -> Bool
isASRVar (ASRVar _) = True
isASRVar _ = False

instance Eq AnnotS where
  (==) (ASChan i is _) (ASChan j js _) = i == j && sort is == sort js
  (==) (ASRVar i) (ASRVar j) = i == j
  (==) _ _ = False

instance Ord AnnotS where
  compare (ASChan i is n) (ASChan j js m) = compare (i,is,n) (j,js,m)
  compare (ASChan i _ _) (ASRVar j) = compare i j
  compare (ASRVar i) (ASChan j _ _) = compare i j
  compare (ASRVar i) (ASRVar j) = compare i j

-- [EffpiIR] ------------------------------------------------------------------

type Import = String

-- | Case classes
data CClass = MkCClass String (Maybe String) deriving (Show, Generic, NFData)

-- | Channel-label dictionary
data ChanMap = BaseChan Natural [Label]
             | MergeChan Natural [Natural] [Label]
             deriving (Show, Generic, NFData)

-- | Type upper-bound
data ChanUB = InChan Natural [Label]
            | OutChan Natural [Label]
            deriving (Show, Generic, NFData)

-- | Effpi recursion variable
data RecVar = MkRecVar Natural String deriving (Show, Generic, NFData)

-- | RHS of local type declarations and role-implementing functions
data TyBody = Out ChanUB Label B TyBody
            | In  ChanUB Label TyBody (Maybe TyBody)
            | Sel ChanUB [(Label, B, TyBody)]
            | Bra ChanUB Natural [Label] [ChanUB] (Maybe TyBody)
            | Rec RecVar TyBody
            | Var RecVar
            | End
            deriving (Show, Generic, NFData)

-- | New type/function declaration; branching continuations.
-- Think declarations in a 'where' block.
data TyDecl = Decl Natural String [ChanUB] [(Label, TyBody)]
            deriving (Show, Generic, NFData)

-- | Local type declaration/Role-implementing function
data RoleTy = MkRoleTy String [ChanUB] TyBody [TyDecl]
            deriving (Show, Generic, NFData)

-- | Entry point; contains channels and role-implementing function calls
data MainFn = MkMain [ChanMap] [(String, [ChanUB])]
            deriving (Show, Generic, NFData)

-- | An Effpi program
data Effpi = Effpi {
  pname :: String,
  ports :: [Import],
  cases :: [CClass],
  recvs :: [RecVar],
  types :: [RoleTy],
  mainf :: MainFn
} deriving (Show, Generic, NFData)

instance Eq ChanUB where
  (==) (InChan i _) (InChan j _) = i == j
  (==) (OutChan i _) (OutChan j _) = i == j
  (==) _ _ = False

instance Eq RecVar where
  (==) (MkRecVar i _) (MkRecVar j _) = i == j

instance Ord ChanUB where
  compare (InChan i _) (InChan j _) = compare i j
  compare (InChan i _) (OutChan j _) = compare i j
  compare (OutChan i _) (OutChan j _) = compare i j
  compare (OutChan i _) (InChan j _) = compare i j

