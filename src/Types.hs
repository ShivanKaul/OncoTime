module Types where

import qualified Data.Map as M
import Text.Parsec.Pos


--data Annotation = Annotation (TypeAn, SourcePos) deriving (Show, Eq)

data Annotation = Annotation (TypeAn) deriving(Show, Eq)

type TypeAn = String


--TEST PROGRAM
data TestProgram a = TestHeader (Header a) | TestDocs Docs | TestUseFileList [UseFile]
    | TestGroupList [(GroupDefs a)] | TestComputation [(Computation a)] 
    | TestFiltersList [(Filter a)] deriving(Show,Eq)

--sans annotations
data Program a = Program (Header a) Docs [UseFile]  [(GroupDefs a)] [(Filter a)] [(Computation a)]
      deriving (Show, Eq)   

data TestProgram2 a = TestProgram2 (Header a) Docs [UseFile]  [(GroupDefs a)] [(Filter a)] [(Computation a)]
      deriving (Show, Eq)     
      
data Header a = Header FileName [Arg a]
      deriving (Show, Eq)

data Arg a = Arg GroupType (Var a) deriving (Show, Eq)
data Var a = Var String a deriving (Show, Eq)

type FileName = String
type FileExt = String
data Docs = Docs String deriving (Show, Eq)

--Use
data UseFile = UseFile [String] deriving (Show, Eq)

--Group
type GroupList a = [(GroupDefs a)] 
data GroupDefs a = Group GroupType (Var a) [(GroupItem a)] -- different params?
    deriving (Show, Eq)

data GroupType = GroupType String deriving (Show, Eq)
data GroupItem a = GroupValString String a | GroupVar (Var a) | GroupRange (RangeType a) | GroupDate IntValue IntValue IntValue deriving (Show, Eq)

--better field for GroupVal than string?

data Filter a =  Filter FilterName [(FieldDef a)] deriving (Show, Eq)  
data FieldDef a = FieldDef FieldName [(FieldVal a)] deriving (Show, Eq)
type FilterName = String
--data FilterVal = FilterString String | FilterVar Var |FilterRange RangeType  -- to be defined in config
type FieldVal a = (GroupItem a)-- deriving (Show, Eq)
--data FilterField = FilterField String deriving (Show, Eq)

data RangeType a = Before IntValue a | After IntValue a | Between IntValue IntValue a | SingleInt IntValue a
     deriving (Show, Eq)
    
type IntValue = Int 
type StringValue = String

data Computation a
    = Foreach (ForEachDef a) [(Computation a)] --for nested for loo, slide 38 is confusing
    | Table (Var a) FilterName (FieldName)
    | List (Var a) [(SeqField a)]
    | Print (PrintAction a)
    | Barchart (Var a)
    deriving (Show, Eq)
   
data PrintAction a 
     = PrintVar (Var a)
     | PrintTimeLine (Var a)--slide 38
     | PrintLength (Var a)
     | PrintFilters [FilterName] (Var a)
     | PrintElement (Var a) (Var a) --like array indexing, slide 39
     deriving (Show, Eq)

--type Sequence a = [[(SeqField a){- separated by ->-}] {-separated by | -}] 

type EventName = String


data Event a
    = Event (EventName) a
    deriving(Show,Eq)

data SeqField a
    = Single (Event a)
    | Disj [(Event a)]
    | Star [(Event a)]
    | Neg (Event a)
    | Comma [(Event a)]
    | Bar [(Event a)]
    deriving(Show,Eq)

     
data ForEachDef a 
     = ForEachFilter FilterName (Var a)
     | ForEachTable (Var a) (Var a)
     | ForEachSequence (Var a) [(SeqField a)] 
     | ForEachList (Var a) (Var a)
     deriving (Show, Eq)

data LexError = FieldNameError String | FilterNameError String | AllowedTypeError String | UndefinedVariable String | AllowedValError String | FieldNotFoundError String | GenError String | MissingFilesError String | MissingConfigFile String | RedecError String | MissingConfigField String | TypeError String | IncorrectEvent String | ComputationTypeMismatch String | ComputationWrongScope String deriving (Show, Eq)


--we should also define a list of aliases perhaps that we pass
data Config =  Config (M.Map (FilterName, Bool) FieldMap) deriving(Eq, Show)

data FieldMap = FieldMap (M.Map FieldName (Field)) deriving(Eq, Show)

data Field = FieldType String | FieldValue [AllowedVal]  deriving(Eq, Show)

--type Field = (AllowedType, [AllowedVal])

type Loopable = Bool

type FieldName = String
type AllowedType = String
type AllowedVal = String


data ComputationType  
     = TTable
     | TList
     | TFilter String
     | TIndex
     | TSequence
     deriving(Eq, Show)
