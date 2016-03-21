module Types where

import qualified Data.Map as M

--TEST PROGRAM
data TestProgram = TestHeader Header | TestDocs Docs | TestUseFileList [UseFile]
    | TestGroupList [GroupDefs] | TestComputation [Computation] 
    | TestFiltersList [Filter] deriving(Show,Eq)

--sans annotations
data Program = Program Header Docs [UseFile]  [GroupDefs] [Filter] [Computation]
      deriving (Show, Eq)   

data TestProgram2 = TestProgram2 Header Docs [UseFile]  [GroupDefs] [Filter] [Computation]
      deriving (Show, Eq)     
      
data Header = Header FileName [Arg]
      deriving (Show, Eq)

data Arg = Arg GroupType Var deriving (Show, Eq)
data Var = Var String deriving (Show, Eq)

type FileName = String
type FileExt = String
data Docs = Docs String deriving (Show, Eq)

--Use
data UseFile = UseFile [String] deriving (Show, Eq)

--Group
type GroupList = [GroupDefs] 
data GroupDefs = Group GroupType Var [GroupItem]
    deriving (Show, Eq)

data GroupType = GroupType String deriving (Show, Eq)
data GroupItem = GroupValString String | GroupVar Var | GroupRange RangeType | GroupDate IntValue IntValue IntValue deriving (Show, Eq)

--better field for GroupVal than string?

data Filter =  Filter FilterName [FieldDef] deriving (Show, Eq) 
data FieldDef = FieldDef FieldName [FieldVal] deriving (Show, Eq)
type FilterName = String
--data FilterVal = FilterString String | FilterVar Var |FilterRange RangeType  -- to be defined in config
type FieldVal = GroupItem -- deriving (Show, Eq)
--data FilterField = FilterField String deriving (Show, Eq)

data RangeType = Before IntValue | After IntValue | Between IntValue IntValue | SingleInt IntValue 
     deriving (Show, Eq)
    
type IntValue = Int 
type StringValue = String

data Computation
    = Foreach ForEachDef [Computation] --for nested for loops, slide 38 is confusing
    | Table Var FilterName FieldName
    | List Var Sequence
    | Print PrintAction
    | Barchart Var
    deriving (Show, Eq)
    

data PrintAction 
     = PrintVar Var
     | PrintTimeLine Var --slide 38
     | PrintLength Var
     | PrintFilters [FieldName] Var
     | PrintElement Var Var --like array indexing, slide 39
     deriving (Show, Eq)

type Sequence = [SeqField{- separated by ->-}]  

type EventName = String

data Event 
    = Event EventName
    deriving(Show,Eq)

data SeqField
    = Single Event 
    | Comma [Event]
    | Bar [Event]
    | Star [Event]
    | Neg Event
    deriving(Show,Eq)
     
data ForEachDef 
     = ForEachFilter FilterName Var 
     | ForEachTable Var Var
     | ForEachSequence Var Sequence
     | ForEachList Var Var
     deriving (Show, Eq)


data LexError   = FieldNameError String | FilterNameError String | AllowedTypeError String | UndefinedVariable String

                | AllowedValError String | FieldNotFoundError String | GenError String | MissingFilesError String 
                | MissingConfigFile String | RedecError String | MissingConfigField String | TypeError String | IncorrectEvent String
                | ComputationTypeMismatch String | ComputationWrongScope String deriving (Show, Eq)

--we should also define a list of aliases perhaps that we pass
data Config =  Config (M.Map (FilterName, Bool) FieldMap) deriving(Eq, Show)

data FieldMap = FieldMap (M.Map FieldName (Field)) deriving(Eq, Show)

data Field = FieldType String | FieldVal [AllowedVal]  deriving(Eq, Show)

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



