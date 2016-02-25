module Types where

--TEST PROGRAM
data TestProgram = TestHeader Header | TestDocs Docs | TestUseFileList [UseFile]| TestGroupList [GroupDefs] | TestComputation [Computation] deriving(Show,Eq)

--sans annotations
data Program = Program Header Docs [UseFile]  [GroupDefs] [Filter] [Computation]
      deriving (Show, Eq)
     
      
data Header = Header FileName [Arg]
      deriving (Show, Eq)

data Arg = Arg GroupType Var deriving (Show, Eq)
data Var = Var String deriving (Show, Eq)

type FileName = String
type FileExt = String
data Docs = Docs String deriving (Show, Eq)

--Come back to this
type UseFileList = [UseFile] 
data UseFile = UseFile String deriving (Show, Eq)

--Group
type GroupList = [GroupDefs] 
data GroupDefs = Group GroupType Var [GroupItem]
    deriving (Show, Eq)

data GroupType = GroupType String deriving (Show, Eq)
data GroupItem = GroupVal String | GroupVar Var | GroupRange RangeType  
     deriving (Show, Eq)

--better field for GroupVal than string?

data Filter =  Filter FilterName [FilterDef] deriving (Show, Eq) 
data FilterDef = FilterDef FilterField [FilterVal] deriving (Show, Eq)
type FilterName = String
--data FilterVal = FilterString String | FilterVar Var |FilterRange RangeType  -- to be defined in config
type FilterVal = GroupItem -- deriving (Show, Eq)
data FilterField = FilterField String deriving (Show, Eq)

data RangeType = Before IntValue | After IntValue | Between IntValue IntValue 
     deriving (Show, Eq)
    
type IntValue = Int 
type StringValue = String

--TODO:
--IS COMPUTATION LIST MANDATORY?
data Computation
    = Foreach ForEachDef [Computation] --for nested for loops, slide 38 is confusing
    | Table TableAction
    | List Var Sequence
    | Print PrintAction
    | Barchart Var
    deriving (Show, Eq)
    
data TableAction
    = TableCount Var FilterName FilterVal 
    deriving (Show, Eq) 

data PrintAction 
     = PrintVar Var
     | PrintTimeLine Var --slide 38
     | PrintLength Var
     | PrintFilters [FilterName] Var
     | PrintElement Var Var --like array indexing, slide 39
     deriving (Show, Eq)

--TODO UNDERSTAND THIS
type Sequence = [[SeqField{- separated by ->-}] {-separated by | -}] 

type EventName = String

data Event 
    = EAll EventName
    | ESome EventName [Var]
    deriving(Show,Eq)

data SeqField
    = Single Event
    | Disj [Event]
    | Star SeqField
    | Neg Event
    deriving(Show,Eq)


     
data ForEachDef 
     = ForEachFilter FilterName Var 
     | ForEachTable Var Var
     | ForEachSequence Var Var
     | ForEachSequenceNoDef Var Sequence
     | ForEachList Var Var
     deriving (Show, Eq)
