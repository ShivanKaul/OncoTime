module Types where


--sans annotations
data Program = Program Header Docs UseFileList GroupList [Filter] [Computation]
      deriving (Show, Eq)
      
data Header = Header FileName ArgsList
      deriving (Show, Eq)

-- type?
type ArgsList = [Var]
type Var = String

type FileName = String
type Docs = String

--Come back to this
type UseFileList = [UseFile] 
type UseFile = String


--Group
type GroupList = [GroupDefs] 
data GroupDefs = Group GroupType Var [GroupItem]
    deriving (Show, Eq)
type GroupType = String
data GroupItem = GroupVal String | GroupVar Var | GroupRange RangeType  
     deriving (Show, Eq)

--better field for GroupVal than string?

data Filter =  Filter FilterName [FilterDef] deriving (Show, Eq) 
data FilterDef = FilterDef FilterField [FilterVal] deriving (Show, Eq)
type FilterName = String
--data FilterVal = FilterString String | FilterVar Var |FilterRange RangeType  -- to be defined in config
data FilterVal = GroupItem deriving (Show, Eq)
type FilterField = String

data RangeType = Before IntValue | After IntValue | Between IntValue IntValue 
     deriving (Show, Eq)
    
type IntValue = Int 
type StringValue = String

--TODO:
--IS COMPUTATION LIST MANDATORY?
data Computation
    = Foreach ForEacDef [Computation] --for nested for loops, slide 38 is confusing
    | Table TableAction
    | Sequence SeqAction
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
data SeqAction 
     = Seq Var EventList
     deriving (Show, Eq)

type EventList = [String]
     
data ForEacDef 
     = ForEachFilter FilterName Var 
     | ForEachTable Var Var
     | ForEachSequence Var
     | ForEachList Var Var
     deriving (Show, Eq)