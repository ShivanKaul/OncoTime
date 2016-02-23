module Types where


--sans annotations
data Program = Program Header Docs UseList GroupList [Filter a] [Computations]
      deriving (Show, Eq)
      
data Header = Header FileName ArgsList
      deriving (Show, Eq)

-- type?
type ArgsList = [Var]
data Var = String

type FileName = String
type Docs = String

--Come back to this
type UseStmtList = [UseStmt] 
type UseStmt = String


type GroupList = [GroupDefs] 

data GroupDefs = Group (Var) [GroupItem]
    deriving (Show, Eq)


data GroupItem = GroupVal String | GroupVar Var | GroupRange RangeType  
     deriving (Show, Eq)

--better field for GroupVal than string?

data Filter a = Filter a

--data Filter = PopFilter | DocFilter | PeriodFilter | EventFilter
  --  deriving (Show, Eq)
{-
data PopFilter
    = PoID FieldValList
    | PoGender FieldValList
    | PoBirth FieldValList
    | PoDiagnosis FieldValList
    | PoPostalCode FieldValList
    deriving (Show, Eq)
    
data EventFilter
    = EvName FieldValList
    deriving (Show, Eq)

data PeriodFilter
    = PdYear FieldValList
    | PdMonth FieldValList
    | PdDay FieldValList
    | PdHour FieldValList
    deriving (Show, Eq)

-}
-- field: {fval1,fval2}
-- |field: {fval1,<fdef1>}
--when fdef is found, merge defined group with current list?

data RangeType = Before IntValue | After IntValue | Between IntValue IntValue
     deriving (Show, Eq)
     

type IntValue = Int 
--type FieldValList = [FieldVal]

data Computation
    = Foreach ForEacDef Computation --for nested for loops, slide 38 is confusing
    | Table TableAction
    | Sequence SeqAction
    | Print PrintAction
    | Barchart Var
    deriving (Show, Eq)
    
data TableAction
    = TAssoc Var CompCount
    | TNative Code --slide 41
    deriving (Show, Eq)

--data CompCount = CCPat PatientField |  CCDoc DoctorField | CCDiag deriving (Show, Eq)
--data PatientField = ID | Gender | Birthyear | Diagnosis | Postalcode deriving (Show, Eq)
--data DoctorField = ID | Oncologist deriving (Show, Eq)

data PrintAction 
     = PVar Var
     | PTimeLine Var --slide 38
     | PLenght Var
     | PAssoc VAr Var --like array indexing, slide 39
     deriving (Show, Eq)

data SeqAction 
     = SEv Var  EventList
     | SNative Code --slide 41
     deriving (Show, Eq)
type Code = String
type EventList = [String]
     
data ForEacDef 
     = ForAll CompType Var--foreach doctor d
     | ForMem CompType Var Var --foreach member d of x
     deriving (Show, Eq)


--data CompType = CTDiag | CTPat | CTDoc deriving (Show, Eq)

     
     


    
