data Program
    = Program Header Docs UseList GroupList FilterList  ComputationList 
      deriving (Show, Eq)
      
data Header
    = HEmpty FName 
    | HArgs FName ArgsList
      deriving (Show, Eq)
      
type ArgsList = [String]

type FName = String
type Docs = String

type UseList = [String] 
 
type FilterList = [Filter]      
data Filter 
    = PopFilter
    | DocFilter
    | PeriodFilter
    | EventFilter
    deriving (Show, Eq)
    
type GroupList = [GroupDefs] 
data GroupDefs 
    = Group Var FieldValList
    deriving (Show, Eq)

-- field: {fval1,fval2}
-- |field: {fval1,<fdef1>}
--when fdef is found, merge defined group with current list?
data FieldVal 
     = FVal Value
     | FDef Var
     | FRange RangeType  
     deriving (Show, Eq)

data RangeType --?
     = Before Value
     | After Value
     | Between Value Value
     deriving (Show, Eq)
     
type Value = String     
type Var = String    
type FieldValList = [FieldVal]

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


type ComputationList  = [Computations]

data Computation
    = Foreach ForEacDef Computation --for nested for loops, slide 38 is confusing
    | Table TableAction
    | Sequence SeqAction
    | Print PrintAction
    | Barchart Var
    deriving (Show, Eq)
    
data TableAction
    = TAssoc Var CompCount
    = TNative Code --slide 41
    deriving (Show, Eq)
data CompCount = CCPat PatientField |  CCDoc DoctorField | CCDiag deriving (Show, Eq)
data PatientField = ID | Gender | Birthyear | Diagnosis | Postalcode deriving (Show, Eq)
data DoctorField = ID | Oncologist deriving (Show, Eq)
data PrintAction 
     = PVar Var
     = PTimeLine Var --slide 38
     = PLenght Var
     = PAssoc VAr Var --like array indexing, slide 39
     deriving (Show, Eq)
data SeqAction 
     = SEv Var  EventList
     | SNative Code --slide 41
     deriving (Show, Eq)
type Code = String
type EventList = [String]
     
type ForEacDef 
     = FAllVar CompType Var--foreach doctor d
     = FMem CompType Var Var --foreach member d of x
     deriving (Show, Eq)
type CompType = CTDiag | CTPat | CTDoc deriving (Show, Eq)

     
     


    
