-- data
data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)
data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate
  { candidateId :: Int
  , codeReview :: Grade
  , cultureFit :: Grade
  , education :: Degree } deriving Show


-- actions
assessCandidateIO :: IO String
assessCandidateIO = do
  candidate <- readCandidate
  let passed = viable candidate
  let statement = if passed
                  then "passed"
                  else "failed"
  return statement

readCandidate :: IO Candidate
readCandidate = do
  putStrLn "enter id:"
  cId <- readInt
  putStrLn "enter code grade:"
  codeGrade <- readGrade
  putStrLn "enter culture fit grade:"
  cultureGrade <- readGrade
  putStrLn "enter education"
  degree <- readDegree
  return (Candidate { candidateId = cId
                    , codeReview = codeGrade
                    , cultureFit = cultureGrade
                    , education = degree })

-- calculations
viable :: Candidate -> Bool
viable candidate = all (== True) tests
  where passedCoding = codeReview candidate > B
        passedCultureFit = cultureFit candidate > C
        educationMin = education candidate >= MS
        tests = [passedCoding
                ,passedCultureFit
                ,educationMin]

readInt :: IO Int
readInt = getLine >>= (return . read)

readGrade :: IO Grade
readGrade = getLine >>= (return . read)

readDegree :: IO Degree
readDegree = getLine >>= (return . read)
