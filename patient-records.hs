module Main where

-- Patient record type
data Patient = Patient
  { patientId      :: Int
  , name           :: String
  , birthDate      :: String
  , phone          :: String
  , email          :: String
  , address        :: String
  , medicalHistory :: [String]
  , allergies      :: [String]
  } deriving (Show, Read)

-- Sample patients
patients :: [Patient]
patients =
  [ Patient 1 "Makhaya M" "1990-01-24" "0797459822" "makhaya@email.com" "123 Main Street"
    ["Asthma", "Appendix removal 2010", "Hypertension in family"] ["Peanut allergy"]
  , Patient 2 "Charles Kunutu" "1985-05-10" "0823334444" "charles@email.com" "456 Oak Avenue"
    ["Diabetes", "None", "Heart disease in family"] ["No allergies"]
  , Patient 3 "Christ Brown" "2000-12-01" "0735556666" "christ@email.com" "789 Pine Road"
    ["None", "None", "No known family history"] ["Lactose intolerant"]
  ]

-- Display a single patient vertically
showPatient :: Patient -> IO ()
showPatient p = do
  putStrLn $ "Patient ID: " ++ show (patientId p)
  putStrLn $ "Name: " ++ name p
  putStrLn $ "Birthdate: " ++ birthDate p
  putStrLn $ "Phone: " ++ phone p
  putStrLn $ "Email: " ++ email p
  putStrLn $ "Address: " ++ address p
  putStrLn $ "Medical History: " ++ show (medicalHistory p)
  putStrLn $ "Allergies: " ++ show (allergies p)
  putStrLn "-----------------------------------"

-- List all patients
listPatients :: [Patient] -> IO ()
listPatients = mapM_ showPatient

-- Get the next available patient ID
nextId :: [Patient] -> Int
nextId [] = 1
nextId ps = maximum (map patientId ps) + 1

-- Add a patient with user input
addPatient :: [Patient] -> IO [Patient]
addPatient ps = do
  putStrLn "Enter patient name:"
  n <- getLine
  putStrLn "Enter birthdate (YYYY-MM-DD):"
  bd <- getLine
  putStrLn "Enter phone number:"
  ph <- getLine
  putStrLn "Enter email:"
  em <- getLine
  putStrLn "Enter address:"
  ad <- getLine
  putStrLn "Enter medical history (comma separated):"
  mh <- fmap (map strip . splitComma) getLine
  putStrLn "Enter allergies (comma separated):"
  al <- fmap (map strip . splitComma) getLine
  let newPatient = Patient (nextId ps) n bd ph em ad mh al
  putStrLn "Patient added successfully."
  return (ps ++ [newPatient])

-- Delete a patient by ID with user input
deletePatient :: [Patient] -> IO [Patient]
deletePatient ps = do
  putStrLn "Enter patient ID to delete:"
  pidStr <- getLine
  let pid = readSafe pidStr
  if any (\p -> patientId p == pid) ps
    then do
      putStrLn $ "Deleted patient with ID " ++ show pid
      return $ filter (\p -> patientId p /= pid) ps
    else do
      putStrLn "Patient not found."
      return ps

-- View a patient by ID with user input
viewPatient :: [Patient] -> IO ()
viewPatient ps = do
  putStrLn "Enter patient ID to view:"
  pidStr <- getLine
  let pid = readSafe pidStr
  case filter (\p -> patientId p == pid) ps of
    [] -> putStrLn "Patient not found."
    (p:_) -> showPatient p

-- Update a patient by ID with user input
updatePatient :: [Patient] -> IO [Patient]
updatePatient ps = do
  putStrLn "Enter patient ID to update:"
  pidStr <- getLine
  let pid = readSafe pidStr
  case filter (\p -> patientId p == pid) ps of
    [] -> putStrLn "Patient not found." >> return ps
    (p:_) -> do
      putStrLn "Leave blank to keep current value."
      putStrLn $ "Current name: " ++ name p
      n <- getLine
      putStrLn $ "Current birthdate: " ++ birthDate p
      bd <- getLine
      putStrLn $ "Current phone: " ++ phone p
      ph <- getLine
      putStrLn $ "Current email: " ++ email p
      em <- getLine
      putStrLn $ "Current address: " ++ address p
      ad <- getLine
      putStrLn $ "Current medical history: " ++ show (medicalHistory p)
      mh <- fmap (map strip . splitComma) getLine
      putStrLn $ "Current allergies: " ++ show (allergies p)
      al <- fmap (map strip . splitComma) getLine

      let updated = Patient
            pid
            (if null n then name p else n)
            (if null bd then birthDate p else bd)
            (if null ph then phone p else ph)
            (if null em then email p else em)
            (if null ad then address p else ad)
            (if null mh then medicalHistory p else mh)
            (if null al then allergies p else al)

      putStrLn "Patient updated successfully."
      return $ map (\x -> if patientId x == pid then updated else x) ps

-- Utilities
splitComma :: String -> [String]
splitComma [] = []
splitComma s = let (h,t) = break (==',') s in h : case t of { [] -> []; (_:r) -> splitComma r }

strip :: String -> String
strip = reverse . dropWhile (==' ') . reverse . dropWhile (==' ')

readSafe :: String -> Int
readSafe s = case reads s of
  [(x, _)] -> x
  _        -> -1

-- Menu loop
menu :: [Patient] -> IO ()
menu ps = do
  putStrLn "\nPatient Records Menu:"
  putStrLn "1. List all patients and their details"
  putStrLn "2. View patient by ID"
  putStrLn "3. Add patient"
  putStrLn "4. Delete patient"
  putStrLn "5. Update patient"
  putStrLn "6. Exit"
  putStrLn "Enter your choice:"
  choice <- getLine
  case choice of
    "1" -> listPatients ps >> menu ps
    "2" -> viewPatient ps >> menu ps
    "3" -> addPatient ps >>= menu
    "4" -> deletePatient ps >>= menu
    "5" -> updatePatient ps >>= menu
    "6" -> putStrLn "Exiting program."
    _   -> putStrLn "Invalid choice." >> menu ps

-- Main
main :: IO ()
main = do
  putStrLn "Welcome to the Patient Records System"
  menu patients
