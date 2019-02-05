data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point
  deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
vplus (Vector i j k) (Vector l m n) = Vector (i + l) (j + m) (k + n)

dotp :: (Num a) => Vector a -> Vector a -> a
dotp (Vector i j k) (Vector l m n) = (i * l) + (j * m) + (k * n)

vmult :: (Num a) => Vector a -> a -> Vector a
vmult (Vector i j k) m = Vector (i * m) (j * m) (k * m)

-- person exercise
data Person = Person { firstName :: String
                      , lastName :: String
                      , age :: Int
                      } deriving (Eq, Show, Read)

mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}
adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}
mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}

mysteryDude =   "Person { firstName =\"Michael\"" ++
                ", lastName =\"Diamond\"" ++
                ", age = 43}"

data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
  deriving (Eq, Show, Read, Ord, Bounded, Enum)


----------------------------------------------------
type Name = String
type PhoneNumber = String
type PhoneBook = [(String, String)]

phonebook = [("heloise", "514-377-9450"), ("Jonathan", "514-706-3076")]
-- inPhoneBook :: String -> String -> [(String, String)] -> Bool
-- inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook
