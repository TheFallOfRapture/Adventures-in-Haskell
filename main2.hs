import System.IO
import qualified Math.Vector as Vec
import Math

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

data Person = Person {
    firstName :: String, lastName :: String, age :: Int, height :: Float, phoneNumber :: String, flavor :: String
} deriving (Show)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Show, Read, Ord, Bounded, Enum)

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ xs = xs
(x:-:xs) .++ ys = x :-: (xs .++ ys)

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Yellow == Yellow = True
    Green == Green = True
    _ == _ = False

mapList :: (a -> a) -> List a -> List a
mapList f (x:-:xs) = (f x) :-: mapList f xs

main :: IO ()
main = do
    putStrLn "Hello, world!"
    putStrLn (show $ surface $ Rectangle (Point 10 10) (Point 20 20))
    putStrLn (show $ Circle (Point 10 20) 5)
    putStrLn (show $ map (Circle (Point 10 20)) [2..6])
    putStrLn (show $ nudge (Circle (Point 10 20) 5) 2 2)
    putStrLn (show $ nudge (baseCircle 5) 2 2)
    putStrLn (show $ nudge (baseRectangle 3 5) 10 20)
    let testPerson = Person "Sam" "Pull" 18 1.70 "(800) 333-3333" "Vanilla"
    putStrLn (show $ age testPerson)
    putStrLn (show testPerson)
    putStrLn (show $ idNotZero 2)
    let today = read "Tuesday" :: Day
    putStrLn (show ([minBound..maxBound] :: [Day]))
    let customList = 1:-:2:-:3:-:4:-:5:-:Empty
    let customList2 = 1:-:2:-:Empty .++ 3:-:Empty
    let customList3 = customList .++ customList2
    putStrLn (show customList3)
    let light1 = Red
    let light2 = Red
    putStrLn (show $ light1 == light2)
    let vec1 = Vec.Vector (2, 3, 1) :: Vec.Vector Float
    let vec2 = Vec.Vector (1, 1, 1) :: Vec.Vector Float
    let vec3 = Vec.Vector ((2*), (3*), (4*))
    let g = vec3 <*> (Vec.Vector (3, 4, 5))
    let f = fmap (+) vec2
    putStrLn (show $ f <*> vec1)
    putStrLn (show g)
    x <- testFunc 2
    putStrLn (show x)
    y <- testFuncTwo 3
    z <- (\x -> putStrLn (show x ++ " IS THE INPUT NUMBER.") >> return (x * 2) >>= (\y -> putStrLn (show y ++ " IS THE OUTPUT NUMBER.") >> return y)) 2
    let vecStart = Vec.Vector (1, 1, 1)
    putStrLn ((show (vecStart + (Vec.Vector(2, 2, 2))) ++ " = " ++ show vecStart ++ " + " ++ show (Vec.Vector(2, 2, 2))))
    putStrLn $ show vecStart ++ " : STARTING MONADIC TRANSFORMATION."
    vecStart <- (\v -> putStrLn (show v ++ " IS THE INPUT VECTOR.") >> return (v + (Vec.Vector (1, 2, 3))) >>= (\v2 -> putStrLn (show v2 ++ " IS THE OUTPUT VECTOR.") >> return v2)) vecStart
    putStrLn $ show vecStart ++ " : ENDING MONADIC TRANSFORMATION."
    putStrLn (show (vecStart + (Vec.Vector(2, 2, 2))) ++ " = " ++ show vecStart ++ " + " ++ show (Vec.Vector(2, 2, 2)))
    putStrLn (show y)
    putStrLn "Bye!"

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x1 y1) r) dx dy = Circle (Point (x1 + dx) (y1 + dy)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) dx dy = Rectangle (Point (x1 + dx) (y1 + dy)) (Point (x2 + dx) (y2 + dy))

baseCircle :: Float -> Shape
baseCircle = (Circle (Point 0 0))

baseRectangle :: Float -> Float -> Shape
baseRectangle width height = (Rectangle (Point 0 0) (Point width height))

idNotZero :: Int -> Maybe Int
idNotZero x
    | x == 0 = Nothing
    | otherwise = Just x
