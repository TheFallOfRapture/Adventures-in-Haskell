import System.IO
import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cube as Cube
import qualified Geometry.Cuboid as Cuboid
import qualified Data.Set as Set

text1 = "This is a test sentence."
text2 = "Here is another test sentence."

set1 = Set.fromList text1
set2 = Set.fromList text2

data Shape = Circle Float Float Float | Rectangle Float Float Float Float

conjugate :: String -> String -> String
conjugate pronoun verb = case verbType of
    'a' -> conjugateA pronoun verb
    'e' -> conjugateE pronoun verb
    'i' -> conjugateI pronoun verb
    where verbType = (verb !! (length verb - 2))

conjugateA :: String -> String -> String
conjugateA pronoun verb = case pronoun of
    "yo" -> root ++ "o"
    "tu" -> root ++ "as"
    "el" -> root ++ "a"
    "ella" -> root ++ "a"
    "nosotros" -> root ++ "amos"
    "vosotros" -> root ++ "ais"
    "ustedes" -> root ++ "an"
    "ellos" -> root ++ "an"
    "ellas" -> root ++ "an"
    where root = init $ init verb

conjugateE :: String -> String -> String
conjugateE pronoun verb = case pronoun of
    "yo" -> root ++ "o"
    "tu" -> root ++ "es"
    "el" -> root ++ "e"
    "ella" -> root ++ "e"
    "nosotros" -> root ++ "emos"
    "vosotros" -> root ++ "eis"
    "ustedes" -> root ++ "en"
    "ellos" -> root ++ "en"
    "ellas" -> root ++ "en"
    where root = init $ init verb

conjugateI :: String -> String -> String
conjugateI pronoun verb = case pronoun of
    "yo" -> root ++ "o"
    "tu" -> root ++ "es"
    "el" -> root ++ "e"
    "ella" -> root ++ "e"
    "nosotros" -> root ++ "imos"
    "vosotros" -> root ++ "eis"
    "ustedes" -> root ++ "en"
    "ellos" -> root ++ "en"
    "ellas" -> root ++ "en"
    where root = init $ init verb

conjugateAll :: String -> [String]
conjugateAll [] = []
conjugateAll verb = map (`conjugate` verb) pronouns
    where pronouns = ["yo", "tu", "el", "nosotros", "vosotros", "ellos"]

conjugateWithPronoun :: String -> String -> String
conjugateWithPronoun pronoun verb = pronoun ++ " " ++ conjugate pronoun verb

conjugateTestAll :: String -> [String]
conjugateTestAll [] = []
conjugateTestAll verb = map (`conjugate` verb) pronouns
    where pronouns = ["yo", "tu", "el", "ella", "nosotros", "vosotros", "ustedes", "ellos", "ellas"]

pronouns = ["yo", "tu", "el", "ella", "nosotros", "vosotros", "ustedes", "ellos", "ellas"]
verbs = ["tocar", "comer", "escribir"]

main :: IO ()
main = do
    putStrLn "Start Program"
    putStrLn "Hello"
    putStrLn (Set.toList set1)
    putStrLn (Set.toList set2)
    putStrLn (Set.toList $ Set.union set1 set2)
    putStrLn (Set.toList $ Set.difference set1 set2)
    putStrLn (Set.toList $ Set.difference set2 set1)
    putStrLn (Set.toList $ Set.intersection set1 set2)
    putStrLn (show $ (map conjugateAll verbs))
