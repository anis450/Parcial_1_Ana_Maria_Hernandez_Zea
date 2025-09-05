import Data.List (sortBy)
import Data.Ord (comparing)


estudiantes :: [(String, Int)]
estudiantes =
    [ ("Ana", 90)
    , ("Carlos", 85)
    , ("Beatriz", 90)
    , ("Daniel", 75)
    ]


ordenados :: [(String, Int)]
ordenados =
    sortBy (comparing (\(nombre, nota) -> (-nota, nombre))) estudiantes

main :: IO ()
main = do
    putStrLn "Declarativo:"
    print ordenados
