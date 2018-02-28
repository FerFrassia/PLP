import Util
import Data.Maybe
import Data.List
import Test.HUnit

-- Ejercicio 1
singleton:: Eq t => t -> Anillo t
singleton b = A b proximo where
     proximo n = if n == b then Just b else Nothing

insertar :: Eq t => t -> Anillo t -> Anillo t
insertar e a = A (actual a) proximo where
     proximo n | n == (actual a) = Just e
               | n == e = (siguiente a (actual a))
               | otherwise = siguiente a n

avanzar :: Anillo t -> Anillo t
avanzar a = A (fromJust (siguiente a (actual a))) proximo where
     proximo n = siguiente a n

-- Ejercicio 2
enAnillo:: Eq t => t -> Anillo t -> Bool
enAnillo e a = e `elem` (aLista a)

aLista:: Eq t => Anillo t -> [t]
aLista a = (actual a) : (unfoldr (\e -> if e == actual(a) then Nothing else Just (e, fromJust(siguiente a e))) (actual (avanzar a)))

aAnillo:: Eq t => [t] -> Maybe (Anillo t)
aAnillo [] = Nothing
aAnillo (x:xs) = Just (avanzar(foldl (\a e-> avanzar(insertar e a)) (singleton x) xs))

-- Ejercicio 3
filterAnillo :: Eq t => (t -> Bool) -> Anillo t -> Maybe (Anillo t)
filterAnillo p a = aAnillo(filter p (aLista(a)))

-- Ejercicio 4
mapAnillo:: Eq a => Eq b => (a -> b) -> Anillo a -> Anillo b
mapAnillo f a =  A (f (actual a)) proximo where
     proximo b = Just (f (fromJust(siguiente a (invF f (aLista a) b))))

invF:: Eq a => Eq b => (a -> b) -> [a] -> b -> a
invF f l e = head(filter (\x -> f(x) == e) l)

--Ejercicio 5
palabraFormable :: String -> [Anillo Char] -> Bool
palabraFormable str lAnillos = presente ([ (str!!i, aLista(lAnillos!!i)) | i <- [0..length(lAnillos)-1] ])

presente :: [(Char, String)] -> Bool
presente l = all (\e -> e) (map (\e -> fst(e) `elem` snd(e)) l)

--Ejercicio 6
-- aclaración: decidimos distinguir a dos anillos que difieren solo en su elemento actual (aclaración del enunciado)
anillos:: Eq a => [a] -> [Anillo a]
anillos l =  aAnillos (tail(subseq l))

subseq:: Eq a => [a] -> [[a]]
subseq = foldr 
     (\e req -> req ++ [ (take i l) ++ [e] ++ (drop i l) | l <- req, i <- [0..length(l)] ])
     [[]]

aAnillos:: Eq a => [[a]] -> [Anillo a]
aAnillos l = [ fromJust (aAnillo listaA) | listaA <- l ]


-- Tests
-- Ej 1
tSingletonContainsA = TestCase (assertEqual "for tSingletonContainsA" 'a' (actual (singleton 'a')))
tSingletonHasOneElement = TestCase (assertEqual "for tSingletonHasOneElement" 'a' (fromJust (siguiente (singleton 'a') 'a')))
tInsertar = TestCase (assertEqual "for tInsertar" ['a','b'] (aLista (insertar 'b' (singleton 'a'))))
tAvanzar = TestCase (assertEqual "for tAvanzar" ['b','a'] (aLista (avanzar (insertar 'b' (singleton 'a')))))

-- Ej 2
tEnAnillo1 = TestCase (assertEqual "for tEnAnillo1" True (enAnillo 'b' (insertar 'b' (singleton 'a'))))
tEnAnillo2 = TestCase (assertEqual "for tEnAnillo2" False (enAnillo 'c' (insertar 'b' (singleton 'a'))))

-- Ej 3
tFilterAnillo1  = TestCase (assertEqual "for tFilterAnillo1" True (enAnillo 8 (fromJust (filterAnillo (>5) anilloEjemplo)))) 
tFilterAnillo2 = TestCase (assertEqual "for tFilterAnillo2" False (enAnillo 3 (fromJust (filterAnillo (>5) anilloEjemplo))))

-- Ej 4
tMapAnillo1 = TestCase (assertEqual "for tMapAnillo1" True (enAnillo 5 (mapAnillo (`mod` 6) anilloEjemplo)))
tMapAnillo2 = TestCase (assertEqual "for tMapAnillo2" True (enAnillo 2 (mapAnillo (`mod` 6) anilloEjemplo)))
tMapAnillo3 = TestCase (assertEqual "for tMapAnillo3" True (enAnillo 3 (mapAnillo (`mod` 6) anilloEjemplo)))
tMapAnillo4 = TestCase (assertEqual "for tMapAnillo4" True (enAnillo 1 (mapAnillo (`mod` 6) anilloEjemplo)))
tMapAnillo5 = TestCase (assertEqual "for tMapAnillo5" 10 (actual (mapAnillo (*2) anilloEjemplo)))

-- Ej 5
a1 = insertar 'l' (insertar 'f' (insertar 'q' (singleton 'a')))
a2 = insertar 'u' (singleton 'x')
a3 = insertar 'a' (insertar 'z' (singleton 'i'))
a4 = insertar 'n' (insertar 's' (insertar 'd' (singleton 'p')))
as = [a1, a2, a3, a4]
tPalabraFormable1 = TestCase (assertEqual "for tPalabraFormable1" True (palabraFormable "luis" as))
tPalabraFormable2 = TestCase (assertEqual "for tPalabraFormable2" False (palabraFormable "flan" as))
tPalabraFormable3 = TestCase (assertEqual "for tPalabraFormable3" True (palabraFormable "quid" as))

-- Ej 6
testAnillos = [singleton 'a',
               singleton 'b',
               singleton 'c',
               insertar 'b' (singleton 'a'),
               insertar 'a' (singleton 'b'),
               insertar 'c' (singleton 'b'),
               insertar 'b' (singleton 'c'),
               insertar 'a' (singleton 'c'),
               insertar 'c' (singleton 'a'),
               insertar 'b' (insertar 'c' (singleton 'a')),
               insertar 'c' (insertar 'b' (singleton 'a')),
               insertar 'a' (insertar 'c' (singleton 'b')),
               insertar 'c' (insertar 'a' (singleton 'b')),
               insertar 'a' (insertar 'b' (singleton 'c')),
               insertar 'b' (insertar 'a' (singleton 'c'))
               ]
tAnillos = TestCase (assertEqual "for tAnillos" (sort [ aLista a | a <- testAnillos]) (sort [ aLista a | a <- (anillos "abc")]))

-- en GHCI: runTestTT tests
tests = TestList [TestLabel "tSingletonContainsA" tSingletonContainsA, 
                  TestLabel "tSingletonHasOneElement" tSingletonHasOneElement,
                  TestLabel "tInsertar" tInsertar,
                  TestLabel "tAvanzar" tAvanzar,
                  TestLabel "tEnAnillo1" tEnAnillo1,
                  TestLabel "tEnAnillo2" tEnAnillo2,
                  TestLabel "tFilterAnillo1" tFilterAnillo1,
                  TestLabel "tFilterAnillo2" tFilterAnillo2,
                  TestLabel "tMapAnillo1" tMapAnillo1,
                  TestLabel "tMapAnillo2" tMapAnillo2,
                  TestLabel "tMapAnillo3" tMapAnillo3,
                  TestLabel "tMapAnillo4" tMapAnillo4,
                  TestLabel "tMapAnillo5" tMapAnillo5,
                  TestLabel "tPalabraFormable1" tPalabraFormable1,
                  TestLabel "tPalabraFormable2" tPalabraFormable2,
                  TestLabel "tPalabraFormable3" tPalabraFormable3,
                  TestLabel "tAnillos" tAnillos]



