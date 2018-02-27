import Util
import Data.Maybe
import Data.List

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
-- aclaración: decidimos distinguir a dos anillos que difieren solo en su elemento actual
anillos:: Eq a => [a] -> [Anillo a]
anillos l =  aAnillos (tail(subseq l))

subseq:: Eq a => [a] -> [[a]]
subseq = foldr 
     (\e req -> req ++ [ (take i l) ++ [e] ++ (drop i l) | l <- req, i <- [0..length(l)] ])
     [[]]

aAnillos:: Eq a => [[a]] -> [Anillo a]
aAnillos l = [ fromJust (aAnillo listaA) | listaA <- l ]


--Tests
tSingleton = actual (anilloTest) == 'a' && fromJust (siguiente (anilloTest) 'a') == 'a'
             where anilloTest = singleton 'a'







