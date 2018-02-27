module Util where
import Data.Maybe

data Anillo t = A t (t -> Maybe t)

actual :: Anillo t -> t
actual (A e _) = e

siguiente:: Anillo t -> t -> Maybe t
siguiente (A _ s) = s

-- Ejemplo del enunciado

anilloEjemplo :: Anillo Integer
anilloEjemplo = A 5 proximo where
    proximo n   | n == 5 = Just 8
                | n == 8 = Just 3
                | n == 3 = Just 7
                | n == 7 = Just 5
                | otherwise = Nothing

anilloEjemplo2 :: Anillo Integer
anilloEjemplo2 = A 5 proximo where
    proximo n   | n == 5 = Just 8
                | n == 8 = Just 3
                | n == 3 = Just 7
                | n == 7 = Just 5
                | n == 9 = Just 9
                | otherwise = Nothing

-- Definiciones de Show

instance (Show t, Eq t) => Show (Anillo t) where
   show a =
    "\n" ++
    ((\(l1,l2,b) -> (if b
            then "  " ++ (showInterleaved "->" l1) ++ "->"
            else (" " ++ showInterleaved "->" l1))
        ++ "\n" ++ "|" ++ replicate (max (digitLength l1) (digitLength l2) + 2 * length l2) ' ' ++ "|"
        ++ "\n" ++ " " ++ (showInterleaved "<-" l2)) $
        (\l -> let h = div (length l) 2 in (take h l, reverse (drop h l), length l > 2 * h)) (a2L a))
    ++ "\n"

showInterleaved :: Show t => String -> [t] -> String
showInterleaved s l = foldr (\x r -> s ++ show x ++ r) "" l

digitLength :: Show t => [t] -> Int
digitLength = foldr (\x r -> (length $ show x) + r) 0

a2L :: Eq t => Anillo t -> [t]
a2L (A e s) =
    map fromJust $
    takeWhile (/= Nothing) $
    iterate (\m -> case m of
        Nothing -> Nothing
        Just v -> let Just x = s v in if x == e then Nothing else Just x) (Just e)
