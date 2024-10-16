{- Semanal 7: Ejecicio 3 -}
module Semanal7 where

{- ocurrenciasElementosRC: Función RECURSIVA que toma como argumentos dos
    listas y devuelve una lista de parejas, en donde cada pareja contiene en su
    parte izquierda un elemento de la segunda lista y en su parte derecha el
    número de veces que aparece dicho elemento en la primera lista. -}
ocurrenciasElementosRc :: forall a. (Eq a, Show a) => [a] -> [a] -> [(a,Int)]
ocurrenciasElementosRc xs [] = []
ocurrenciasElementosRc xs (y:ys) = (y, length (filter (== y) xs)) : ocurrenciasElementosRc xs ys

{- ocurrenciasElementos: Función que toma como argumentos dos listas y
    devuelve una lista de parejas, en donde cada pareja contiene en su parte
    izquierda un elemento de la segunda lista y en su parte derecha el número
    de veces que aparece dicho elemento en la primera lista. -}
ocurrenciasElementos :: forall a. (Eq a, Show a) => [a] -> [a] -> [(a,Int)]
ocurrenciasElementos xs ys = ocurrenciasElementosTR xs (reverseTR ys []) []
-- Uso de reverse para que la salida sea identica.
    where
        {- ocurrenciasElementosTR: Función basada en RECURSIÓN DE COLA que toma como
            argumentos dos listas y devuelve una lista de parejas, en donde cada pareja
            contiene en su parte izquierda un elemento de la segunda lista y en su
            parte derecha el número de veces que aparece dicho elemento en la primera
            lista. 
            Debe iniciarse con ocurrenciasElementosTR _ _ [] para su correcto 
            funcionamiento. -}
        ocurrenciasElementosTR :: forall a. (Eq a, Show a) => [a] -> [a] -> [(a,Int)] -> [(a,Int)]
        ocurrenciasElementosTR _ [] acc = acc
        ocurrenciasElementosTR xs (y:ys) acc = ocurrenciasElementosTR xs ys ((y, occurencesTR y xs 0):acc)
        
{- occurencesTR: Función basada en recursión de cola que toma como argumentos 
    un elemento y una lista y devuelve la cantidad de veces que aparece el 
    elemento en la lista. 
    Debe iniciarse con occurencesTR _ _ 0, para su correcto funcionamiento. -}
occurencesTR :: Eq a => a -> [a] -> Int -> Int
occurencesTR _ [] acc = acc
occurencesTR x (y:ys) acc
    | x == y = occurencesTR x ys (acc + 1)
    | otherwise = occurencesTR x ys acc

{- reverseTR: Función basada en recursión de cola que toma como 
    argumento una lista y devuelve la lista invertida.
    Debe iniciarse con reverseTR _ [] para su correcto funcionamiento. -}
reverseTR :: [a] -> [a] -> [a]
reverseTR [] acc = acc
reverseTR (x:xs) acc = reverseTR xs (x:acc)