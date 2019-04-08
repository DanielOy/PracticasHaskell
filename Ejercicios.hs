import qualified Data.Set as S
import qualified Data.Map as M

--1.Determina el resultado de un número x elevado a una potencia n (6 pts)
mathPow :: Int -> Int -> Int
mathPow x 0 = 1
mathPow x n = x * (mathPow x (n-1))


--2.Determina si un número n se encuentra en un rango determinado (6 pts)
ifrange :: Int -> String
ifrange n  = if n>=0 && n<=10 then "Dentro del rango 0-10" else "Fuera del rango 0-10"


--3.Dado un número entero en segundos, determinar la cantidad de horas, minutos y 
--segundos que contiene. (6 pts)
--secondsToDate :: Integer -> (Integer, Integer, Integer)
secondsToDate :: Integer -> String
secondsToDate xx = formatDate (horas, minutos, segundos)
    where 
    mm = div xx 60
    hh = div mm 60
    segundos = mod xx 60
    minutos = mod mm 60
    horas = mod hh 60

formatDate :: (Integer, Integer, Integer) -> String
formatDate (valor1,valor2,valor3) = show(valor1)++" Hora(s) "++show(valor2)++" Minuto(s) "++show(valor3)++" Segundo(s) "


--4.	Determine el mayor de 4 enteros (6 pts)
maxNumber :: Int -> Int -> Int -> Int -> String
maxNumber a b c d = if a>b && a>c && a>d then "Mayor primer numero valor: " ++show(a) else
    if b>a && b>c && b>d then "Mayor segundo numero valor: " ++show(b) else 
        if c>a && c>b && c>d then "Mayor tercer numero valor: "++show(c) else
            "Mayor cuarto numero valor: "++show(d)


--5.	Calcula la suma de una lista (arreglo) de elementos. (6 pts)
sumList :: [Int] -> Int
sumList (x:xs) = sum(x:xs)


--6.	Determina si un elemento dado está contenido en una lista. Devuelve verdadero o falso. (8 pts)
foundInList :: Int -> Bool
list = [2,4..20]
foundInList elem = any (==elem) list


--7.	Determina si dada una lista, ésta se encuentra ordenada. Se debe devolver verdadero o falso. (9 pts)
isListOrder :: Ord a =>[a] -> Bool
isListOrder [] = True
isListOrder [_] = True
isListOrder (x:y:xs)=(x<y) && isListOrder (y:xs) 


--8.	Dadas dos listas, determine si son iguales. Devolver verdadeo o falso. (9 pts)
isListEquals :: [Int] -> [Int] -> Bool
isListEquals (x:xs)(y:ys) = (x:xs)==(y:ys)


--9.	Realizar un función recursiva que retorne como salida el resultado de la suma 1 + 3 + 5 + 7 + 9 + N (10 pts)
sumSucesion :: Int -> Int
sumSucesion 0 = 0
sumSucesion num = num*2-1 + sumSucesion (num-1)


--10.	Realizar una función que reciba una lista y devuelva empleando recursividad otra lista de 
--      los elementos pares. (10 pts)
listPair :: [Int] -> [Int]
listPair xs = filter even xs

--11.	Realiza una función en Haskell que permita cargar calcular la unión, intersección y diferencia de 
--dos conjuntos datos. Para esto puede hacer uso de la librería “Data.set” (12 pts.)

conjuntos :: [Int] -> [Int] -> String
conjuntos xs ys = lunion ++ lintersecion ++ ldiferencia
    where
    lunion = "Union: "++ show(S.toAscList(S.union (S.fromList xs) (S.fromList ys)))
    lintersecion = " Interseccion: "++show(S.toAscList(S.intersection (S.fromList xs) (S.fromList ys)))
    ldiferencia = " Diferencia: "++show(S.toAscList(S.difference (S.fromList xs) (S.fromList ys)))

--12.	Realiza una funcón que permita definir un mapa de datos y permita encontrar un valor a partir de 
--su clave. Para esto puede hacer uso de la librería “Data.map” (12 pts.)
searchMap :: Integer -> Maybe String
d1 = M.fromList[(3,"Ana"),(4,"Juan"),(5,"Daniel")]   --Se define el mapa con los datos 
searchMap ws =  (M.lookup ws d1)