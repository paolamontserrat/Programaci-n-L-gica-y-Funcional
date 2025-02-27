module Ejercicios where

-- import Data.List
-- import Data.List (sort)
import Data.List (genericLength)
import Data.Char
import Data.Map (Map, fromList, toList)

-- 1 Aplicar descuento e IVA
type Cesta = [(String, Double, Double)] -- (Producto, Precio, Porcentaje)

aplicarDescuento :: Double -> Double -> Double
aplicarDescuento precio descuento = precio * (1 - descuento / 100)

aplicarIVA :: Double -> Double -> Double
aplicarIVA precio iva = precio * (1 + iva / 100)

calcularPrecioFinal :: Cesta -> (Double -> Double -> Double) -> Double
calcularPrecioFinal cesta funcion = sum [funcion precio porcentaje | (_, precio, porcentaje) <- cesta]

-- 2 Aplicar una función a una lista
aplicarALista :: (a -> b) -> [a] -> [b]
aplicarALista f xs = map f xs

-- 3 Contar palabras en una frase
contarPalabras :: String -> [(String, Int)]
contarPalabras frase = [(palabra, length palabra) | palabra <- words frase]

-- 4 Convertir notas a calificaciones
calificacion :: Double -> String
calificacion nota
    | nota >= 95 = "Excelente"
    | nota >= 85 = "Notable"
    | nota >= 75 = "Bueno"
    | nota >= 70 = "Suficiente"
    | otherwise  = "Desempeño insuficiente"

convertirNotas :: [(String, Double)] -> [(String, String)]
convertirNotas notas = [(map toUpper materia, calificacion nota) | (materia, nota) <- notas]

-- 5 Calcular módulo de un vector
moduloVector :: [Double] -> Double
moduloVector v = sqrt (sum (map (^2) v))

-- 6 Encontrar valores atípicos


-- Función para calcular la media de una lista de números
media :: [Double] -> Double
media xs = sum xs / fromIntegral (length xs)

-- Función para calcular la desviación típica de una lista de números
desviacionTipica :: [Double] -> Double
desviacionTipica xs = sqrt (sum (map (\x -> (x - m) ** 2) xs) / fromIntegral (length xs))
  where m = media xs

-- Función para encontrar valores atípicos basados en un rango
valoresAtipicos :: [Double] -> [Double]
valoresAtipicos xs
  | null xs = []  -- Si la lista está vacía, no hay valores atípicos
  | otherwise = filter (\x -> x < limiteInferior || x > limiteSuperior) xs
  where
    m = media xs
    d = desviacionTipica xs
    limiteInferior = m - 2 * d  -- Límite inferior del rango
    limiteSuperior = m + 2 * d  -- Límite superior del rango


