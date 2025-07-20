-- |
-- Módulo      : Matematicas.Estadistica
-- Descripción : Funciones estadísticas básicas
-- Copyright   : (c) Diego Obando, 2024
--
-- Este módulo proporciona funciones para cálculos estadísticos
-- fundamentales.
module Matematicas.Estadistica
  ( -- * Medidas de tendencia central
    promedio,
    mediana,
    moda,

    -- * Medidas de dispersión
    varianza,
    desviacionEstandar,
    rango,

    -- * Otras funciones útiles
    sumatoria,
    productoria,
    maximo,
    minimo,
  )
where

import Data.List (group, maximumBy, minimumBy, sort)
import Data.Ord (comparing)

-- | Calcula el promedio aritmético de una lista de números.
--
-- >>> promedio [1, 2, 3, 4, 5]
-- 3.0
--
-- >>> promedio [10, 20, 30]
-- 20.0
promedio :: [Double] -> Double
promedio [] = error "No se puede calcular el promedio de una lista vacía"
promedio xs = sum xs / fromIntegral (length xs)

-- | Calcula la mediana de una lista de números.
--
-- >>> mediana [1, 2, 3, 4, 5]
-- 3.0
--
-- >>> mediana [1, 2, 3, 4]
-- 2.5
mediana :: [Double] -> Double
mediana [] = error "No se puede calcular la mediana de una lista vacía"
mediana xs =
  let ordenados = sort xs
      n = length ordenados
      medio = n `div` 2
   in if even n
        then (ordenados !! (medio - 1) + ordenados !! medio) / 2
        else ordenados !! medio

-- | Encuentra la moda (valor más frecuente) de una lista.
--
-- >>> moda [1, 2, 2, 3, 3, 3]
-- 3.0
moda :: [Double] -> Double
moda [] = error "No se puede calcular la moda de una lista vacía"
moda xs =
  let grupos = group (sort xs)
      masFrequente = maximumBy (comparing length) grupos
   in head masFrequente

-- | Calcula la varianza de una lista de números.
--
-- >>> varianza [1, 2, 3, 4, 5]
-- 2.0
varianza :: [Double] -> Double
varianza [] = error "No se puede calcular la varianza de una lista vacía"
varianza xs =
  let prom = promedio xs
      diferencias = map (\x -> (x - prom) ^ 2) xs
   in promedio diferencias

-- | Calcula la desviación estándar de una lista de números.
--
-- >>> desviacionEstandar [1, 2, 3, 4, 5]
-- 1.4142135623730951
desviacionEstandar :: [Double] -> Double
desviacionEstandar xs = sqrt (varianza xs)

-- | Calcula el rango (diferencia entre máximo y mínimo).
--
-- >>> rango [1, 5, 3, 9, 2]
-- 8.0
rango :: [Double] -> Double
rango [] = error "No se puede calcular el rango de una lista vacía"
rango xs = maximum xs - minimum xs

-- | Calcula la sumatoria de una lista de números.
--
-- >>> sumatoria [1, 2, 3, 4, 5]
-- 15.0
sumatoria :: [Double] -> Double
sumatoria = sum

-- | Calcula la productoria de una lista de números.
--
-- >>> productoria [1, 2, 3, 4]
-- 24.0
productoria :: [Double] -> Double
productoria = product

-- | Encuentra el valor máximo en una lista.
--
-- >>> maximo [1, 5, 3, 9, 2]
-- 9.0
maximo :: [Double] -> Double
maximo [] = error "No se puede encontrar el máximo de una lista vacía"
maximo xs = maximum xs

-- | Encuentra el valor mínimo en una lista.
--
-- >>> minimo [1, 5, 3, 9, 2]
-- 1.0
minimo :: [Double] -> Double
minimo [] = error "No se puede encontrar el mínimo de una lista vacía"
minimo xs = minimum xs