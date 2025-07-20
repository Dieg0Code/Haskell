-- |
-- Módulo      : Matematicas.Basicas
-- Descripción : Funciones matemáticas fundamentales
-- Copyright   : (c) Diego Obando, 2024
-- Licencia    : MIT
-- Mantenedor  : diego@ejemplo.com
-- Estabilidad : experimental
--
-- Este módulo proporciona funciones matemáticas básicas incluyendo
-- operaciones aritméticas avanzadas y cálculos de potencias.
--
-- == Ejemplo de uso:
--
-- >>> factorial 5
-- 120
--
-- >>> potencia 2 3
-- 8
--
-- >>> mcd 12 8
-- 4
module Matematicas.Basicas
  ( -- * Operaciones básicas
    cuadrado,
    cubo,
    potencia,

    -- * Factoriales y combinatorias
    factorial,
    fibonacci,

    -- * Números primos
    esPrimo,
    primos,

    -- * Máximo común divisor y mínimo común múltiplo
    mcd,
    mcm,

    -- * Operaciones con signo
    absoluto,
    signo,
  )
where

-- | Calcula el cuadrado de un número.
--
-- >>> cuadrado 5
-- 25
--
-- >>> cuadrado (-3)
-- 9
cuadrado :: (Num a) => a -> a
cuadrado x = x * x

-- | Calcula el cubo de un número.
--
-- >>> cubo 3
-- 27
--
-- >>> cubo (-2)
-- -8
cubo :: (Num a) => a -> a
cubo x = x * x * x

-- | Calcula una potencia usando exponenciación rápida.
--
-- Esta implementación es más eficiente que la multiplicación repetida
-- para exponentes grandes.
--
-- >>> potencia 2 10
-- 1024
--
-- >>> potencia 3 4
-- 81
--
-- >>> potencia 5 0
-- 1
potencia :: (Num a, Integral b) => a -> b -> a
potencia _ 0 = 1
potencia base exponente
  | exponente < 0 = error "Exponente negativo no soportado"
  | even exponente =
      let mitad = potencia base (exponente `div` 2)
       in mitad * mitad
  | otherwise = base * potencia base (exponente - 1)

-- | Calcula el factorial de un número entero no negativo.
--
-- El factorial de n (escrito n!) es el producto de todos los números
-- enteros positivos menores o iguales a n.
--
-- >>> factorial 0
-- 1
--
-- >>> factorial 5
-- 120
--
-- >>> factorial 10
-- 3628800
factorial :: Integer -> Integer
factorial n
  | n < 0 = error "El factorial no está definido para números negativos"
  | n == 0 = 1
  | otherwise = n * factorial (n - 1)

-- | Calcula el n-ésimo número de Fibonacci.
--
-- La secuencia de Fibonacci se define como:
-- F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
--
-- >>> fibonacci 0
-- 0
--
-- >>> fibonacci 1
-- 1
--
-- >>> fibonacci 10
-- 55
fibonacci :: Integer -> Integer
fibonacci n
  | n < 0 = error "Fibonacci no está definido para números negativos"
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fibonacci (n - 1) + fibonacci (n - 2)

-- | Verifica si un número es primo.
--
-- Un número primo es un entero mayor que 1 que no tiene divisores
-- positivos distintos de 1 y él mismo.
--
-- >>> esPrimo 2
-- True
--
-- >>> esPrimo 17
-- True
--
-- >>> esPrimo 15
-- False
esPrimo :: Integer -> Bool
esPrimo n
  | n < 2 = False
  | n == 2 = True
  | even n = False
  | otherwise = not (any (\x -> n `mod` x == 0) [3, 5 .. isqrt n])
  where
    isqrt = floor . sqrt . fromIntegral

-- | Genera una lista infinita de números primos.
--
-- Usa la criba de Eratóstenes para generar primos eficientemente.
--
-- >>> take 10 primos
-- [2,3,5,7,11,13,17,19,23,29]
primos :: [Integer]
primos = criba [2 ..]
  where
    criba (p : xs) = p : criba [x | x <- xs, x `mod` p /= 0]
    criba [] = []

-- | Calcula el máximo común divisor de dos números.
--
-- Utiliza el algoritmo de Euclides para encontrar el MCD.
--
-- >>> mcd 12 8
-- 4
--
-- >>> mcd 17 13
-- 1
--
-- >>> mcd 100 25
-- 25
mcd :: Integer -> Integer -> Integer
mcd a 0 = abs a
mcd a b = mcd b (a `mod` b)

-- | Calcula el mínimo común múltiplo de dos números.
--
-- Se basa en la relación: MCM(a,b) = |a*b| / MCD(a,b)
--
-- >>> mcm 12 8
-- 24
--
-- >>> mcm 3 7
-- 21
mcm :: Integer -> Integer -> Integer
mcm a b = abs (a * b) `div` mcd a b

-- | Calcula el valor absoluto de un número.
--
-- >>> absoluto (-5)
-- 5
--
-- >>> absoluto 3
-- 3
absoluto :: (Num a, Ord a) => a -> a
absoluto x = if x < 0 then -x else x

-- | Determina el signo de un número.
--
-- Retorna 1 para positivos, -1 para negativos, 0 para cero.
--
-- >>> signo 5
-- 1
--
-- >>> signo (-3)
-- -1
--
-- >>> signo 0
-- 0
signo :: (Num a, Ord a) => a -> a
signo x
  | x > 0 = 1
  | x < 0 = -1
  | otherwise = 0