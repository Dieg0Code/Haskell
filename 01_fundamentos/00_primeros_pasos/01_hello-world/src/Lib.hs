module Lib
  ( sumar,
    restar,
    multiplicar,
    dividir,
    potencia,
    areaCirculo,
    areaRectangulo,
    raizCuadrada,
    factorial,
    esPar,
    esPositivo,
    celsiusAFahrenheit,
    metrosAPies,
    mostrarCalculadora,
  )
where

-- ========================================
-- OPERACIONES BÁSICAS
-- ========================================

-- Suma de dos números
sumar :: Double -> Double -> Double
sumar x y = x + y

-- Resta de dos números
restar :: Double -> Double -> Double
restar x y = x - y

-- Multiplicación de dos números
multiplicar :: Double -> Double -> Double
multiplicar x y = x * y

-- División de dos números (cuidado con el cero)
dividir :: Double -> Double -> Double
dividir x y = x / y

-- Potencia (base elevada a exponente)
potencia :: Double -> Double -> Double
potencia base exponente = base ** exponente

-- Raíz cuadrada (usando la función sqrt)
raizCuadrada :: Double -> Double
raizCuadrada x = sqrt x

-- Factorial (usando recursión)
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- ========================================
-- FUNCIONES GEOMÉTRICAS
-- ========================================

-- Área de un círculo
areaCirculo :: Double -> Double
areaCirculo radio = pi * radio * radio

-- Área de un rectángulo
areaRectangulo :: Double -> Double -> Double
areaRectangulo largo ancho = largo * ancho

-- =========================================
-- FUNCIONES DE VALIDACIÓN
-- =========================================

-- Validar si un número es par
esPar :: Int -> Bool
esPar n = n `mod` 2 == 0 -- o también: mod n 2 == 0

-- Validar si un número es positivo
esPositivo :: Double -> Bool
esPositivo x = x > 0

-- =========================================
-- FUNCIONES DE CONVERSION DE UNIDADES
-- =========================================

-- Convertir de Celsius a Fahrenheit
celsiusAFahrenheit :: Double -> Double
celsiusAFahrenheit c = (c * 9 / 5) + 32

-- Convertir de metros a pies
metrosAPies :: Double -> Double
metrosAPies m = m * 3.28084

-- ========================================
-- FUNCIÓN DEMO
-- ========================================

-- Función que muestra ejemplos de la calculadora
mostrarCalculadora :: IO ()
mostrarCalculadora = do
  putStrLn "=== 🧮 CALCULADORA HASKELL ==="
  putStrLn ""
  putStrLn "Operaciones básicas:"
  putStrLn ("5 + 3 = " ++ show (sumar 5 3))
  putStrLn ("10 - 4 = " ++ show (restar 10 4))
  putStrLn ("6 * 7 = " ++ show (multiplicar 6 7))
  putStrLn ("15 / 3 = " ++ show (dividir 15 3))
  putStrLn ("2 ^ 8 = " ++ show (potencia 2 8))
  putStrLn ("Raíz cuadrada de 16 = " ++ show (raizCuadrada 16))
  putStrLn ("Factorial de 5 = " ++ show (factorial 5))
  putStrLn ("¿Es 4 par? " ++ show (esPar 4))
  putStrLn ("¿Es -3 positivo? " ++ show (esPositivo (-3)))
  putStrLn ""
  putStrLn "Funciones geométricas:"
  putStrLn ("Área círculo (radio=5): " ++ show (areaCirculo 5))
  putStrLn ("Área rectángulo (4x6): " ++ show (areaRectangulo 4 6))
  putStrLn ""
  putStrLn "Conversión de unidades:"
  putStrLn ("20°C a Fahrenheit: " ++ show (celsiusAFahrenheit 20))
  putStrLn ("10 metros a pies: " ++ show (metrosAPies 10))
  putStrLn ""
  putStrLn "=== FIN DE LA CALCULADORA ==="