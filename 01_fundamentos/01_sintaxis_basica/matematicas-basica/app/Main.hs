-- |
-- Aplicación de demostración para la biblioteca de matemáticas básicas.
--
-- Esta aplicación muestra ejemplos de uso de todas las funciones
-- implementadas en los módulos de matemáticas.
module Main where

import Matematicas.Basicas
import Matematicas.Estadistica
import Matematicas.Geometria

main :: IO ()
main = do
  putStrLn "🧮 Biblioteca de Matemáticas Básicas - Demostración"
  putStrLn "=" ++ replicate 50 '='

  demoMatematicasBasicas
  demoGeometria
  demoEstadistica

demoMatematicasBasicas :: IO ()
demoMatematicasBasicas = do
  putStrLn "\n📊 MATEMÁTICAS BÁSICAS:"
  putStrLn "------------------------"

  putStrLn $ "Cuadrado de 5: " ++ show (cuadrado 5)
  putStrLn $ "Cubo de 3: " ++ show (cubo 3)
  putStrLn $ "2^10 = " ++ show (potencia 2 10)
  putStrLn $ "Factorial de 6: " ++ show (factorial 6)
  putStrLn $ "Fibonacci(10): " ++ show (fibonacci 10)
  putStrLn $ "¿Es 17 primo? " ++ show (esPrimo 17)
  putStrLn $ "Primeros 10 primos: " ++ show (take 10 primos)
  putStrLn $ "MCD(12, 8): " ++ show (mcd 12 8)
  putStrLn $ "MCM(12, 8): " ++ show (mcm 12 8)

demoGeometria :: IO ()
demoGeometria = do
  putStrLn "\n📐 GEOMETRÍA:"
  putStrLn "--------------"

  putStrLn $ "Área círculo (r=5): " ++ show (areaCirculo 5.0)
  putStrLn $ "Perímetro círculo (r=5): " ++ show (perimetroCirculo 5.0)
  putStrLn $ "Área triángulo (3,4,5): " ++ show (areaTriangulo 3.0 4.0 5.0)
  putStrLn $ "Hipotenusa (3,4): " ++ show (hipotenusa 3.0 4.0)
  putStrLn $ "Área rectángulo (5x3): " ++ show (areaRectangulo 5.0 3.0)
  putStrLn $ "Volumen esfera (r=3): " ++ show (volumenEsfera 3.0)
  putStrLn $ "Distancia (0,0) a (3,4): " ++ show (distanciaEuclidiana (0, 0) (3, 4))

demoEstadistica :: IO ()
demoEstadistica = do
  putStrLn "\n📈 ESTADÍSTICA:"
  putStrLn "----------------"

  let datos = [1, 2, 3, 4, 5, 3, 4, 3]

  putStrLn $ "Datos: " ++ show datos
  putStrLn $ "Promedio: " ++ show (promedio datos)
  putStrLn $ "Mediana: " ++ show (mediana datos)
  putStrLn $ "Moda: " ++ show (moda datos)
  putStrLn $ "Varianza: " ++ show (varianza datos)
  putStrLn $ "Desviación estándar: " ++ show (desviacionEstandar datos)
  putStrLn $ "Rango: " ++ show (rango datos)
  putStrLn $ "Máximo: " ++ show (maximo datos)
  putStrLn $ "Mínimo: " ++ show (minimo datos)