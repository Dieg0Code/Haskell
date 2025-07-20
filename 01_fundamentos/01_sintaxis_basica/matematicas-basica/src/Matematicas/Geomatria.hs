-- |
-- Módulo      : Matematicas.Geometria
-- Descripción : Funciones geométricas básicas
-- Copyright   : (c) Diego Obando, 2024
--
-- Este módulo proporciona funciones para cálculos geométricos
-- básicos en 2D y 3D.
module Matematicas.Geometria
  ( -- * Círculos
    areaCirculo,
    perimetroCirculo,

    -- * Triángulos
    areaTriangulo,
    hipotenusa,

    -- * Rectángulos
    areaRectangulo,
    perimetroRectangulo,

    -- * Esferas
    volumenEsfera,
    superficieEsfera,

    -- * Distancias
    distanciaEuclidiana,
  )
where

-- | Calcula el área de un círculo dado su radio.
--
-- >>> areaCirculo 5.0
-- 78.53981633974483
--
-- >>> areaCirculo 1.0
-- 3.141592653589793
areaCirculo :: Double -> Double
areaCirculo radio
  | radio < 0 = error "El radio no puede ser negativo"
  | otherwise = pi * radio * radio

-- | Calcula el perímetro de un círculo dado su radio.
--
-- >>> perimetroCirculo 5.0
-- 31.41592653589793
perimetroCirculo :: Double -> Double
perimetroCirculo radio
  | radio < 0 = error "El radio no puede ser negativo"
  | otherwise = 2 * pi * radio

-- | Calcula el área de un triángulo usando la fórmula de Herón.
--
-- >>> areaTriangulo 3.0 4.0 5.0
-- 6.0
areaTriangulo :: Double -> Double -> Double -> Double
areaTriangulo a b c
  | a <= 0 || b <= 0 || c <= 0 = error "Los lados deben ser positivos"
  | not (esTrianguloValido a b c) = error "Los lados no forman un triángulo válido"
  | otherwise = sqrt (s * (s - a) * (s - b) * (s - c))
  where
    s = (a + b + c) / 2

-- | Verifica si tres lados pueden formar un triángulo válido.
esTrianguloValido :: Double -> Double -> Double -> Bool
esTrianguloValido a b c = a + b > c && a + c > b && b + c > a

-- | Calcula la hipotenusa de un triángulo rectángulo.
--
-- >>> hipotenusa 3.0 4.0
-- 5.0
hipotenusa :: Double -> Double -> Double
hipotenusa a b
  | a < 0 || b < 0 = error "Los catetos no pueden ser negativos"
  | otherwise = sqrt (a * a + b * b)

-- | Calcula el área de un rectángulo.
--
-- >>> areaRectangulo 5.0 3.0
-- 15.0
areaRectangulo :: Double -> Double -> Double
areaRectangulo largo ancho
  | largo < 0 || ancho < 0 = error "Las dimensiones no pueden ser negativas"
  | otherwise = largo * ancho

-- | Calcula el perímetro de un rectángulo.
--
-- >>> perimetroRectangulo 5.0 3.0
-- 16.0
perimetroRectangulo :: Double -> Double -> Double
perimetroRectangulo largo ancho
  | largo < 0 || ancho < 0 = error "Las dimensiones no pueden ser negativas"
  | otherwise = 2 * (largo + ancho)

-- | Calcula el volumen de una esfera dado su radio.
--
-- >>> volumenEsfera 3.0
-- 113.09733552923255
volumenEsfera :: Double -> Double
volumenEsfera radio
  | radio < 0 = error "El radio no puede ser negativo"
  | otherwise = (4 / 3) * pi * radio * radio * radio

-- | Calcula la superficie de una esfera dado su radio.
--
-- >>> superficieEsfera 3.0
-- 113.09733552923255
superficieEsfera :: Double -> Double
superficieEsfera radio
  | radio < 0 = error "El radio no puede ser negativo"
  | otherwise = 4 * pi * radio * radio

-- | Calcula la distancia euclidiana entre dos puntos en 2D.
--
-- >>> distanciaEuclidiana (0, 0) (3, 4)
-- 5.0
--
-- >>> distanciaEuclidiana (1, 2) (4, 6)
-- 5.0
distanciaEuclidiana :: (Double, Double) -> (Double, Double) -> Double
distanciaEuclidiana (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)