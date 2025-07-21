# Tipos Básicos y Operadores en Haskell

En esta modulo, exploraremos los tipos básicos de datos en Haskell y cómo utilizarlos con operadores. Aprenderemos a trabajar con números, cadenas de texto, booleanos y más, además de cómo realizar operaciones básicas con ellos.

## Números

En Haskell, trabajar con números es fundamental y el sistema de tipos te ayuda a ser muy preciso sobre qué tipo de números estás usando. A diferencia de otros lenguajes que tienen un tipo "número" genérico, Haskell distingue claramente entre diferentes tipos numéricos.

### **🔢 Tipos Numéricos en Haskell**

#### **📊 Números Enteros**

**🔍 `Int` - Enteros de Tamaño Fijo**

```haskell
-- Int tiene un rango limitado (usualmente 32 o 64 bits)
edadPersona :: Int
edadPersona = 25

-- Valores típicos en sistemas de 64 bits:
minimoInt :: Int
minimoInt = -9223372036854775808

maximoInt :: Int
maximoInt = 9223372036854775807

-- Operaciones básicas
suma :: Int
suma = 10 + 5        -- 15

resta :: Int
resta = 20 - 8       -- 12

multiplicacion :: Int
multiplicacion = 6 * 7   -- 42

-- División entera (descarta decimales)
divisionEntera :: Int
divisionEntera = 17 `div` 3   -- 5
```

**🔍 ¿Qué significa "tamaño fijo"?**

- **`Int`** tiene un rango limitado que depende de tu sistema
- En sistemas de 64 bits: aproximadamente -9 × 10¹⁸ a 9 × 10¹⁸
- **¡Puede desbordarse!** Si excedes el rango, el comportamiento no está definido

**🔍 ¿Qué es `div`?**

- **`div`**: División entera que trunca hacia menos infinito
- **`17 `div` 3`**: Notación infija - equivale a `div 17 3`
- Siempre retorna un entero, descartando la parte decimal

**🧪 Probando límites de `Int`:**

```haskell
*Main> import Data.Int
*Main> (maxBound :: Int)
9223372036854775807
*Main> (minBound :: Int)
-9223372036854775808

-- ¡Cuidado con el desbordamiento!
*Main> (maxBound :: Int) + 1
-9223372036854775808  -- ¡Se desbordó!
```

**🔍 `Integer` - Enteros de Precisión Arbitraria**

```haskell
-- Integer puede ser tan grande como la memoria lo permita
numeroGigante :: Integer
numeroGigante = 123456789012345678901234567890

-- Factorial de números grandes (¡sin desbordamiento!)
factorialGrande :: Integer
factorialGrande = factorial 50
-- donde factorial n = product [1..n]

-- Operaciones con Integer
potenciaGrande :: Integer
potenciaGrande = 2^100   -- ¡Funciona perfectamente!

-- Fibonacci de números grandes
fiboGrande :: Integer
fiboGrande = fibonacci 100  -- No hay problema de desbordamiento
```

**🔍 ¿Cuándo usar `Int` vs `Integer`?**

- **`Int`**: Para contadores, índices, tamaños - cuando sabes que el número será pequeño
- **`Integer`**: Para cálculos matemáticos, factoriales, potencias grandes

#### **🧪 Comparando `Int` vs `Integer`**

```haskell
-- Int - rápido pero limitado
contadorPequeno :: Int
contadorPequeno = 1000

longitudLista :: [a] -> Int
longitudLista = length  -- length retorna Int

-- Integer - lento pero ilimitado
calculoMatematico :: Integer
calculoMatematico = 999999999999999999999999999999999

factorialSeguro :: Integer -> Integer
factorialSeguro 0 = 1
factorialSeguro n = n * factorialSeguro (n - 1)

-- Conversiones entre tipos
convertirAInteger :: Int -> Integer
convertirAInteger = fromIntegral

convertirAInt :: Integer -> Int
convertirAInt = fromIntegral  -- ¡Cuidado! Puede perder precisión
```

**🔍 ¿Qué es `fromIntegral`?**

- Función que convierte entre diferentes tipos numéricos
- **`fromIntegral :: (Integral a, Num b) => a -> b`**
- Útil para convertir entre `Int`, `Integer`, `Float`, `Double`

#### **🎯 Números Decimales**

**🔍 `Float` - Punto Flotante de Precisión Simple**

```haskell
-- Float usa 32 bits (menos preciso)
temperatura :: Float
temperatura = 23.5

-- Operaciones básicas
sumaFloat :: Float
sumaFloat = 3.14 + 2.86     -- 6.0

multiplicacionFloat :: Float
multiplicacionFloat = 2.5 * 4.0   -- 10.0

-- División normal (no entera)
divisionFloat :: Float
divisionFloat = 7.0 / 2.0   -- 3.5

-- ¡Cuidado con la precisión!
precision :: Float
precision = 0.1 + 0.2       -- 0.30000001 (¡no exactamente 0.3!)
```

**🔍 `Double` - Punto Flotante de Precisión Doble**

```haskell
-- Double usa 64 bits (más preciso)
pi_preciso :: Double
pi_preciso = 3.141592653589793

-- Cálculos científicos
velocidadLuz :: Double
velocidadLuz = 299792458.0  -- metros por segundo

-- Mejor precisión que Float
precisionMejorada :: Double
precisionMejorada = 0.1 + 0.2   -- 0.30000000000000004 (mejor que Float)

-- Operaciones matemáticas avanzadas
raizCuadrada :: Double
raizCuadrada = sqrt 25.0        -- 5.0

seno :: Double
seno = sin (pi / 2)             -- 1.0

logaritmo :: Double
logaritmo = log 2.718281828     -- ≈ 1.0
```

**🔍 ¿Qué funciones matemáticas están disponibles?**

```haskell
-- Funciones trigonométricas
sin, cos, tan :: Double -> Double

-- Funciones exponenciales y logarítmicas
exp, log, sqrt :: Double -> Double

-- Redondeo
floor, ceiling, round :: Double -> Integer

-- Valor absoluto
abs :: Double -> Double

-- Ejemplo de uso:
angulo :: Double
angulo = pi / 4                    -- 45 grados en radianes

seno45 :: Double
seno45 = sin angulo                -- ≈ 0.7071

redondeado :: Integer
redondeado = round (sqrt 10.0)     -- 3
```

**🆚 `Float` vs `Double` - ¿Cuál elegir?**

```haskell
-- Float - menos memoria, menos precisión
coordenadaFloat :: Float
coordenadaFloat = 1.23456789012345  -- Se trunca a ~7 dígitos

-- Double - más memoria, más precisión
coordenadaDouble :: Double
coordenadaDouble = 1.23456789012345  -- Mantiene ~15 dígitos

-- Comparación práctica
*Main> let f = 1.23456789012345 :: Float
*Main> let d = 1.23456789012345 :: Double
*Main> f
1.2345679
*Main> d
1.23456789012345
```

**💡 Recomendación:**

- **`Float`**: Solo si la memoria es crítica y no necesitas mucha precisión
- **`Double`**: Para la mayoría de cálculos científicos y aplicaciones normales

### **⚙️ Operadores Aritméticos**

#### **🧮 Operadores Básicos**

```haskell
-- Suma (+)
suma1 = 5 + 3           -- 8
suma2 = 1.5 + 2.7       -- 4.2

-- Resta (-)
resta1 = 10 - 4         -- 6
resta2 = 3.14 - 1.14    -- 2.0

-- Multiplicación (*)
mult1 = 6 * 7           -- 42
mult2 = 2.5 * 4.0       -- 10.0

-- Potenciación (^) - exponente entero positivo
pot1 = 2^3              -- 8
pot2 = 3.0^2            -- 9.0

-- Potenciación (**) - exponente decimal
pot3 = 2.0**3.5         -- 11.313708498984761
pot4 = 9.0**(1/2)       -- 3.0 (raíz cuadrada)
```

**🔍 Diferencia entre `^` y `**`:\*\*

- **`^`**: Base cualquier número, exponente `Integer` no negativo
- **`**`**: Ambos argumentos son `Floating` (Float o Double)

#### **➗ Operadores de División**

```haskell
-- División normal (/) - siempre retorna punto flotante
division1 = 10.0 / 3.0  -- 3.3333333333333335
division2 = 7 / 2       -- 3.5 (¡7 se convierte automáticamente!)

-- División entera (div) - trunca hacia menos infinito
divEntera1 = 17 `div` 5    -- 3
divEntera2 = (-17) `div` 5 -- -4

-- Función div en notación prefija
divEntera3 = div 17 5      -- 3

-- Módulo (mod) - resto de la división
modulo1 = 17 `mod` 5       -- 2
modulo2 = (-17) `mod` 5    -- 3

-- Función mod en notación prefija
modulo3 = mod 17 5         -- 2

-- Quotient y Remainder (quot, rem) - trunca hacia cero
quot1 = 17 `quot` 5        -- 3
quot2 = (-17) `quot` 5     -- -3

rem1 = 17 `rem` 5          -- 2
rem2 = (-17) `rem` 5       -- -2
```

**🔍 Diferencia entre `div`/`mod` y `quot`/`rem`:**

```haskell
-- Para números POSITIVOS son iguales:
*Main> 17 `div` 5
3
*Main> 17 `quot` 5
3

-- Para números NEGATIVOS son diferentes:
*Main> (-17) `div` 5
-4                    -- div trunca hacia menos infinito
*Main> (-17) `quot` 5
-3                    -- quot trunca hacia cero

*Main> (-17) `mod` 5
3                     -- mod mantiene el signo del divisor
*Main> (-17) `rem` 5
-2                    -- rem mantiene el signo del dividendo
```

**💡 Regla mnemotécnica:**

- **`div`/`mod`**: "Division/Modulo" - comportamiento matemático estándar
- **`quot`/`rem`**: "Quotient/Remainder" - comportamiento como en C/Java

### **🎯 Conversiones Entre Tipos Numéricos**

#### **🔄 Conversiones Automáticas vs Explícitas**

```haskell
-- ❌ Haskell NO hace conversiones automáticas
-- problema = 5 + 3.14  -- ¡ERROR! No puede sumar Int y Double

-- ✅ Debes convertir explícitamente
solucion1 = 5.0 + 3.14           -- Hacer 5 un Double literalmente
solucion2 = fromIntegral 5 + 3.14 -- Convertir 5 a Double

-- Conversiones comunes
enteroADouble :: Int -> Double
enteroADouble = fromIntegral

doubleAEntero :: Double -> Int
doubleAEntero = round  -- o floor, ceiling, truncate

-- Ejemplos prácticos
edad :: Int
edad = 25

edadPromedio :: Double
edadPromedio = fromIntegral edad + 0.5  -- 25.5

precio :: Double
precio = 19.99

precioRedondeado :: Int
precioRedondeado = round precio  -- 20
```

#### **🧪 Funciones de Conversión**

```haskell
-- fromIntegral - convierte desde cualquier tipo integral
convertir1 = fromIntegral (42 :: Int) :: Double     -- 42.0
convertir2 = fromIntegral (42 :: Integer) :: Float  -- 42.0

-- round - redondea al entero más cercano
redondeo1 = round 3.7    -- 4
redondeo2 = round 3.2    -- 3
redondeo3 = round 3.5    -- 4 (redondea hacia el par más cercano)

-- floor - redondea hacia abajo
piso1 = floor 3.7        -- 3
piso2 = floor (-3.2)     -- -4

-- ceiling - redondea hacia arriba
techo1 = ceiling 3.2     -- 4
techo2 = ceiling (-3.7)  -- -3

-- truncate - elimina la parte decimal
truncar1 = truncate 3.7  -- 3
truncar2 = truncate (-3.7) -- -3
```

### **📊 Literales Numéricos y Notación**

#### **🔢 Diferentes Bases Numéricas**

```haskell
-- Decimal (base 10) - normal
decimal = 42

-- Hexadecimal (base 16) - prefijo 0x
hexadecimal = 0xFF      -- 255 en decimal
otroHex = 0xABC         -- 2748 en decimal

-- Octal (base 8) - prefijo 0o
octal = 0o777           -- 511 en decimal
otroOctal = 0o123       -- 83 en decimal

-- Binario (base 2) - prefijo 0b
binario = 0b1010        -- 10 en decimal
otroBinario = 0b11111111 -- 255 en decimal

-- Verificando en GHCi:
*Main> 0xFF
255
*Main> 0o777
511
*Main> 0b1010
10
```

#### **🎯 Notación Científica**

```haskell
-- Notación científica para números muy grandes o pequeños
numeroGrande = 1.23e6       -- 1230000.0 (1.23 × 10⁶)
numeroPequeno = 4.56e-3     -- 0.00456 (4.56 × 10⁻³)

-- Ejemplos científicos
velocidadLuz = 2.998e8      -- metros por segundo
masaElectron = 9.109e-31    -- kilogramos
constanteAvogadro = 6.022e23

-- También puedes usar E mayúscula
otroGrande = 1.5E10
otroPequeno = 2.3E-5
```

#### **📝 Separadores en Números**

```haskell
-- Puedes usar guiones bajos para claridad (desde GHC 8.6)
millonClaro = 1_000_000
piPreciso = 3.141_592_653_589_793
hexClaro = 0xFF_FF_FF
binarioClaro = 0b1010_0101_1111_0000

-- Los guiones bajos son ignorados por el compilador
*Main> 1_000_000 == 1000000
True
```

### **🧮 Ejemplos Prácticos**

#### **💰 Calculadora de Propinas**

```haskell
-- Calcular propina de un restaurante
calcularPropina :: Double -> Double -> Double
calcularPropina cuenta porcentaje = cuenta * (porcentaje / 100)

-- Calcular total con propina
totalConPropina :: Double -> Double -> Double
totalConPropina cuenta porcentaje =
    cuenta + calcularPropina cuenta porcentaje

-- Ejemplo de uso:
-- totalConPropina 50.0 15.0  -- 57.5 (cuenta + 15% propina)

-- Versión más elegante usando where
calcularTotal :: Double -> Double -> Double
calcularTotal cuenta porcentaje = cuenta + propina
  where
    propina = cuenta * (porcentaje / 100)
```

#### **📊 Estadísticas Básicas**

```haskell
-- Promedio de una lista de números
promedio :: [Double] -> Double
promedio numeros = sum numeros / fromIntegral (length numeros)

-- Encontrar máximo y mínimo
rangoLista :: [Double] -> (Double, Double)
rangoLista numeros = (minimum numeros, maximum numeros)

-- Sumar cuadrados (útil para varianza)
sumaCuadrados :: [Double] -> Double
sumaCuadrados numeros = sum (map (^2) numeros)

-- Ejemplo de uso:
datos = [1.5, 2.3, 3.7, 2.1, 4.2]
-- promedio datos        -- 2.76
-- rangoLista datos      -- (1.5, 4.2)
-- sumaCuadrados datos   -- 39.78
```

#### **🎯 Conversión de Temperaturas**

```haskell
-- Celsius a Fahrenheit
celsiusAFahrenheit :: Double -> Double
celsiusAFahrenheit celsius = celsius * 9/5 + 32

-- Fahrenheit a Celsius
fahrenheitACelsius :: Double -> Double
fahrenheitACelsius fahrenheit = (fahrenheit - 32) * 5/9

-- Celsius a Kelvin
celsiusAKelvin :: Double -> Double
celsiusAKelvin celsius = celsius + 273.15

-- Ejemplos:
-- celsiusAFahrenheit 0      -- 32.0 (punto de congelación)
-- celsiusAFahrenheit 100    -- 212.0 (punto de ebullición)
-- fahrenheitACelsius 98.6   -- 37.0 (temperatura corporal normal)
```

### **⚠️ Errores Comunes y Cómo Evitarlos**

#### **🚫 Error de Tipos Incompatibles**

```haskell
-- ❌ Error común:
-- problema = 5 + 3.14  -- No compila!

-- ✅ Soluciones:
solucion1 = 5.0 + 3.14              -- Hacer ambos Double
solucion2 = fromIntegral 5 + 3.14   -- Convertir explícitamente
solucion3 = 5 + round 3.14          -- Convertir a Int
```

#### **⚠️ División Entera Accidental**

```haskell
-- ❌ Resultado inesperado:
problemaDiv = 5 `div` 2  -- 2 (no 2.5!)

-- ✅ Para división decimal:
divisionCorrecta = 5.0 / 2.0  -- 2.5
-- o
divisionCorrecta2 = fromIntegral 5 / fromIntegral 2  -- 2.5
```

#### **🔄 Desbordamiento de `Int`**

```haskell
-- ❌ Peligroso con números grandes:
-- factorial :: Int -> Int
-- factorial n = product [1..n]  -- ¡Se desborda rápidamente!

-- ✅ Usar Integer para cálculos grandes:
factorialSeguro :: Integer -> Integer
factorialSeguro n = product [1..n]  -- Sin problemas de desbordamiento
```

### **🎯 Conceptos Clave**

#### **🏗️ Sistema de Tipos Estricto**

- Haskell **NO hace conversiones automáticas** entre tipos numéricos
- Debes ser **explícito** sobre las conversiones
- Esto previene errores sutiles comunes en otros lenguajes

#### **⚖️ Cuándo Usar Cada Tipo**

- **`Int`**: Contadores, índices, tamaños (números que sabes que serán pequeños)
- **`Integer`**: Matemáticas, factoriales, números potencialmente muy grandes
- **`Float`**: Cuando la memoria es crítica y la precisión no es importante
- **`Double`**: La mayoría de cálculos con decimales

#### **🎯 Precisión vs Rendimiento**

- **Más precisión** → Mayor uso de memoria y tiempo
- **Menos precisión** → Más rápido pero posibles errores de redondeo
- **Elige según las necesidades** de tu aplicación

### **🧪 Ejercicios para Practicar**

```haskell
-- 1. Escribe una función que calcule el área de un círculo
areaCirculo :: Double -> Double
areaCirculo radio = ?

-- 2. Función que determine si un año es bisiesto
esBisiesto :: Int -> Bool
esBisiesto ano = ?

-- 3. Convertir segundos a horas, minutos y segundos
convertirTiempo :: Int -> (Int, Int, Int)
convertirTiempo segundosTotales = ?

-- 4. Calcular el n-ésimo término de la secuencia de Fibonacci
fibonacci :: Integer -> Integer
fibonacci n = ?

-- 5. Función que calcule el interés compuesto
interesCompuesto :: Double -> Double -> Int -> Double
interesCompuesto principal tasa anos = ?
```

### **🚀 Siguiente Paso**

¡Excelente! Ya dominas los números en Haskell. El siguiente tema es **Caracteres y Strings**, donde aprenderás a trabajar con texto y caracteres individuales.

**¿Listo para continuar con caracteres y strings?** 📝

## Caracteres y Strings

En Haskell, el manejo de texto es elegante y poderoso. A diferencia de muchos lenguajes donde los strings son primitivos, en Haskell un **String es simplemente una lista de caracteres**. Esta representación simple pero poderosa te permite usar todas las funciones de listas para manipular texto.

### **📝 El Tipo `Char` (Carácter)**

#### **🔤 ¿Qué es un `Char`?**

Un `Char` representa **un solo carácter Unicode**. Se escribe entre comillas simples (`'`).

```haskell
-- Caracteres básicos
letra :: Char
letra = 'a'

mayuscula :: Char
mayuscula = 'A'

digito :: Char
digito = '5'

espacio :: Char
espacio = ' '

-- Caracteres especiales
nuevaLinea :: Char
nuevaLinea = '\n'

tabulacion :: Char
tabulacion = '\t'

comillaSimple :: Char
comillaSimple = '\''

barraInvertida :: Char
barraInvertida = '\\'
```

**🔍 ¿Qué son los caracteres de escape?**

- **`\n`**: Nueva línea (como Enter)
- **`\t`**: Tabulación (como Tab)
- **`\'`**: Comilla simple literal
- **`\"`**: Comilla doble literal
- **`\\`**: Barra invertida literal
- **`\r`**: Retorno de carro

#### **🌍 Soporte Unicode**

```haskell
-- Caracteres en español
enie :: Char
enie = 'ñ'

acentuada :: Char
acentuada = 'á'

-- Caracteres de otros idiomas
chino :: Char
chino = '中'

emoji :: Char
emoji = '😀'

-- Caracteres griegos (útiles en matemáticas)
pi :: Char
pi = 'π'

lambda :: Char
lambda = 'λ'

-- Verificando en GHCi:
-- *Main> 'ñ'
-- 'ñ'
-- *Main> '😀'
-- '\128512'  -- Código Unicode
```

#### **🧮 Funciones para Caracteres**

```haskell
-- Verificar tipos de caracteres
esLetra :: Char -> Bool
esLetra = isLetter
-- isLetter 'a' = True, isLetter '5' = False

esDigito :: Char -> Bool
esDigito = isDigit
-- isDigit '7' = True, isDigit 'x' = False

esEspacio :: Char -> Bool
esEspacio = isSpace
-- isSpace ' ' = True, isSpace '\t' = True

esMayuscula :: Char -> Bool
esMayuscula = isUpper
-- isUpper 'A' = True, isUpper 'a' = False

esMinuscula :: Char -> Bool
esMinuscula = isLower
-- isLower 'a' = True, isLower 'A' = False

-- Conversiones de caracteres
aMayuscula :: Char -> Char
aMayuscula = toUpper
-- toUpper 'a' = 'A'

aMinuscula :: Char -> Char
aMinuscula = toLower
-- toLower 'A' = 'a'

-- Obtener código ASCII/Unicode
codigoChar :: Char -> Int
codigoChar = ord
-- ord 'A' = 65, ord 'a' = 97

-- Convertir código a carácter
charDeCodigo :: Int -> Char
charDeCodigo = chr
-- chr 65 = 'A', chr 97 = 'a'
```

**🔍 ¿De dónde vienen estas funciones?**

```haskell
import Data.Char  -- Necesitas importar este módulo

-- Ahora puedes usar:
-- isLetter, isDigit, isSpace, isUpper, isLower
-- toUpper, toLower, ord, chr
```

### **📚 El Tipo `String`**

#### **🔍 ¿Qué es un `String` en Haskell?**

```haskell
-- String es simplemente un alias para [Char]
type String = [Char]

-- Estas declaraciones son equivalentes:
mensaje1 :: String
mensaje1 = "Hola mundo"

mensaje2 :: [Char]
mensaje2 = ['H','o','l','a',' ','m','u','n','d','o']

-- ¡Son exactamente lo mismo!
sonIguales :: Bool
sonIguales = mensaje1 == mensaje2  -- True
```

**🔍 ¿Qué significa `type String = [Char]`?**

- **`type`**: Crea un **alias** de tipo (no un tipo nuevo)
- **`String`** es solo otro nombre para **`[Char]`**
- Puedes usar todas las funciones de listas con strings

#### **📝 Literales de String**

```haskell
-- String vacío
vacio :: String
vacio = ""

-- String simple
saludo :: String
saludo = "¡Hola!"

-- String con caracteres especiales
multilinea :: String
multilinea = "Primera línea\nSegunda línea\tcon tabulación"

-- String con comillas
conComillas :: String
conComillas = "Dijo: \"¡Hola mundo!\""

-- String largo (se puede dividir en líneas)
textoLargo :: String
textoLargo = "Este es un texto muy largo " ++
             "que se extiende por varias líneas " ++
             "para mayor legibilidad"
```

### **🔧 Operaciones Básicas con Strings**

#### **🔗 Concatenación**

```haskell
-- Operador (++) para concatenar
concatenar :: String
concatenar = "Hola" ++ " " ++ "mundo"  -- "Hola mundo"

-- Agregar un carácter al inicio (:)
agregarCaracter :: String
agregarCaracter = 'H' : "ola"  -- "Hola"

-- Concatenar múltiples strings
unirVarios :: String
unirVarios = concat ["Buenos", " ", "días", "!"]  -- "Buenos días!"

-- Unir con separador
unirConSeparador :: String
unirConSeparador = intercalate ", " ["manzana", "pera", "banana"]
-- "manzana, pera, banana"
```

**🔍 ¿Qué es `intercalate`?**

```haskell
import Data.List  -- intercalate está aquí

-- intercalate separador lista
-- Inserta el separador entre cada elemento de la lista
```

#### **📏 Longitud y Acceso**

```haskell
-- Longitud de un string
longitudTexto :: String -> Int
longitudTexto = length
-- length "Hola" = 4

-- Verificar si está vacío
estaVacio :: String -> Bool
estaVacio = null
-- null "" = True, null "algo" = False

-- Acceder a caracteres por índice
primerCaracter :: String -> Char
primerCaracter texto = texto !! 0
-- "Hola" !! 0 = 'H'

-- Tomar los primeros n caracteres
primeros :: Int -> String -> String
primeros = take
-- take 3 "Hola mundo" = "Hol"

-- Saltar los primeros n caracteres
resto :: Int -> String -> String
resto = drop
-- drop 5 "Hola mundo" = "mundo"

-- Dividir en una posición
dividir :: Int -> String -> (String, String)
dividir n texto = (take n texto, drop n texto)
-- dividir 4 "Hola mundo" = ("Hola", " mundo")
```

#### **🔍 Búsqueda y Verificación**

```haskell
-- Verificar si un carácter está en el string
contiene :: Char -> String -> Bool
contiene caracter texto = caracter `elem` texto
-- 'a' `elem` "Hola" = True

-- Verificar si un substring está presente
contieneSubstring :: String -> String -> Bool
contieneSubstring substring texto = substring `isInfixOf` texto
-- "ola" `isInfixOf` "Hola mundo" = True

-- Verificar prefijos y sufijos
empiezaCon :: String -> String -> Bool
empiezaCon prefijo texto = prefijo `isPrefixOf` texto
-- "Hol" `isPrefixOf` "Hola" = True

terminaCon :: String -> String -> Bool
terminaCon sufijo texto = sufijo `isSuffixOf` texto
-- "ndo" `isSuffixOf` "mundo" = True

-- Encontrar la posición de un substring
encontrarPosicion :: String -> String -> Maybe Int
encontrarPosicion substring texto = -- implementación compleja
-- findIndex (\i -> substring `isPrefixOf` drop i texto) [0..length texto]
```

**🔍 ¿Qué es `Maybe Int`?**

- **`Maybe`** es un tipo que puede ser **`Nothing`** (nada encontrado) o **`Just valor`** (encontrado en posición valor)
- Es la forma segura de Haskell de manejar valores que pueden no existir

### **🛠️ Transformaciones de Strings**

#### **🔄 Cambio de Caso**

```haskell
-- Convertir todo a mayúsculas
aMayusculas :: String -> String
aMayusculas = map toUpper
-- aMayusculas "Hola Mundo" = "HOLA MUNDO"

-- Convertir todo a minúsculas
aMinusculas :: String -> String
aMinusculas = map toLower
-- aMinusculas "Hola Mundo" = "hola mundo"

-- Capitalizar (primera letra mayúscula)
capitalizar :: String -> String
capitalizar "" = ""
capitalizar (x:xs) = toUpper x : map toLower xs
-- capitalizar "hOLA mUNDO" = "Hola mundo"

-- Capitalizar cada palabra
capitalizarPalabras :: String -> String
capitalizarPalabras texto = unwords (map capitalizar (words texto))
-- capitalizarPalabras "hola mundo" = "Hola Mundo"
```

**🔍 ¿Qué son `words` y `unwords`?**

- **`words`**: Divide un string en una lista de palabras
- **`unwords`**: Une una lista de palabras en un string

```haskell
*Main> words "Hola mundo cruel"
["Hola","mundo","cruel"]
*Main> unwords ["Hola","mundo","cruel"]
"Hola mundo cruel"
```

#### **✂️ Filtrado y Limpieza**

```haskell
-- Eliminar espacios al inicio y final
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- Eliminar solo espacios del inicio
trimIzquierda :: String -> String
trimIzquierda = dropWhile isSpace

-- Eliminar solo espacios del final
trimDerecha :: String -> String
trimDerecha = dropWhileEnd isSpace

-- Filtrar solo letras
soloLetras :: String -> String
soloLetras = filter isLetter
-- soloLetras "Hola123Mundo!" = "HolaMundo"

-- Filtrar solo dígitos
soloDigitos :: String -> String
soloDigitos = filter isDigit
-- soloDigitos "abc123def456" = "123456"

-- Eliminar caracteres específicos
eliminarCaracter :: Char -> String -> String
eliminarCaracter c = filter (/= c)
-- eliminarCaracter 'a' "banana" = "bnn"
```

**🔍 ¿Qué es `dropWhileEnd`?**

```haskell
import Data.List  -- dropWhileEnd está aquí

-- dropWhile: elimina elementos del inicio mientras se cumpla la condición
-- dropWhileEnd: elimina elementos del final mientras se cumpla la condición
```

#### **🔀 Reversión y Ordenamiento**

```haskell
-- Revertir un string
revertir :: String -> String
revertir = reverse
-- revertir "Hola" = "aloH"

-- Ordenar caracteres alfabéticamente
ordenar :: String -> String
ordenar = sort
-- ordenar "dcba" = "abcd"

-- Verificar si es palíndromo
esPalindromo :: String -> Bool
esPalindromo texto = textoLimpio == reverse textoLimpio
  where
    textoLimpio = map toLower (filter isLetter texto)
-- esPalindromo "A man a plan a canal Panama" = True
```

### **📊 Análisis de Strings**

#### **🔢 Contar Elementos**

```haskell
-- Contar caracteres específicos
contarCaracter :: Char -> String -> Int
contarCaracter c texto = length (filter (== c) texto)
-- contarCaracter 'a' "banana" = 3

-- Contar palabras
contarPalabras :: String -> Int
contarPalabras = length . words
-- contarPalabras "Hola mundo cruel" = 3

-- Contar líneas
contarLineas :: String -> Int
contarLineas = length . lines
-- contarLineas "línea1\nlínea2\nlínea3" = 3

-- Contar vocales
contarVocales :: String -> Int
contarVocales = length . filter esVocal . map toLower
  where
    esVocal c = c `elem` "aeiou"
-- contarVocales "Hola Mundo" = 4
```

#### **📈 Estadísticas de Texto**

```haskell
-- Frecuencia de caracteres
frecuenciaCaracteres :: String -> [(Char, Int)]
frecuenciaCaracteres texto =
  map (\grupo -> (head grupo, length grupo))
  (group (sort texto))
-- frecuenciaCaracteres "hello" = [('e',1),('h',1),('l',2),('o',1)]

-- Carácter más frecuente
caracterMasFrecuente :: String -> Char
caracterMasFrecuente texto =
  fst (maximumBy (comparing snd) (frecuenciaCaracteres texto))

-- Longitud promedio de palabras
longitudPromedioPalabras :: String -> Double
longitudPromedioPalabras texto =
  let palabras = words texto
      totalCaracteres = sum (map length palabras)
      numeroPalabras = length palabras
  in fromIntegral totalCaracteres / fromIntegral numeroPalabras
```

**🔍 ¿Qué hace `group`?**

```haskell
import Data.List  -- group está aquí

-- group agrupa elementos consecutivos iguales
*Main> group "aabbccaaa"
["aa","bb","cc","aaa"]

-- Por eso primero ordenamos con sort, luego agrupamos
*Main> group (sort "banana")
["aaa","bnn"]
```

### **🎯 Validación de Strings**

#### **✅ Funciones de Validación**

```haskell
-- Verificar si es un número
esNumero :: String -> Bool
esNumero = all isDigit
-- esNumero "123" = True, esNumero "12a" = False

-- Verificar si es un número decimal
esDecimal :: String -> Bool
esDecimal texto =
  case break (== '.') texto of
    (antes, "") -> esNumero antes && not (null antes)
    (antes, _:despues) -> esNumero antes && esNumero despues &&
                          not (null antes) && not (null despues)
-- esDecimal "123.45" = True, esDecimal "12.3.4" = False

-- Verificar si es email básico
esEmailBasico :: String -> Bool
esEmailBasico email =
  '@' `elem` email &&
  '.' `elem` email &&
  length email > 5 &&
  not (null (takeWhile (/= '@') email)) &&
  not (null (drop 1 (dropWhile (/= '@') email)))

-- Verificar si es password seguro
esPasswordSeguro :: String -> Bool
esPasswordSeguro password =
  length password >= 8 &&
  any isUpper password &&
  any isLower password &&
  any isDigit password
```

#### **🧹 Sanitización**

```haskell
-- Limpiar string para URL (slug)
aSlug :: String -> String
aSlug =
  intercalate "-" .
  words .
  map (\c -> if isAlphaNum c || isSpace c then toLower c else ' ')
-- aSlug "¡Hola Mundo!" = "hola-mundo"

-- Escapar caracteres HTML
escaparHTML :: String -> String
escaparHTML = concatMap escaparCaracter
  where
    escaparCaracter '<' = "&lt;"
    escaparCaracter '>' = "&gt;"
    escaparCaracter '&' = "&amp;"
    escaparCaracter '"' = "&quot;"
    escaparCaracter c = [c]

-- Generar iniciales
generarIniciales :: String -> String
generarIniciales = map toUpper . map head . words
-- generarIniciales "Juan Carlos Pérez" = "JCP"
```

### **🔀 Conversiones y Parsing**

#### **🔄 String a Otros Tipos**

```haskell
-- String a Int (usando read - puede fallar)
stringAInt :: String -> Int
stringAInt = read
-- read "123" :: Int = 123
-- read "abc" :: Int = *** Exception: no parse

-- Conversión segura con Maybe
stringAIntSeguro :: String -> Maybe Int
stringAIntSeguro s =
  case reads s of
    [(n, "")] -> Just n
    _ -> Nothing
-- stringAIntSeguro "123" = Just 123
-- stringAIntSeguro "abc" = Nothing

-- String a Double
stringADouble :: String -> Maybe Double
stringADouble s =
  case reads s of
    [(n, "")] -> Just n
    _ -> Nothing

-- Bool a String
boolAString :: Bool -> String
boolAString = show
-- show True = "True"

-- Lista a String
listaAString :: Show a => [a] -> String
listaAString = show
-- show [1,2,3] = "[1,2,3]"
```

#### **📋 Parsing Simple**

```haskell
-- Dividir por delimitador
dividirPor :: Char -> String -> [String]
dividirPor delimitador =
  map (dropWhile (== delimitador)) .
  groupBy (\a b -> a /= delimitador && b /= delimitador)
-- dividirPor ',' "a,b,c" = ["a","b","c"]

-- Parsear CSV simple
parsearCSV :: String -> [[String]]
parsearCSV = map (dividirPor ',') . lines

-- Parsear pares clave-valor
parsearConfig :: String -> [(String, String)]
parsearConfig =
  mapMaybe parsearLinea .
  filter (not . null) .
  map (dropWhile isSpace) .
  lines
  where
    parsearLinea linea =
      case break (== '=') linea of
        (clave, '=':valor) -> Just (trim clave, trim valor)
        _ -> Nothing
```

### **🎨 Ejemplos Prácticos**

#### **📧 Generador de Username**

```haskell
-- Generar username desde nombre completo
generarUsername :: String -> String
generarUsername nombreCompleto =
  let palabras = words (map toLower nombreCompleto)
      sinEspacios = concat palabras
      soloAlfanum = filter isAlphaNum sinEspacios
  in take 12 soloAlfanum
-- generarUsername "Juan Carlos Pérez García" = "juancarlospe"

-- Versión con iniciales + apellido
generarUsernameCorto :: String -> String
generarUsernameCorto nombreCompleto =
  let palabras = words (map toLower nombreCompleto)
  in case palabras of
       [] -> ""
       [solo] -> take 8 solo
       (primero:resto) ->
         [head primero] ++ concatMap (take 1) (init resto) ++ last resto
-- generarUsernameCorto "Juan Carlos Pérez García" = "jcgarcia"
```

#### **🔐 Generador de Password**

```haskell
-- Evaluar fortaleza de password
fortalezaPassword :: String -> String
fortalezaPassword password
  | length password < 6 = "Muy débil"
  | length password < 8 = "Débil"
  | not (tieneVariedad password) = "Regular"
  | length password < 12 = "Fuerte"
  | otherwise = "Muy fuerte"
  where
    tieneVariedad p = any isUpper p && any isLower p && any isDigit p

-- Sugerir mejoras para password
sugerirMejoras :: String -> [String]
sugerirMejoras password =
  let sugerencias = []
      conLongitud = if length password < 8
                   then "Usar al menos 8 caracteres" : sugerencias
                   else sugerencias
      conMayuscula = if not (any isUpper password)
                    then "Incluir letras mayúsculas" : conLongitud
                    else conLongitud
      conMinuscula = if not (any isLower password)
                    then "Incluir letras minúsculas" : conMayuscula
                    else conMayuscula
      conNumero = if not (any isDigit password)
                 then "Incluir números" : conMinuscula
                 else conMinuscula
  in reverse conNumero
```

#### **📝 Formateador de Texto**

```haskell
-- Justificar texto a un ancho específico
justificarTexto :: Int -> String -> String
justificarTexto ancho texto =
  let lineas = dividirEnLineas ancho (words texto)
  in unlines (map (justificarLinea ancho) lineas)

dividirEnLineas :: Int -> [String] -> [[String]]
dividirEnLineas _ [] = []
dividirEnLineas ancho palabras =
  let (linea, resto) = tomarPalabrasQueQuepan ancho palabras
  in linea : dividirEnLineas ancho resto

tomarPalabrasQueQuepan :: Int -> [String] -> ([String], [String])
tomarPalabrasQueQuepan ancho =
  go 0 []
  where
    go longitud acum [] = (reverse acum, [])
    go longitud acum (p:ps)
      | longitud + length p + length acum > ancho = (reverse acum, p:ps)
      | otherwise = go (longitud + length p) (p:acum) ps

justificarLinea :: Int -> [String] -> String
justificarLinea ancho [palabra] = palabra
justificarLinea ancho palabras =
  let totalLetras = sum (map length palabras)
      espaciosNecesarios = ancho - totalLetras
      huecos = length palabras - 1
      espaciosPorHueco = espaciosNecesarios `div` huecos
      espaciosExtra = espaciosNecesarios `mod` huecos
  in intercalate (replicate espaciosPorHueco ' ') palabras
```

### **🎯 Conceptos Clave**

#### **📚 String = [Char]**

- Los strings son **listas de caracteres**
- Puedes usar **todas las funciones de listas** con strings
- `"hola"` es exactamente igual a `['h','o','l','a']`

#### **🔧 Immutabilidad**

- Los strings son **inmutables** - no se pueden modificar
- Las "modificaciones" crean **nuevos strings**
- Esto es seguro pero puede ser menos eficiente para strings muy grandes

#### **🎨 Composición de Funciones**

```haskell
-- En lugar de múltiples pasos:
-- paso1 = map toLower texto
-- paso2 = filter isLetter paso1
-- resultado = reverse paso2

-- Compón las operaciones:
procesar = reverse . filter isLetter . map toLower
resultado = procesar texto
```

### **⚠️ Errores Comunes**

#### **🚫 Índices Fuera de Rango**

```haskell
-- ❌ Peligroso:
-- texto !! 10  -- ¡Error si texto tiene menos de 11 caracteres!

-- ✅ Seguro:
obtenerCaracterSeguro :: Int -> String -> Maybe Char
obtenerCaracterSeguro i texto
  | i >= 0 && i < length texto = Just (texto !! i)
  | otherwise = Nothing
```

#### **🔄 Conversiones Inseguras**

```haskell
-- ❌ Puede fallar:
-- read "abc" :: Int  -- ¡Exception!

-- ✅ Manejo seguro:
leerEntero :: String -> Maybe Int
leerEntero s = case reads s of
  [(n, "")] -> Just n
  _ -> Nothing
```

### **🧪 Ejercicios para Practicar**

```haskell
-- 1. Función que cuente palabras únicas en un texto
palabrasUnicas :: String -> Int
palabrasUnicas = ?

-- 2. Verificar si un string es un palíndromo (ignorando espacios y mayúsculas)
esPalindromoMejorado :: String -> Bool
esPalindromoMejorado = ?

-- 3. Codificar/decodificar ROT13
rot13 :: String -> String
rot13 = ?

-- 4. Encontrar la palabra más larga en un texto
palabraMasLarga :: String -> String
palabraMasLarga = ?

-- 5. Convertir texto a "lenguaje de programador" (camelCase)
aCamelCase :: String -> String
aCamelCase = ?
```

### **🚀 Siguiente Paso**

¡Genial! Ya dominas caracteres y strings en Haskell. El siguiente tema es **Booleanos y Operadores Lógicos**, donde aprenderás sobre lógica y toma de decisiones.

**¿Listo para continuar con booleanos?** ✅

## Booleanos y Operadores Lógicos

Los valores booleanos representan **verdad o falsedad** y son fundamentales para tomar decisiones en tu código. En Haskell, el tipo `Bool` tiene solo dos valores posibles: `True` y `False`. Los operadores lógicos te permiten combinar y manipular estos valores para crear lógica compleja.

### **✅ El Tipo `Bool`**

#### **🔍 ¿Qué es un Booleano?**

Un `Bool` representa un **valor de verdad**. Solo puede ser una de dos cosas:

```haskell
-- Los únicos dos valores booleanos
verdadero :: Bool
verdadero = True

falso :: Bool
falso = False

-- Ejemplos de expresiones que retornan Bool
esMayor :: Bool
esMayor = 5 > 3          -- True

esIgual :: Bool
esIgual = 2 + 2 == 5     -- False

esPositivo :: Bool
esPositivo = 10 > 0      -- True
```

**🔍 ¿Por qué se llama "Bool"?**

- Nombrado en honor a **George Boole**, matemático inglés (1815-1864)
- Inventó el **álgebra booleana**, la base de la lógica computacional
- Sus ideas son la base de todos los circuitos digitales modernos

#### **📊 Funciones que Retornan Bool**

```haskell
-- Funciones de comparación (veremos más adelante)
esIgualA :: Int -> Int -> Bool
esIgualA x y = x == y

esMenorQue :: Int -> Int -> Bool
esMenorQue x y = x < y

-- Funciones de verificación
esPar :: Int -> Bool
esPar n = n `mod` 2 == 0

esVacio :: [a] -> Bool
esVacio lista = null lista

-- Funciones de caracteres (requiere import Data.Char)
esLetra :: Char -> Bool
esLetra = isLetter

esDigito :: Char -> Bool
esDigito = isDigit
```

### **🔧 Operadores Lógicos Básicos**

#### **🚫 Negación (`not`)**

El operador `not` **invierte** un valor booleano:

```haskell
-- not :: Bool -> Bool
negacion1 = not True     -- False
negacion2 = not False    -- True

-- Ejemplos prácticos
noEsPar :: Int -> Bool
noEsPar n = not (esPar n)

noEstaVacio :: [a] -> Bool
noEstaVacio lista = not (null lista)

-- Uso con comparaciones
noEsIgual :: Int -> Int -> Bool
noEsIgual x y = not (x == y)
-- Equivale a: x /= y
```

**🔍 Precedencia de `not`:**

```haskell
-- not tiene alta precedencia
resultado1 = not True && False   -- False (equivale a (not True) && False)
resultado2 = not (True && False) -- True (paréntesis cambian el orden)
```

#### **🤝 Conjunción (`&&`) - "Y Lógico"**

El operador `&&` retorna `True` **solo si ambos operandos son `True`**:

```haskell
-- (&&) :: Bool -> Bool -> Bool

-- Tabla de verdad para &&:
-- True  && True  = True
-- True  && False = False
-- False && True  = False
-- False && False = False

conjuncion1 = True && True    -- True
conjuncion2 = True && False   -- False
conjuncion3 = False && True   -- False
conjuncion4 = False && False  -- False

-- Ejemplos prácticos
esAdultoJoven :: Int -> Bool
esAdultoJoven edad = edad >= 18 && edad <= 30

esPasswordValida :: String -> Bool
esPasswordValida password =
    length password >= 8 &&
    any isUpper password &&
    any isDigit password

estaEnRango :: Int -> Int -> Int -> Bool
estaEnRango minimo maximo valor = valor >= minimo && valor <= maximo
```

**⚡ Evaluación Perezosa (Short-Circuit):**

```haskell
-- Si el primer operando es False, el segundo NO se evalúa
resultado = False && error "¡Esto no se ejecuta!"  -- False (no hay error)

-- Esto es útil para evitar errores:
esSafeDiv :: Int -> Int -> Bool
esSafeDiv x y = y /= 0 && x `mod` y == 0
-- Si y == 0, no se evalúa x `mod` y (que causaría error)
```

#### **🔀 Disyunción (`||`) - "O Lógico"**

El operador `||` retorna `True` **si al menos uno de los operandos es `True`**:

```haskell
-- (||) :: Bool -> Bool -> Bool

-- Tabla de verdad para ||:
-- True  || True  = True
-- True  || False = True
-- False || True  = True
-- False || False = False

disyuncion1 = True || True    -- True
disyuncion2 = True || False   -- True
disyuncion3 = False || True   -- True
disyuncion4 = False || False  -- False

-- Ejemplos prácticos
esFinDeSemana :: String -> Bool
esFinDeSemana dia = dia == "sabado" || dia == "domingo"

tieneDescuento :: Int -> Bool -> Bool
tieneDescuento edad esEstudiante = edad >= 65 || esEstudiante

puedeVotar :: Int -> String -> Bool
puedeVotar edad nacionalidad =
    edad >= 18 || nacionalidad == "ciudadano"
```

**⚡ Evaluación Perezosa:**

```haskell
-- Si el primer operando es True, el segundo NO se evalúa
resultado = True || error "¡Esto no se ejecuta!"  -- True (no hay error)

-- Ejemplo útil:
esSafeAccess :: [a] -> Int -> Bool
esSafeAccess lista indice = null lista || indice < length lista
-- Si la lista está vacía, no verifica el índice
```

### **📊 Operadores de Comparación**

#### **⚖️ Igualdad y Desigualdad**

```haskell
-- Igualdad (==)
igualdad1 = 5 == 5        -- True
igualdad2 = 3 == 7        -- False
igualdad3 = "hola" == "hola"  -- True

-- Desigualdad (/=)
desigualdad1 = 5 /= 3     -- True
desigualdad2 = 4 /= 4     -- False
desigualdad3 = "a" /= "b" -- True

-- Con diferentes tipos (requieren conversión)
comparacion = fromIntegral 5 == 5.0  -- True
-- comparacion2 = 5 == 5.0  -- ¡ERROR! Tipos incompatibles
```

**🔍 ¿Qué significa `/=`?**

- Es el operador de **"no igual"**
- En matemáticas se escribe ≠
- Equivale a `not (x == y)`

#### **📈 Comparaciones Ordinales**

```haskell
-- Menor que (<)
menor1 = 3 < 5         -- True
menor2 = 8 < 2         -- False

-- Menor o igual (<=)
menorIgual1 = 4 <= 4   -- True
menorIgual2 = 6 <= 3   -- False

-- Mayor que (>)
mayor1 = 10 > 5        -- True
mayor2 = 2 > 8         -- False

-- Mayor o igual (>=)
mayorIgual1 = 7 >= 7   -- True
mayorIgual2 = 3 >= 9   -- False

-- Con strings (orden lexicográfico/alfabético)
stringComparacion1 = "abc" < "def"    -- True
stringComparacion2 = "casa" > "auto"  -- True (c > a)
stringComparacion3 = "Hola" < "hola"  -- True (H < h en ASCII)
```

**🔍 ¿Qué es "orden lexicográfico"?**

- Es el **orden de diccionario**
- Se compara carácter por carácter de izquierda a derecha
- Usa los códigos ASCII/Unicode para comparar caracteres

```haskell
-- Ejemplos paso a paso:
-- "abc" < "def": 'a' < 'd', por tanto True
-- "casa" < "cosa": 'a' < 'o', por tanto True
-- "casa" < "casb": iguales hasta 'a' < 'b', por tanto True
```

### **🎯 Funciones Útiles con Booleanos**

#### **🔢 Funciones de Agregación**

```haskell
-- and :: [Bool] -> Bool
-- Retorna True si TODOS los elementos son True
todosVerdaderos = and [True, True, True]    -- True
algunosFalsos = and [True, False, True]     -- False
listaVacia = and []                         -- True (convenio matemático)

-- or :: [Bool] -> Bool
-- Retorna True si AL MENOS UNO es True
algunosVerdaderos = or [False, True, False] -- True
todosFalsos = or [False, False, False]      -- False
listaVaciaOr = or []                        -- False (convenio matemático)

-- Ejemplos prácticos
todosPositivos :: [Int] -> Bool
todosPositivos numeros = and (map (> 0) numeros)

algunEsPar :: [Int] -> Bool
algunEsPar numeros = or (map even numeros)

-- Versión más elegante con all y any (ver abajo)
todosPositivos' :: [Int] -> Bool
todosPositivos' = all (> 0)

algunEsPar' :: [Int] -> Bool
algunEsPar' = any even
```

#### **🎪 Cuantificadores: `all` y `any`**

```haskell
-- all :: (a -> Bool) -> [a] -> Bool
-- Verifica si TODOS los elementos cumplen la condición
todosPositivos = all (> 0) [1, 2, 3, 4]      -- True
todosPositivos2 = all (> 0) [1, -2, 3, 4]    -- False

-- any :: (a -> Bool) -> [a] -> Bool
-- Verifica si AL MENOS UNO cumple la condición
algunPositivo = any (> 0) [1, -2, -3, -4]    -- True
algunPositivo2 = any (> 0) [-1, -2, -3, -4]  -- False

-- Ejemplos más complejos
todasLasPalabrasLargas :: [String] -> Bool
todasLasPalabrasLargas = all (\palabra -> length palabra > 5)

hayNumerosPares :: [Int] -> Bool
hayNumerosPares = any even

todasLasEdadesValidas :: [Int] -> Bool
todasLasEdadesValidas edades = all (\edad -> edad >= 0 && edad <= 150) edades
```

**🔍 ¿Qué es `\palabra -> length palabra > 5`?**

- Es una **función lambda** (función anónima)
- **`\`**: Representa la letra griega λ (lambda)
- **`palabra`**: Parámetro de la función
- **`->`**: Separador entre parámetros y cuerpo
- **`length palabra > 5`**: Cuerpo de la función

### **🔄 Patrones con Booleanos**

#### **🎛️ Condicionales con `if-then-else`**

```haskell
-- Sintaxis: if condicion then valorSiTrue else valorSiFalse
absoluto :: Int -> Int
absoluto x = if x >= 0 then x else -x

maximo :: Int -> Int -> Int
maximo x y = if x > y then x else y

-- Con múltiples condiciones
clasificarEdad :: Int -> String
clasificarEdad edad =
    if edad < 13
    then "Niño"
    else if edad < 18
         then "Adolescente"
         else "Adulto"

-- Booleanos en if
mostrarEstado :: Bool -> String
mostrarEstado activo = if activo then "Encendido" else "Apagado"
```

**🔍 Diferencias con otros lenguajes:**

- En Haskell, `if-then-else` es una **expresión**, no una declaración
- **SIEMPRE** debe tener `else` (porque debe retornar un valor)
- Ambas ramas (`then` y `else`) deben retornar el **mismo tipo**

#### **🛡️ Guards (Guardas)**

Los **guards** son una forma más elegante de escribir múltiples condiciones:

```haskell
-- Sintaxis con guards
clasificarNota :: Int -> String
clasificarNota nota
  | nota >= 90 = "Excelente"
  | nota >= 80 = "Muy bueno"
  | nota >= 70 = "Bueno"
  | nota >= 60 = "Regular"
  | otherwise  = "Insuficiente"

-- Función con múltiples parámetros
calcularDescuento :: Int -> Double -> Double
calcularDescuento edad precio
  | edad >= 65          = precio * 0.8  -- 20% descuento adultos mayores
  | edad <= 12          = precio * 0.5  -- 50% descuento niños
  | precio > 100        = precio * 0.9  -- 10% descuento compras grandes
  | otherwise           = precio

-- Guards con condiciones complejas
puedeConducir :: Int -> Bool -> Bool -> Bool
puedeConducir edad tieneLicencia tieneVehiculo
  | edad < 18                                = False
  | not tieneLicencia                        = False
  | not tieneVehiculo                        = False
  | edad >= 18 && tieneLicencia && tieneVehiculo = True
  | otherwise                                = False
```

**🔍 ¿Qué es `otherwise`?**

- Es simplemente **`True`** con un nombre más legible
- **Siempre** debe ser la última condición
- Actúa como el "caso por defecto"

```haskell
-- Estas definiciones son equivalentes:
ejemplo1 x
  | x > 0 = "positivo"
  | otherwise = "no positivo"

ejemplo2 x
  | x > 0 = "positivo"
  | True = "no positivo"
```

### **🧪 Operadores de Comparación Avanzados**

#### **🔗 Comparaciones Encadenadas**

```haskell
-- En lugar de x >= a && x <= b, puedes usar:
estaEnRango :: Int -> Bool
estaEnRango x = x >= 1 && x <= 10

-- Para múltiples comparaciones:
esTrianguloValido :: Double -> Double -> Double -> Bool
esTrianguloValido a b c =
    a + b > c &&
    a + c > b &&
    b + c > a &&
    a > 0 && b > 0 && c > 0

-- Versión más legible con guards
esTrianguloValido' :: Double -> Double -> Double -> Bool
esTrianguloValido' a b c
  | a <= 0 || b <= 0 || c <= 0 = False
  | a + b <= c = False
  | a + c <= b = False
  | b + c <= a = False
  | otherwise = True
```

#### **🎯 Función `compare`**

```haskell
-- compare :: Ord a => a -> a -> Ordering
-- Retorna: LT (menor), EQ (igual), GT (mayor)

comparacion1 = compare 5 3   -- GT (Greater Than)
comparacion2 = compare 2 7   -- LT (Less Than)
comparacion3 = compare 4 4   -- EQ (Equal)

-- Usando compare con guards
clasificarComparacion :: Int -> Int -> String
clasificarComparacion x y = case compare x y of
    LT -> "x es menor que y"
    EQ -> "x es igual a y"
    GT -> "x es mayor que y"

-- Ordenamiento personalizado
compararPorLongitud :: String -> String -> Ordering
compararPorLongitud s1 s2 = compare (length s1) (length s2)
```

**🔍 ¿Qué es `Ordering`?**

- Es un tipo de datos con tres valores: `LT`, `EQ`, `GT`
- **`LT`**: Less Than (menor que)
- **`EQ`**: Equal (igual)
- **`GT`**: Greater Than (mayor que)

### **🎨 Ejemplos Prácticos**

#### **🔐 Validador de Password**

```haskell
-- Validador completo de contraseña
validarPassword :: String -> (Bool, [String])
validarPassword password = (esValida, errores)
  where
    longitud = length password
    tieneMayuscula = any isUpper password
    tieneMinuscula = any isLower password
    tieneNumero = any isDigit password
    tieneEspecial = any (`elem` "!@#$%^&*") password

    esValida = longitud >= 8 &&
               tieneMayuscula &&
               tieneMinuscula &&
               tieneNumero

    errores = concat [
        if longitud < 8 then ["Mínimo 8 caracteres"] else [],
        if not tieneMayuscula then ["Incluir mayúsculas"] else [],
        if not tieneMinuscula then ["Incluir minúsculas"] else [],
        if not tieneNumero then ["Incluir números"] else []
      ]

-- Función de fortaleza
fortalezaPassword :: String -> String
fortalezaPassword password
  | not tieneRequisitosBasicos = "Muy débil"
  | length password < 10 = "Débil"
  | not tieneCaracteresEspeciales = "Regular"
  | length password < 15 = "Fuerte"
  | otherwise = "Muy fuerte"
  where
    tieneRequisitosBasicos =
        length password >= 8 &&
        any isUpper password &&
        any isLower password &&
        any isDigit password
    tieneCaracteresEspeciales = any (`elem` "!@#$%^&*") password
```

#### **📅 Calculadora de Fecha**

```haskell
-- Verificar si un año es bisiesto
esBisiesto :: Int -> Bool
esBisiesto año
  | año `mod` 400 == 0 = True   -- Divisible por 400
  | año `mod` 100 == 0 = False  -- Divisible por 100 pero no por 400
  | año `mod` 4 == 0 = True     -- Divisible por 4 pero no por 100
  | otherwise = False           -- No divisible por 4

-- Días en cada mes
diasEnMes :: Int -> Int -> Int
diasEnMes mes año
  | mes `elem` [1,3,5,7,8,10,12] = 31
  | mes `elem` [4,6,9,11] = 30
  | mes == 2 && esBisiesto año = 29
  | mes == 2 = 28
  | otherwise = error "Mes inválido"

-- Validar fecha
esFechaValida :: Int -> Int -> Int -> Bool
esFechaValida dia mes año
  | año < 1 = False
  | mes < 1 || mes > 12 = False
  | dia < 1 || dia > diasEnMes mes año = False
  | otherwise = True
```

#### **🎯 Sistema de Calificaciones**

```haskell
-- Tipo de datos para calificaciones
data Calificacion = A | B | C | D | F deriving (Show, Eq)

-- Convertir nota numérica a letra
notaALetra :: Double -> Calificacion
notaALetra nota
  | nota >= 90 = A
  | nota >= 80 = B
  | nota >= 70 = C
  | nota >= 60 = D
  | otherwise = F

-- Verificar si pasó el curso
pasoCurso :: [Double] -> Bool
pasoCurso notas =
    let promedio = sum notas / fromIntegral (length notas)
        tieneF = any (< 60) notas
        mayoriaAprobatoria = length (filter (>= 70) notas) > length notas `div` 2
    in promedio >= 70 && not tieneF && mayoriaAprobatoria

-- Calcular GPA (Grade Point Average)
calcularGPA :: [Calificacion] -> Double
calcularGPA calificaciones =
    let puntos = map calificacionAPuntos calificaciones
        totalPuntos = sum puntos
        totalMaterias = fromIntegral (length calificaciones)
    in totalPuntos / totalMaterias
  where
    calificacionAPuntos A = 4.0
    calificacionAPuntos B = 3.0
    calificacionAPuntos C = 2.0
    calificacionAPuntos D = 1.0
    calificacionAPuntos F = 0.0
```

### **⚖️ Precedencia de Operadores**

#### **📊 Tabla de Precedencia (mayor a menor)**

```haskell
-- 1. not (precedencia más alta)
-- 2. comparaciones: ==, /=, <, <=, >, >=
-- 3. && (conjunción)
-- 4. || (disyunción - precedencia más baja)

-- Ejemplos de precedencia:
ejemplo1 = not True && False    -- (not True) && False = False
ejemplo2 = True || False && True -- True || (False && True) = True
ejemplo3 = 5 > 3 && 2 < 4       -- (5 > 3) && (2 < 4) = True

-- Usar paréntesis para claridad:
ejemplo1_claro = (not True) && False
ejemplo2_claro = True || (False && True)
ejemplo3_claro = (5 > 3) && (2 < 4)
```

#### **🧮 Ejemplos de Precedencia Compleja**

```haskell
-- Sin paréntesis (siguiendo precedencia)
expresion1 = not False || True && False
-- Evaluación: (not False) || (True && False) = True || False = True

-- Con paréntesis (cambiando precedencia)
expresion2 = not (False || True) && False
-- Evaluación: not (True) && False = False && False = False

-- Ejemplo práctico - validación de rango
enRangoValido = x >= 0 && x <= 100 || x == -1
-- Se evalúa como: ((x >= 0) && (x <= 100)) || (x == -1)
-- Acepta valores 0-100 OR el valor especial -1

-- Versión más clara con paréntesis
enRangoValido_claro = (x >= 0 && x <= 100) || (x == -1)
  where x = 50  -- ejemplo
```

### **💡 Optimizaciones y Buenas Prácticas**

#### **⚡ Evaluación Perezosa**

```haskell
-- Aprovecha la evaluación perezosa para optimización
buscarEnListaGrande :: [Int] -> Bool
buscarEnListaGrande lista =
    not (null lista) &&     -- Verifica primero si hay elementos
    head lista > 0          -- Solo si no está vacía, verifica el primero

-- Función segura que evita errores
divideSinError :: Int -> Int -> Bool
divideSinError x y = y /= 0 && x `mod` y == 0
-- Si y == 0, no evalúa x `mod` y (que causaría error)
```

#### **🎯 Funciones de Utilidad**

```haskell
-- Operador XOR (O exclusivo)
xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

-- Versión más elegante
xor' :: Bool -> Bool -> Bool
xor' a b = a /= b

-- Implicación lógica (a implica b)
implica :: Bool -> Bool -> Bool
implica False _ = True  -- False implica cualquier cosa
implica True b = b      -- True implica solo True

-- Equivalencia lógica
equivale :: Bool -> Bool -> Bool
equivale a b = a == b

-- Función útil: between (está entre)
between :: Ord a => a -> a -> a -> Bool
between minimo maximo valor = valor >= minimo && valor <= maximo
```

`Ord` es una keyword que indica que el tipo debe ser **ordenable** (tiene operadores de comparación), los tipos ordenables incluyen `Int`, `Float`, `Char`, `String`, etc.

### **🔧 Conversiones y Utilidades**

#### **🔄 Bool a Otros Tipos**

```haskell
-- Bool a Int
boolAInt :: Bool -> Int
boolAInt True = 1
boolAInt False = 0

-- Bool a String
boolAString :: Bool -> String
boolAString = show  -- "True" o "False"

-- Bool a String personalizado
boolASiNo :: Bool -> String
boolASiNo True = "Sí"
boolASiNo False = "No"

-- Int a Bool (0 = False, cualquier otro = True)
intABool :: Int -> Bool
intABool 0 = False
intABool _ = True

-- Maybe a Bool (Nothing = False, Just _ = True)
maybeTienValor :: Maybe a -> Bool
maybeTienValor Nothing = False
maybeTienValor (Just _) = True
```

### **🎯 Conceptos Clave**

#### **⚡ Evaluación Perezosa (Lazy Evaluation)**

- **`&&`** y **`||`** usan evaluación perezosa
- Si el resultado se puede determinar con el primer operando, el segundo **no se evalúa**
- Esto previene errores y mejora el rendimiento

#### **🎭 Expresiones vs Declaraciones**

- En Haskell, `if-then-else` es una **expresión** (retorna un valor)
- **Siempre** debe tener `else`
- Guards (`|`) son una alternativa más elegante para múltiples condiciones

#### **🔗 Composabilidad**

```haskell
-- Los booleanos se componen naturalmente
esValidoYCompleto :: String -> Bool
esValidoYCompleto texto =
    not (null texto) &&           -- No está vacío
    all isAlphaNum texto &&       -- Solo caracteres alfanuméricos
    length texto >= 3 &&          -- Longitud mínima
    any isUpper texto             -- Al menos una mayúscula
```

### **🧪 Ejercicios para Practicar**

```haskell
-- 1. Verificar si un número está en un rango (inclusivo)
estaEnRango :: Int -> Int -> Int -> Bool
estaEnRango minimo maximo numero = numero >= minimo && numero <= maximo
-- Ejemplo: estaEnRango 1 10 5 = True

-- 2. Verificar si un año es bisiesto
esBisiesto :: Int -> Bool
esBisiesto año = (año `mod` 4 == 0 && año `mod` 100 /= 0) || (año `mod` 400 == 0)
-- Ejemplo: esBisiesto 2020 = True, esBisiesto 1900 = False

-- 3. Validar triángulo (tres lados pueden formar un triángulo válido)
esTrianguloValido :: Double -> Double -> Double -> Bool
esTrianguloValido a b c = a + b > c && a + c > b && b + c > a
-- Ejemplo: esTrianguloValido 3 4 5 = True, esTrianguloValido 1 2 3 = False

-- 4. Verificar si todas las palabras en una lista tienen más de 3 caracteres
todasPalabrasLargas :: [String] -> Bool
todasPalabrasLargas palabras = all (\p -> length p > 3) palabras
-- Ejemplo: todasPalabrasLargas ["hola", "mundo"] = True, todasPalabrasLargas ["hola", "a"] = False

-- 5. Función que determine si un estudiante pasa (promedio >= 70 y no más de 3 faltas)
estudiantePasa :: [Double] -> Int -> Bool
estudiantePasa notas faltas = promedio >= 70 && faltas <= 3
  where promedio = sum notas / fromIntegral (length notas)
```

### **🚀 Siguiente Paso**

¡Excelente! Ya dominas los booleanos y operadores lógicos en Haskell. El siguiente tema es **Comparaciones y Precedencia**, donde profundizaremos en el ordenamiento y precedencia de operadores.

**¿Listo para continuar con comparaciones y precedencia?** ⚖️

## Comparaciones y Precedencia

El sistema de comparaciones y precedencia de operadores en Haskell es fundamental para escribir código correcto y expresivo. Entender cómo se evalúan las expresiones y en qué orden te ayudará a evitar errores sutiles y escribir código más claro.

### **⚖️ Sistema de Comparaciones en Haskell**

#### **🔍 La Typeclass `Eq` (Igualdad)**

**🔍 ¿Qué es una Typeclass?**

- Una **typeclass** es como un **contrato** o **interfaz** que define qué operaciones puede realizar un tipo
- **NO es una clase de POO** - es más como una interfaz en Java/C#
- Define **comportamiento compartido** sin datos
- Los tipos pueden "implementar" o "ser instancia de" una typeclass

La typeclass `Eq` define los operadores de igualdad y desigualdad:

```haskell
-- Definición simplificada de Eq
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

-- Implementación por defecto (mutuamente recursivas)
x /= y = not (x == y)
x == y = not (x /= y)
```

**🔍 Desglosando la sintaxis:**

- **`class`**: Palabra clave para definir una typeclass (NO es clase de POO)
- **`Eq`**: Nombre de la typeclass
- **`a`**: Variable de tipo genérico (como T en otros lenguajes)
- **`where`**: Palabra clave que introduce las definiciones de funciones
- **`(==) :: a -> a -> Bool`**: Signatura de la función de igualdad
  - Toma dos valores del mismo tipo `a`
  - Retorna un `Bool`
- **`(/=) :: a -> a -> Bool`**: Signatura de la función de desigualdad
- **`x /= y = not (x == y)`**: Implementación por defecto
  - Si defines `==`, obtienes `/=` gratis
  - Si defines `/=`, obtienes `==` gratis

**🔍 ¿Qué significa `Eq a`?**

- "El tipo `a` tiene operaciones de igualdad"
- "Puedes comparar valores del tipo `a` por igualdad"
- Solo necesitas implementar UNO de los dos operadores, el otro se deriva automáticamente

#### **🧪 Ejemplos con `Eq`**

```haskell
-- Tipos básicos que son instancias de Eq
numero1 = 5 == 5        -- True
numero2 = 3 /= 7        -- True

texto1 = "hola" == "hola"    -- True
texto2 = "casa" /= "perro"   -- True

bool1 = True == False        -- False
bool2 = True /= False        -- True

char1 = 'a' == 'a'          -- True
char2 = 'x' /= 'y'          -- True

-- Listas (compara elemento por elemento)
lista1 = [1,2,3] == [1,2,3]     -- True
lista2 = [1,2] /= [1,2,3]       -- True (diferentes longitudes)

-- Tuplas (compara cada componente)
tupla1 = (1, "hola") == (1, "hola")     -- True
tupla2 = (1, 2) /= (2, 1)               -- True
```

**🔍 ¿Cómo funciona la comparación de listas?**

- Se comparan **elemento por elemento** de izquierda a derecha
- `[1,2,3] == [1,2,3]`: 1==1 ✓, 2==2 ✓, 3==3 ✓ → True
- `[1,2] == [1,2,3]`: 1==1 ✓, 2==2 ✓, pero longitudes diferentes → False

#### **🔍 La Typeclass `Ord` (Orden)**

La typeclass `Ord` **extiende** `Eq` y añade operadores de comparación:

```haskell
-- Definición simplificada de Ord
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<), (<=), (>), (>=) :: a -> a -> Bool
  max, min :: a -> a -> a

-- Ordering es un tipo con tres valores:
data Ordering = LT | EQ | GT
-- LT = Less Than (menor que)
-- EQ = Equal (igual)
-- GT = Greater Than (mayor que)
```

**🔍 Desglosando la sintaxis:**

- **`class Eq a => Ord a`**: "Para que un tipo sea `Ord`, primero debe ser `Eq`"
- **`=>`**: Operador de **restricción de clase** (constraint)
- **`compare :: a -> a -> Ordering`**: Función principal de comparación
- **`(<), (<=), (>), (>=)`**: Operadores de comparación
- **`max, min`**: Funciones para encontrar máximo y mínimo
- **`data Ordering = LT | EQ | GT`**: Tipo de datos con tres constructores

**🔍 ¿Qué significa `Eq a => Ord a`?**

- **Restricción de superclase**: `Ord` requiere que el tipo también sea `Eq`
- **Lógica**: Para ordenar cosas, necesitas poder compararlas por igualdad
- **Ejemplo**: Para saber si `x < y`, necesitas poder verificar si `x == y`

**🔍 ¿Qué es `Ordering`?**

- Un **tipo de datos** (como un enum en otros lenguajes)
- Tiene exactamente **tres valores posibles**:
  - **`LT`**: Less Than (menor que)
  - **`EQ`**: Equal (igual)
  - **`GT`**: Greater Than (mayor que)

#### **🧮 Operadores de Comparación**

```haskell
-- Menor que (<)
comparacion1 = 3 < 5         -- True
comparacion2 = 10 < 2        -- False

-- Menor o igual (<=)
comparacion3 = 4 <= 4        -- True
comparacion4 = 6 <= 3        -- False

-- Mayor que (>)
comparacion5 = 8 > 3         -- True
comparacion6 = 2 > 9         -- False

-- Mayor o igual (>=)
comparacion7 = 7 >= 7        -- True
comparacion8 = 3 >= 8        -- False

-- Función compare (la más fundamental)
resultado1 = compare 5 3     -- GT (5 es mayor que 3)
resultado2 = compare 2 7     -- LT (2 es menor que 7)
resultado3 = compare 4 4     -- EQ (4 es igual a 4)
```

**🔍 ¿Por qué `compare` es fundamental?**

- Todos los otros operadores se pueden derivar de `compare`
- `x < y` es equivalente a `compare x y == LT`
- `x == y` es equivalente a `compare x y == EQ`
- `x > y` es equivalente a `compare x y == GT`

### **📊 Comparación de Diferentes Tipos**

#### **🔢 Números**

```haskell
-- Enteros
entero1 = 10 > 5             -- True
entero2 = 3 <= 3             -- True

-- Decimales
decimal1 = 3.14 > 2.71       -- True
decimal2 = 1.0 == 1.0        -- True

-- ⚠️ Cuidado con la precisión de punto flotante
precision1 = 0.1 + 0.2 == 0.3    -- False! (problemas de precisión)
precision2 = abs ((0.1 + 0.2) - 0.3) < 0.0001  -- True (comparación segura)

-- Diferentes tipos numéricos requieren conversión
-- numero_mixto = 5 == 5.0    -- ¡ERROR! Tipos incompatibles
numero_ok = fromIntegral 5 == 5.0  -- True
```

**🔍 ¿Por qué `0.1 + 0.2 /= 0.3`?**

- Los números de **punto flotante** usan representación **binaria**
- Algunos decimales **no se pueden representar exactamente** en binario
- **0.1** en decimal es **infinito** en binario (como 1/3 = 0.333...)
- Siempre hay pequeños **errores de redondeo**

**🔍 ¿Qué es `fromIntegral`?**

- Función que **convierte** entre diferentes tipos numéricos
- **`fromIntegral :: (Integral a, Num b) => a -> b`**
- **`Integral`**: Tipos enteros (Int, Integer)
- **`Num`**: Tipos numéricos en general
- Útil para convertir `Int` a `Double`, etc.

**🔍 ¿Qué es `abs`?**

- Función de **valor absoluto**
- **`abs :: Num a => a -> a`**
- Convierte números negativos en positivos
- `abs (-5) = 5`, `abs 3 = 3`

```haskell
-- Función para comparar floats con tolerancia
compararFloats :: Double -> Double -> Double -> Bool
compararFloats x y tolerancia = abs (x - y) < tolerancia

-- Uso seguro
esIgual = compararFloats (0.1 + 0.2) 0.3 0.0001  -- True
```

#### **📝 Strings (Orden Lexicográfico)**

```haskell
-- Orden alfabético (lexicográfico)
string1 = "abc" < "def"         -- True ('a' < 'd')
string2 = "casa" < "perro"      -- True ('c' < 'p')
string3 = "auto" > "avión"      -- False ('t' < 'v')

-- Comparación carácter por carácter
string4 = "casa" < "casamiento" -- True (prefijo)
string5 = "abc" < "abd"         -- True ('c' < 'd')

-- ¡Cuidado con mayúsculas y minúsculas!
string6 = "ABC" < "abc"         -- True (ASCII: mayúsculas < minúsculas)
string7 = "Zorro" < "abeja"     -- True ('Z' < 'a' en ASCII)

-- Para comparación sin importar mayúsculas:
import Data.Char (toLower)

compararSinCaso :: String -> String -> Ordering
compararSinCaso s1 s2 = compare (map toLower s1) (map toLower s2)

-- Ejemplo:
resultado = compararSinCaso "Zorro" "abeja"  -- GT
```

**🔍 ¿Qué es "orden lexicográfico"?**

- Es el **orden de diccionario**
- Se compara **carácter por carácter** de izquierda a derecha
- Usa los **códigos ASCII/Unicode** para comparar caracteres
- Como buscar palabras en un diccionario

**🔍 ¿Cómo funciona paso a paso?**

```haskell
-- "casa" vs "perro":
-- Posición 0: 'c' vs 'p' → 'c' < 'p' (ASCII 99 < 112)
-- Por tanto "casa" < "perro" = True

-- "casa" vs "casamiento":
-- Posición 0: 'c' == 'c' ✓
-- Posición 1: 'a' == 'a' ✓
-- Posición 2: 's' == 's' ✓
-- Posición 3: 'a' == 'a' ✓
-- "casa" termina, "casamiento" continúa
-- Por tanto "casa" < "casamiento" = True (prefijo)
```

**🔍 ¿Qué son los códigos ASCII/Unicode?**

- **ASCII**: Estándar de codificación de caracteres (0-127)
- **Unicode**: Extensión que incluye todos los caracteres del mundo
- Cada carácter tiene un **número único**:
  - `'A'` = 65, `'B'` = 66, ..., `'Z'` = 90
  - `'a'` = 97, `'b'` = 98, ..., `'z'` = 122
  - `'0'` = 48, `'1'` = 49, ..., `'9'` = 57
- Por eso `'Z' < 'a'` (90 < 97)

**🔍 ¿Qué hace `map toLower`?**

- **`map`**: Aplica una función a cada elemento de una lista
- **`toLower`**: Convierte un carácter a minúscula
- **`map toLower "Hola"`** = **`"hola"`**
- Permite comparación **insensible a mayúsculas**

#### **🔤 Caracteres**

```haskell
-- Comparación por código ASCII/Unicode
char1 = 'a' < 'b'           -- True (97 < 98)
char2 = 'A' < 'a'           -- True (65 < 97)
char3 = '1' < '2'           -- True (49 < 50)
char4 = '9' < 'A'           -- True (57 < 65)

-- Orden ASCII: números < mayúsculas < minúsculas
orden = ['9', 'A', 'a']     -- Ordenado ascendentemente

-- Verificar códigos con ord
import Data.Char (ord)
codigo_9 = ord '9'          -- 57
codigo_A = ord 'A'          -- 65
codigo_a = ord 'a'          -- 97
```

**🔍 ¿Qué es `ord`?**

- Función que **convierte** un carácter a su código ASCII/Unicode
- **`ord :: Char -> Int`**
- **Ejemplo**: `ord 'A'` = 65
- **Contrario**: `chr :: Int -> Char` convierte número a carácter

**🔍 ¿Por qué este orden?**

- **Diseño histórico** del código ASCII
- **Números** (48-57) vienen primero
- **Mayúsculas** (65-90) vienen después
- **Minúsculas** (97-122) vienen al final
- Por eso `'9' < 'A' < 'a'`

#### **📋 Listas**

```haskell
-- Las listas se comparan lexicográficamente (como strings)
lista1 = [1,2,3] < [1,2,4]      -- True (3 < 4)
lista2 = [1,2] < [1,2,3]        -- True (prefijo)
lista3 = [2] > [1,9,9,9]        -- True (2 > 1, no importa el resto)

-- Lista vacía es menor que cualquier lista no vacía
lista4 = [] < [1]               -- True
lista5 = [] < [-1000]           -- True

-- Comparación elemento por elemento
lista6 = ["abc", "def"] < ["abc", "xyz"]  -- True ("def" < "xyz")
```

**🔍 ¿Cómo se comparan listas paso a paso?**

```haskell
-- [1,2,3] vs [1,2,4]:
-- Posición 0: 1 == 1 ✓
-- Posición 1: 2 == 2 ✓
-- Posición 2: 3 < 4 ✓
-- Por tanto [1,2,3] < [1,2,4] = True

-- [1,2] vs [1,2,3]:
-- Posición 0: 1 == 1 ✓
-- Posición 1: 2 == 2 ✓
-- [1,2] termina, [1,2,3] continúa
-- Por tanto [1,2] < [1,2,3] = True (prefijo)

-- [2] vs [1,9,9,9]:
-- Posición 0: 2 > 1
-- Por tanto [2] > [1,9,9,9] = True (primera diferencia decide)
```

#### **🎭 Tuplas**

```haskell
-- Las tuplas se comparan componente por componente
tupla1 = (1, 2) < (1, 3)        -- True (1 == 1, 2 < 3)
tupla2 = (2, 1) > (1, 999)      -- True (2 > 1, no importa el resto)
tupla3 = (1, "abc") < (1, "def") -- True (1 == 1, "abc" < "def")

-- Tuplas de diferentes tamaños NO se pueden comparar
-- tupla_error = (1, 2) == (1, 2, 3)  -- ¡ERROR de tipos!

-- Orden de prioridad: primer elemento tiene mayor prioridad
tupla4 = (1, 999) < (2, 0)      -- True (1 < 2)
```

**🔍 ¿Cómo funciona la comparación de tuplas?**

- Se compara **componente por componente** de izquierda a derecha
- El **primer componente** tiene **mayor prioridad**
- Si el primer componente decide, **no se evalúan** los demás
- **Ejemplo**: `(2, 1) > (1, 999)` → 2 > 1, por tanto True

### **⚖️ Precedencia de Operadores**

#### **📊 Tabla Completa de Precedencia**

En Haskell, los operadores tienen diferentes **niveles de precedencia** (0-9, donde 9 es la más alta):

```haskell
-- PRECEDENCIA 9 (MÁS ALTA)
-- . (composición de funciones)
-- !! (indexación de listas)

-- PRECEDENCIA 8
-- ^, ^^, ** (exponenciación)

-- PRECEDENCIA 7
-- *, /, `div`, `mod`, `rem`, `quot` (multiplicación/división)

-- PRECEDENCIA 6
-- +, - (suma/resta)

-- PRECEDENCIA 5
-- : (cons), ++ (concatenación)

-- PRECEDENCIA 4
-- ==, /=, <, <=, >, >=, `elem`, `notElem` (comparaciones)

-- PRECEDENCIA 3
-- && (conjunción lógica)

-- PRECEDENCIA 2
-- || (disyunción lógica)

-- PRECEDENCIA 1
-- >>, >>= (operadores de mónadas)

-- PRECEDENCIA 0 (MÁS BAJA)
-- $, `seq` (aplicación de función)
```

**🔍 ¿Qué es precedencia?**

- **Orden de evaluación** de operadores en una expresión
- **Mayor precedencia** = se evalúa **primero**
- **Menor precedencia** = se evalúa **después**
- Como en matemáticas: `*` antes que `+`

**🔍 ¿Qué son esos operadores?**

- **`.`**: Composición de funciones (veremos más adelante)
- **`!!`**: Acceso a elemento de lista por índice
- **`^`**: Exponenciación (base cualquiera, exponente entero)
- **`div`**: División entera
- **`:`**: Constructor de lista (cons)
- **`++`**: Concatenación de listas
- **`elem`**: Verificar si elemento está en lista
- **`&&`**: AND lógico
- **`||`**: OR lógico
- **`$`**: Aplicación de función con precedencia baja

#### **🧮 Ejemplos de Precedencia**

```haskell
-- Precedencia aritmética
expr1 = 2 + 3 * 4           -- 2 + (3 * 4) = 14
expr2 = 2 * 3 + 4           -- (2 * 3) + 4 = 10
expr3 = 2^3 * 4             -- (2^3) * 4 = 32

-- Precedencia con comparaciones
expr4 = 2 + 3 == 5          -- (2 + 3) == 5 = True
expr5 = 2 == 2 + 0          -- 2 == (2 + 0) = True
expr6 = not True == False   -- (not True) == False = True

-- Precedencia lógica
expr7 = True || False && False   -- True || (False && False) = True
expr8 = not False || True && False -- (not False) || (True && False) = True

-- Comparaciones encadenadas
expr9 = 1 < 2 && 2 < 3      -- (1 < 2) && (2 < 3) = True
expr10 = 1 == 1 || 2 > 3    -- (1 == 1) || (2 > 3) = True
```

**🔍 ¿Por qué `expr1 = 2 + 3 * 4` es 14 y no 20?**

- **`*`** tiene precedencia 7
- **`+`** tiene precedencia 6
- **Mayor precedencia** se evalúa **primero**
- Por tanto: `2 + (3 * 4) = 2 + 12 = 14`

**🔍 ¿Qué es `not`?**

- Operador de **negación lógica**
- **`not :: Bool -> Bool`**
- **`not True = False`**, **`not False = True`**
- Tiene **precedencia muy alta** (9)

#### **🎯 Asociatividad**

Además de precedencia, los operadores tienen **asociatividad**:

```haskell
-- ASOCIATIVOS POR LA IZQUIERDA (LEFT-ASSOCIATIVE)
-- +, -, *, /, ==, /=, <, <=, >, >=
expr1 = 10 - 3 - 2          -- (10 - 3) - 2 = 5
expr2 = 12 / 4 / 3          -- (12 / 4) / 3 = 1.0

-- ASOCIATIVOS POR LA DERECHA (RIGHT-ASSOCIATIVE)
-- ^, :, ++, &&, ||, $
expr3 = 2^3^2               -- 2^(3^2) = 2^9 = 512
expr4 = 1 : 2 : [3]         -- 1 : (2 : [3]) = [1,2,3]
expr5 = "a" ++ "b" ++ "c"   -- "a" ++ ("b" ++ "c") = "abc"

-- NO ASOCIATIVOS (NON-ASSOCIATIVE)
-- compare, elem, notElem
-- Esto significa que no puedes encadenarlos sin paréntesis:
-- expr_error = 1 `compare` 2 `compare` 3  -- ¡ERROR!
```

**🔍 ¿Qué es asociatividad?**

- **Dirección** en que se agrupan operadores de la **misma precedencia**
- **Izquierda**: `a op b op c` = `(a op b) op c`
- **Derecha**: `a op b op c` = `a op (b op c)`
- **No asociativo**: **NO** se puede encadenar

**🔍 ¿Por qué importa la asociatividad?**

```haskell
-- RESTA por la izquierda:
-- 10 - 3 - 2 = (10 - 3) - 2 = 7 - 2 = 5

-- Si fuera por la derecha (incorrecto):
-- 10 - 3 - 2 = 10 - (3 - 2) = 10 - 1 = 9

-- EXPONENCIACIÓN por la derecha (correcto):
-- 2^3^2 = 2^(3^2) = 2^9 = 512
// No (2^3)^2 = 8^2 = 64
```

**🔍 ¿Qué es el constructor `:`?**

- **Constructor de lista** (pronunciado "cons")
- **`(:) :: a -> [a] -> [a]`**
- Agrega un elemento **al frente** de una lista
- **`1 : [2,3] = [1,2,3]`**
- **`'h' : "ola" = "hola"`** (porque String = [Char])

### **🧪 Casos Especiales y Trampas Comunes**

#### **⚠️ Errores de Precedencia**

```haskell
-- ❌ Error común: precedencia incorrecta
problema1 = not True && False    -- (not True) && False = False
-- Se lee como: (not True) && False, no not (True && False)

-- ✅ Solución con paréntesis
solucion1 = not (True && False)  -- not False = True

-- ❌ Error con operadores de igualdad
problema2 = 2 + 3 == 4 + 1       -- (2 + 3) == (4 + 1) = True
-- Esto está bien, pero puede ser confuso

-- ❌ Error con función de aplicación
-- problema3 = take 3 reverse [1,2,3,4,5]  -- ¡ERROR!
-- Se interpreta como: take 3 reverse ([1,2,3,4,5])

-- ✅ Solución con paréntesis o $
solucion3a = take 3 (reverse [1,2,3,4,5])   -- [5,4,3]
solucion3b = take 3 $ reverse [1,2,3,4,5]   -- [5,4,3]
```

**🔍 ¿Por qué `problema3` da error?**

- **`take`** es una función que necesita **dos argumentos**
- **`take 3 reverse [1,2,3,4,5]`** se interpreta como:
- **`take 3 reverse ([1,2,3,4,5])`**
- **`reverse`** es una función, **no un argumento**
- Por eso **necesitas paréntesis** o **`$`**

**🔍 ¿Qué son `take` y `reverse`?**

- **`take :: Int -> [a] -> [a]`**: Toma los primeros n elementos
- **`reverse :: [a] -> [a]`**: Invierte una lista
- **`take 3 [1,2,3,4,5] = [1,2,3]`**
- **`reverse [1,2,3,4,5] = [5,4,3,2,1]`**

#### **🔍 El Operador `$` (Aplicación de Función)**

El operador `$` tiene la precedencia **más baja** y es muy útil:

```haskell
-- Definición de $
($) :: (a -> b) -> a -> b
f $ x = f x

-- ¿Para qué sirve? Para evitar paréntesis
sin_dollar = sqrt (abs (sin (pi/4)))
con_dollar = sqrt $ abs $ sin $ pi/4

-- Ejemplos prácticos
expr1 = show $ 2 + 3            -- show (2 + 3) = "5"
expr2 = length $ words "hola mundo"  -- length (words "hola mundo") = 2
expr3 = sum $ map (^2) [1,2,3]  -- sum (map (^2) [1,2,3]) = 14

-- Múltiples funciones
procesamiento = reverse $ take 3 $ drop 2 $ [1,2,3,4,5,6]
-- Equivale a: reverse (take 3 (drop 2 [1,2,3,4,5,6]))
-- Resultado: [5,4,3]
```

**🔍 ¿Qué hace exactamente `$`?**

- **`f $ x`** es **exactamente igual** a **`f x`**
- **La diferencia**: `$` tiene **precedencia 0** (la más baja)
- **Todo a la derecha** se evalúa **primero**
- **Evita paréntesis** anidados

**🔍 ¿Qué significan esas signaturas raras?**

- **`($) :: (a -> b) -> a -> b`**:
  - Toma una **función** `(a -> b)`
  - Toma un **argumento** `a`
  - Retorna el **resultado** `b`
- **`show :: Show a => a -> String`**: Convierte algo a String
- **`words :: String -> [String]`**: Divide string en palabras
- **`length :: [a] -> Int`**: Cuenta elementos de lista
- **`sum :: Num a => [a] -> a`**: Suma elementos de lista

**🔍 ¿Por qué se usa `$`?**

- **Precedencia baja**: Todo a la derecha se evalúa primero
- **Legibilidad**: Se lee más naturalmente de izquierda a derecha
- **Menos paréntesis**: Reduce el "ruido visual"

#### **🔗 Comparaciones Encadenadas Seguras**

```haskell
-- ❌ En algunos lenguajes puedes hacer: 1 < x < 10
-- En Haskell NO funciona así:
-- problema = 1 < x < 10  -- ¡ERROR de tipos!

-- ✅ Debes usar && explícitamente
enRango :: (Ord a) => a -> a -> a -> Bool
enRango minimo maximo valor = minimo <= valor && valor <= maximo

-- O crear una función helper
between :: (Ord a) => a -> a -> a -> Bool
between bajo alto x = bajo <= x && x <= alto

-- Uso
esValido = between 1 10 5       -- True
esValido2 = enRango 0 100 50    -- True
```

**🔍 ¿Por qué `1 < x < 10` no funciona?**

- **`1 < x`** retorna un **`Bool`**
- **`Bool < 10`** no tiene sentido (no puedes comparar Bool con Int)
- **Error de tipos**: tipos incompatibles

**🔍 ¿Qué significa `(Ord a) =>`?**

- **Restricción de tipo** (type constraint)
- "El tipo `a` debe ser una instancia de `Ord`"
- "Puedes usar operadores de comparación con `a`"
- **Sin esto**, no podrías usar `<=` en la función

### **🎨 Ejemplos Prácticos Avanzados**

#### **📊 Función de Ordenamiento Personalizada**

```haskell
-- Ordenar por longitud de string
import Data.List (sortBy)
import Data.Ord (comparing)

ordenarPorLongitud :: [String] -> [String]
ordenarPorLongitud = sortBy (comparing length)

-- Ejemplo:
palabras = ["casa", "a", "elefante", "sol"]
resultado = ordenarPorLongitud palabras  -- ["a", "sol", "casa", "elefante"]

-- Ordenar tuplas por segundo elemento
ordenarPorSegundo :: (Ord b) => [(a, b)] -> [(a, b)]
ordenarPorSegundo = sortBy (comparing snd)

pares = [("Juan", 25), ("Ana", 20), ("Luis", 30)]
ordenados = ordenarPorSegundo pares  -- [("Ana",20), ("Juan",25), ("Luis",30)]
```

**🔍 ¿Qué hace cada función?**

- **`import Data.List`**: Importa módulo con funciones de lista
- **`sortBy :: (a -> a -> Ordering) -> [a] -> [a]`**: Ordena con función personalizada
- **`comparing :: Ord a => (b -> a) -> b -> b -> Ordering`**: Crea función de comparación
- **`length :: [a] -> Int`**: Cuenta caracteres en string
- **`snd :: (a, b) -> b`**: Extrae segundo elemento de tupla

**🔍 ¿Cómo funciona `comparing length`?**

- **`comparing`** toma una función **`b -> a`**
- La convierte en una función de comparación **`b -> b -> Ordering`**
- **`comparing length "casa" "a"`** = **`compare (length "casa") (length "a")`**
- = **`compare 4 1`** = **`GT`**

#### **🎯 Validador de Rangos Múltiples**

```haskell
-- Verificar si un valor está en múltiples rangos
estaEnRangos :: (Ord a) => [(a, a)] -> a -> Bool
estaEnRangos rangos valor = any (\(min, max) -> min <= valor && valor <= max) rangos

-- Ejemplo: horarios de atención
horariosAtencion = [(9, 12), (14, 18)]  -- 9-12 y 14-18
estaAbierto = estaEnRangos horariosAtencion 15  -- True
estaCerrado = estaEnRangos horariosAtencion 13  -- False

-- Validador de caracteres válidos
esCaracterValido :: Char -> Bool
esCaracterValido c = estaEnRangos [('a', 'z'), ('A', 'Z'), ('0', '9')] c

-- Ejemplos
valido1 = esCaracterValido 'a'    -- True
valido2 = esCaracterValido 'Z'    -- True
valido3 = esCaracterValido '5'    -- True
valido4 = esCaracterValido '!'    -- False
```

**🔍 ¿Qué hace `any`?**

- **`any :: (a -> Bool) -> [a] -> Bool`**
- Verifica si **AL MENOS UN** elemento cumple la condición
- **`any even [1,3,4,7] = True`** (porque 4 es par)

**🔍 ¿Qué es `\(min, max) -> min <= valor && valor <= max`?**

- **Función lambda** (función anónima)
- **`\`**: Símbolo lambda
- **`(min, max)`**: **Pattern matching** en tupla
- **`->`**: Separador entre parámetros y cuerpo
- Verifica si `valor` está entre `min` y `max`

#### **🔍 Comparador de Versiones**

```haskell
-- Representar una versión como lista de números
type Version = [Int]

-- Comparar versiones (lexicográficamente)
compararVersiones :: Version -> Version -> Ordering
compararVersiones = compare

-- Ejemplos
version1 = [1, 2, 3]     -- v1.2.3
version2 = [1, 2, 10]    -- v1.2.10
version3 = [1, 3, 0]     -- v1.3.0

resultado1 = compararVersiones version1 version2  -- LT (1.2.3 < 1.2.10)
resultado2 = compararVersiones version2 version3  -- LT (1.2.10 < 1.3.0)

-- Verificar si una versión es compatible (>= versión mínima)
esCompatible :: Version -> Version -> Bool
esCompatible versionMinima versionActual = versionActual >= versionMinima

compatible = esCompatible [1, 2, 0] [1, 2, 5]  -- True
```

**🔍 ¿Qué es `type Version = [Int]`?**

- **Alias de tipo** (type alias)
- **`Version`** es **otro nombre** para **`[Int]`**
- **No crea un tipo nuevo**, solo mejora legibilidad
- **`Version`** y **`[Int]`** son **completamente intercambiables**

**🔍 ¿Por qué las versiones se comparan lexicográficamente?**

- **`[1,2,3]`** vs **`[1,2,10]`**:
  - Posición 0: 1 == 1 ✓
  - Posición 1: 2 == 2 ✓
  - Posición 2: 3 < 10 ✓
  - Por tanto [1,2,3] < [1,2,10] = True (v1.2.3 < v1.2.10)

#### **📈 Sistema de Calificaciones con Comparaciones**

```haskell
-- Tipo para calificaciones
data Calificacion = A | B | C | D | F deriving (Eq, Ord, Show)

-- Al derivar Ord, el orden es el orden de declaración: A < B < C < D < F

-- Función para obtener la mejor calificación
mejorCalificacion :: [Calificacion] -> Calificacion
mejorCalificacion = minimum  -- minimum funciona porque A < B < C < D < F

-- Verificar si todas las calificaciones son aprobatorias
todasAprobatorias :: [Calificacion] -> Bool
todasAprobatorias califs = all (<= C) califs  -- A, B, C son aprobatorias

-- Ejemplos
calificaciones = [B, A, C, B]
mejor = mejorCalificacion calificaciones     -- A
aprobado = todasAprobatorias calificaciones  -- True

calificaciones2 = [B, A, F, C]
aprobado2 = todasAprobatorias calificaciones2  -- False (por la F)
```

**🔍 ¿Qué es `data Calificacion = A | B | C | D | F`?**

- **Definición de tipo de datos** algebraico
- **`data`**: Palabra clave para definir tipos
- **`Calificacion`**: Nombre del tipo
- **`A | B | C | D | F`**: **Constructores** separados por **`|`**
- Cada calificación es **exactamente uno** de estos valores

**🔍 ¿Qué significa `deriving (Eq, Ord, Show)`?**

- **`deriving`**: Palabra clave para **generación automática**
- **`Eq`**: Genera **`==`** y **`/=`** automáticamente
- **`Ord`**: Genera **`<`**, **`<=`**, etc. automáticamente
- **`Show`**: Genera **`show`** (conversión a String) automáticamente
- **Orden**: **A < B < C < D < F** (orden de declaración)

**🔍 ¿Qué hacen `minimum` y `all`?**

- **`minimum :: Ord a => [a] -> a`**: Encuentra el **elemento menor**
- **`all :: (a -> Bool) -> [a] -> Bool`**: Verifica si **todos** cumplen condición
- **`(<= C)`**: **Función parcialmente aplicada** (veremos más adelante)

### **🔧 Operadores Personalizados**

#### **⚙️ Definiendo Tus Propios Operadores**

```haskell
-- Operador para verificar si está en rango
(∈) :: (Ord a) => a -> (a, a) -> Bool
x ∈ (min, max) = min <= x && x <= max

-- Uso
estaEnRango = 5 ∈ (1, 10)       -- True
fueraDeRango = 15 ∈ (1, 10)     -- False

-- Operador para diferencia aproximada
(≈) :: Double -> Double -> Bool
x ≈ y = abs (x - y) < 0.0001

-- Uso
sonIguales = 0.1 + 0.2 ≈ 0.3    -- True

-- Operador de implicación lógica
(⟹) :: Bool -> Bool -> Bool
False ⟹ _ = True    -- False implica cualquier cosa
True ⟹ x = x        -- True implica solo True

-- Uso
implicacion1 = False ⟹ False    -- True
implicacion2 = True ⟹ False     -- False
```

**🔍 ¿Cómo se definen operadores personalizados?**

- **Símbolos especiales**: Puedes usar símbolos Unicode
- **Paréntesis**: **`(∈)`** convierte símbolo en función
- **Signatura normal**: Como cualquier función
- **Pattern matching**: Puedes usar patterns en argumentos

**🔍 ¿Qué es `_` en `False ⟹ _`?**

- **Wildcard pattern** (comodín)
- Significa "**cualquier valor**", no me importa cuál
- **No se usa** la variable, solo se hace match
- **`False ⟹ _`** = "Si el primer argumento es False, no importa el segundo"

**🔍 ¿Cómo definir precedencia para operadores personalizados?**

```haskell
-- Puedes especificar precedencia y asociatividad
infixl 7 ⊗  -- Asociativo por la izquierda, precedencia 7
(⊗) :: Int -> Int -> Int
x ⊗ y = x * 2 + y

infixr 6 ⊕  -- Asociativo por la derecha, precedencia 6
(⊕) :: Int -> Int -> Int
x ⊕ y = x + y * 2

-- Uso
resultado = 2 ⊗ 3 ⊕ 4    -- 2 ⊗ (3 ⊕ 4) = 2 ⊗ 11 = 26
-- No: (2 ⊗ 3) ⊕ 4 = 7 ⊕ 4 = 15
```

**🔍 ¿Qué significan `infixl` e `infixr`?**

- **`infixl`**: **Asociativo por la izquierda**
- **`infixr`**: **Asociativo por la derecha**
- **`infix`**: **No asociativo**
- **Número**: **Precedencia** (0-9)

### **🎯 Conceptos Clave**

#### **📚 Jerarquía de Typeclasses**

```haskell
-- Jerarquía básica:
-- Eq ← Ord
//
-- Para que un tipo sea Ord, debe ser Eq primero
-- Esto garantiza consistencia: si x < y, entonces x /= y

-- Ejemplo de implementación
data Semaforo = Rojo | Amarillo | Verde deriving (Eq, Ord, Show)

-- El orden será: Rojo < Amarillo < Verde (orden de declaración)
```

**🔍 ¿Por qué esta jerarquía tiene sentido?**

- **Orden implica igualdad**: Si puedes ordenar, debes poder comparar igualdad
- **Consistencia**: Si `x < y`, entonces **automáticamente** `x /= y`
- **Reutilización**: `Ord` puede **usar** las funciones de `Eq`

#### **⚡ Evaluación y Cortocircuito**

```haskell
-- Los operadores && y || usan evaluación perezosa
-- pero los operadores de comparación NO

-- Esto es seguro (evaluación perezosa):
seguro = False && error "¡No se evalúa!"  -- False

-- Esto NO es seguro (evaluación estricta):
-- peligroso = (error "¡Se evalúa!") == 5  -- ¡Exception!

-- Para comparaciones seguras, usa guardas:
compararSeguro :: Maybe Int -> Int -> Bool
compararSeguro Nothing _ = False
compararSeguro (Just x) y = x == y
```

**🔍 ¿Qué es evaluación perezosa vs estricta?**

- **Perezosa**: Solo evalúa **si es necesario**
- **Estricta**: **Siempre** evalúa todos los argumentos
- **`&&` y `||`**: Perezosos (pueden parar en el primer argumento)
- **Comparaciones**: Estrictas (evalúan ambos lados)

**🔍 ¿Qué es `error`?**

- **`error :: String -> a`**: Función que **termina el programa** con un mensaje
- **Solo para debugging** o casos "imposibles"
- **NO usar en código normal**

**🔍 ¿Qué es `Maybe Int`?**

- **Tipo que puede ser `Nothing` o `Just Int`**
- **Forma segura** de representar "valor que puede no existir"
- **`Nothing`**: No hay valor
- **`Just 5`**: El valor es 5

#### **🔗 Composición de Comparaciones**

```haskell
-- Puedes componer comparaciones usando la instancia Monoid de Ordering
import Data.Monoid (mappend)

compararPersonas :: (String, Int) -> (String, Int) -> Ordering
compararPersonas (nombre1, edad1) (nombre2, edad2) =
    compare edad1 edad2 `mappend` compare nombre1 nombre2
-- Primero compara por edad, en caso de empate compara por nombre

-- Ejemplo:
personas = [("Ana", 25), ("Juan", 25), ("Luis", 30)]
personasOrdenadas = sortBy compararPersonas personas
-- [("Ana",25), ("Juan",25), ("Luis",30)]
```

**🔍 ¿Qué es `mappend`?**

- **Operación de "combinación"** de la typeclass `Monoid`
- **Para `Ordering`**: Combina comparaciones de forma inteligente
- **Si el primero es `EQ`**, usa el segundo
- **Si no**, usa el primero

**🔍 ¿Cómo funciona la composición paso a paso?**

```haskell
-- ("Ana", 25) vs ("Juan", 25):
-- compare 25 25 = EQ (edades iguales)
-- compare "Ana" "Juan" = LT ("Ana" < "Juan")
-- EQ `mappend` LT = LT
// Por tanto ("Ana", 25) < ("Juan", 25)
```

### **⚠️ Errores Comunes y Mejores Prácticas**

#### **🚫 Problemas de Precisión**

```haskell
-- ❌ Nunca compares floats directamente
malo = (sqrt 2) ^ 2 == 2.0        -- Podría ser False

-- ✅ Usa tolerancia
epsilon = 1e-10
bueno = abs ((sqrt 2) ^ 2 - 2.0) < epsilon  -- True

-- ✅ Función helper
aproxIgual :: Double -> Double -> Bool
aproxIgual x y = abs (x - y) < 1e-10
```

**🔍 ¿Qué es `sqrt`?**

- **`sqrt :: Floating a => a -> a`**: Función de raíz cuadrada
- **`sqrt 4.0 = 2.0`**

**🔍 ¿Qué es `1e-10`?**

- **Notación científica**: **1 × 10⁻¹⁰**
- **0.0000000001**: Un número muy pequeño
- **Tolerancia** para comparaciones de punto flotante

#### **🔄 Comparaciones de Tipos Mixtos**

```haskell
-- ❌ No puedes comparar tipos diferentes directamente
-- problema = 5 == 5.0  -- ¡Error de tipos!

-- ✅ Convierte explícitamente
solucion1 = fromIntegral 5 == 5.0  -- True
solucion2 = 5 == round 5.0         -- True
```

**🔍 ¿Qué es `round`?**

- **`round :: (RealFrac a, Integral b) => a -> b`**
- **Redondea** un número decimal al entero más cercano
- **`round 3.7 = 4`**, **`round 3.2 = 3`**

#### **📝 Orden de Strings**

```haskell
-- ⚠️ Cuidado con el orden ASCII
problema = "Z" < "a"  -- True (¡Z viene antes que a en ASCII!)

-- ✅ Para orden alfabético real:
import Data.Char (toLower)

ordenAlfabetico :: String -> String -> Ordering
ordenAlfabetico s1 s2 = compare (map toLower s1) (map toLower s2)
```

### **🧪 Ejercicios para Practicar**

```haskell
-- 1. Función que verifique si tres números están en orden ascendente
enOrdenAscendente :: (Ord a) => a -> a -> a -> Bool
enOrdenAscendente x y z = x <= y && y <= z

-- 2. Función que encuentre el elemento del medio en una lista ordenada
elementoMedio :: (Ord a) => [a] -> Maybe a
elementoMedio xs
  | null xs = Nothing
  | otherwise = let sorted = sort xs
                    len = length sorted
                in Just (sorted !! (len `div` 2))

-- 3. Verificar si una lista está ordenada
estaOrdenada :: (Ord a) => [a] -> Bool
estaOrdenada [] = True
estaOrdenada [_] = True
estaOrdenada (x:y:xs) = x <= y && estaOrdenada (y:xs)

-- 4. Función que compare fechas (año, mes, día)
type Fecha = (Int, Int, Int)

compararFechas :: Fecha -> Fecha -> Ordering
compararFechas (año1, mes1, dia1) (año2, mes2, dia2) =
  compare año1 año2 `mappend`
  compare mes1 mes2 `mappend`
  compare dia1 dia2

-- 5. Operador que verifique si un número está cerca de otro
(~=) :: Double -> Double -> Bool
x ~= y = abs (x - y) < 0.001
```

**🔍 ¿Qué significan esos ejercicios?**

1. **`enOrdenAscendente`**: Verifica si tres valores van de menor a mayor
2. **`elementoMedio`**: Encuentra el elemento central de una lista ordenada
3. **`estaOrdenada`**: Verifica si lista ya está en orden ascendente
4. **`compararFechas`**: Compara fechas por año, luego mes, luego día
5. **`(~=)`**: Operador personalizado para "aproximadamente igual"

### **🚀 Siguiente Paso**

¡Excelente! Ya dominas las comparaciones y precedencia en Haskell. Has completado la sección **Tipos Básicos y Operadores**.

El siguiente gran tema es **Funciones y Definiciones**, donde aprenderás a crear y usar funciones, que son el corazón de la programación funcional.

**¿Listo para continuar con funciones?** 🔧
