# Sintaxis Básica de Haskell

Haskell es un lenguaje con una sintaxis única y bastante diferente a otros lenguajes del paradigma imperativo. Aún así, su sintaxis es bastante limpia y simple comparada con lenguajes como C o Java.

## Tipos de Datos Básicos

Haskell es un lenguaje fuertemente tipado, lo que significa que cada variable tiene un tipo de dato específico. Los tipos de datos básicos en Haskell incluyen:

### Enteros

Haskell maneja dos tipos de datos para números enteros, `Int` e `Integer`. La diferencia principal es que `Int` tiene un tamaño fijo (normalmente 64 bits), mientras que `Integer` puede crecer según sea necesario, permitiendo trabajar con números enteros de tamaño ilimitado.

```haskell
-- Entero de tamaño fijo
numeroFijo :: Int
numeroFijo = 42

-- Entero de tamaño ilimitado
numeroIlimitado :: Integer
numeroIlimitado = 123456789012345678901234567890
```

**🎯 ¿Cuándo usar cada uno?**

- **`Int`**: Para índices, contadores, operaciones donde sabes que los números serán pequeños
- **`Integer`**: Para cálculos matemáticos, factorial, fibonacci, cuando la precisión es crítica

### Números de Punto Flotante

Los números de punto flotante, los que tienen decimales, se representan con el tipo `Float` o `Double`. `Float` es de precisión simple y `Double` es de precisión doble.

```haskell
-- Número de punto flotante de precisión simple
numeroFloat :: Float
numeroFloat = 3.14

-- Número de punto flotante de precisión doble
numeroDouble :: Double
numeroDouble = 3.141592653589793
```

**💡 Recomendación**: Usa `Double` por defecto - mejor precisión y es el estándar en la mayoría de aplicaciones.

### Caracteres y Texto

```haskell
-- Carácter individual (entre comillas simples)
letra :: Char
letra = 'A'

inicial :: Char
inicial = 'D'

-- Cadena de texto (entre comillas dobles)
-- String es en realidad [Char] (lista de caracteres)
nombre :: String
nombre = "Diego"

mensaje :: String
mensaje = "¡Hola, mundo funcional!"

-- Texto unicode
emoji :: String
emoji = "🚀💻🎯"
```

**🔗 Operaciones con texto:**

```haskell
-- Concatenación de strings
saludo :: String
saludo = "Hola, " ++ nombre ++ "!"

-- Longitud de string
longitudNombre :: Int
longitudNombre = length nombre  -- 5
```

### Booleanos

Los valores booleanos solo pueden ser `True` o `False` (con mayúscula inicial).

```haskell
-- Valores booleanos básicos
verdadero :: Bool
verdadero = True

falso :: Bool
falso = False

-- Resultado de comparaciones
esMayorDeEdad :: Bool
esMayorDeEdad = 25 >= 18  -- True

esIgual :: Bool
esIgual = "Haskell" == "Python"  -- False
```

**⚡ Operadores lógicos:**

```haskell
-- AND lógico
resultado1 :: Bool
resultado1 = True && False  -- False

-- OR lógico
resultado2 :: Bool
resultado2 = True || False  -- True

-- NOT lógico
resultado3 :: Bool
resultado3 = not True  -- False

-- Combinaciones complejas
esValido :: Bool
esValido = (edad >= 18) && (not (nombre == ""))
  where
    edad = 25
    nombre = "Diego"
```

### Listas - Tu Nueva Estructura Favorita

Las listas son fundamentales en Haskell - homogéneas (todos los elementos del mismo tipo) y pueden ser infinitas.

```haskell
-- Lista de enteros
numeros :: [Int]
numeros = [1, 2, 3, 4, 5]

-- Lista de strings
nombres :: [String]
nombres = ["Ana", "Luis", "María"]

-- Lista de booleanos
estados :: [Bool]
estados = [True, False, True]

-- Lista vacía
vacia :: [Int]
vacia = []

-- Rangos (¡súper útiles!)
del1al10 :: [Int]
del1al10 = [1..10]  -- [1,2,3,4,5,6,7,8,9,10]

pares :: [Int]
pares = [2,4..20]  -- [2,4,6,8,10,12,14,16,18,20]

-- Lista infinita (¡evaluación perezosa!)
infinitos :: [Int]
infinitos = [1..]  -- [1,2,3,4,5,6,7,8,9,10,11,12...]
```

### Tuplas - Agrupando Datos Diferentes

Las tuplas pueden contener elementos de diferentes tipos y tienen tamaño fijo.

```haskell
-- Tupla de dos elementos (par)
coordenada :: (Int, Int)
coordenada = (5, 10)

-- Tupla de tres elementos (trío)
persona :: (String, Int, Bool)
persona = ("Diego", 25, True)  -- (nombre, edad, esProgramador)

-- Tupla mixta
configuracion :: (String, Double, [Int])
configuracion = ("servidor", 3.14, [80, 443, 8080])
```

**🔧 Acceso a elementos:**

```haskell
-- Para tuplas de 2 elementos
primerElemento :: Int
primerElemento = fst (5, 10)  -- 5

segundoElemento :: Int
segundoElemento = snd (5, 10)  -- 10

-- Para tuplas más grandes necesitas pattern matching (próximo tema)
```

## 💡 Comparación con Otros Lenguajes

| Haskell         | JavaScript         | Python            | Java            |
| --------------- | ------------------ | ----------------- | --------------- |
| `Int`           | `number`           | `int`             | `int`           |
| `Double`        | `number`           | `float`           | `double`        |
| `String`        | `string`           | `str`             | `String`        |
| `Bool`          | `boolean`          | `bool`            | `boolean`       |
| `[Int]`         | `number[]`         | `list[int]`       | `List<Integer>` |
| `(Int, String)` | `[number, string]` | `tuple[int, str]` | ❌              |

## 🧪 Experimentos en GHCi

Prueba estos comandos en el REPL para entender mejor los tipos:

```haskell
-- Ver el tipo de cualquier expresión
*Main> :type 42
42 :: Num a => a

*Main> :type "Hola"
"Hola" :: String

*Main> :type True
True :: Bool

*Main> :type [1, 2, 3]
[1, 2, 3] :: Num a => [a]

*Main> :type (5, "Hola")
(5, "Hola") :: Num a => (a, String)

-- Operaciones básicas
*Main> length "Haskell"
7

*Main> head [1, 2, 3, 4]
1

*Main> tail [1, 2, 3, 4]
[2,3,4]

*Main> take 3 [1..]
[1,2,3]
```

## 🎯 Conceptos Clave

### **🔒 Inmutabilidad**

```haskell
-- Una vez definido, un valor NUNCA cambia
x :: Int
x = 5
-- x = 10  -- ¡ERROR! No puedes "reasignar"
```

### **🎭 Inferencia de Tipos**

```haskell
-- Haskell puede deducir tipos automáticamente
numero = 42        -- Haskell infiere :: Num a => a
texto = "Hola"     -- Haskell infiere :: String
lista = [1, 2, 3]  -- Haskell infiere :: Num a => [a]

-- Pero es buena práctica escribir tipos explícitos
numeroExplicito :: Int
numeroExplicito = 42
```

### **⚡ Evaluación Perezosa**

```haskell
-- Las listas infinitas no se evalúan hasta que las necesitas
todosLosNumeros = [1..]
primerosTres = take 3 todosLosNumeros  -- Solo evalúa [1,2,3]
```

## 📝 Signatura de Tipos - Explicación Formal

### **¿Qué es una Signatura de Tipos?**

La **signatura de tipos** es la "etiqueta" que le dice a Haskell (y a ti) qué tipo de datos acepta y retorna una función.

```haskell
-- Estructura: nombre :: TipoEntrada -> TipoSalida
edad :: Int
edad = 25

-- Para funciones: nombre :: TipoParam1 -> TipoParam2 -> TipoRetorno
sumar :: Int -> Int -> Int
sumar x y = x + y
```

### **Anatomía de una Signatura:**

```haskell
-- Función simple
doble :: Int -> Int
--  ↑     ↑      ↑
-- nombre entrada salida

-- Función con múltiples parámetros
suma :: Int -> Int -> Int
--  ↑    ↑     ↑     ↑
-- nombre p1   p2   salida

-- Función más compleja
procesarTexto :: String -> [String] -> Bool -> String
--       ↑        ↑         ↑         ↑       ↑
--     nombre   param1    param2    param3  salida
```

### **¿Por qué son importantes?**

#### **🛡️ 1. Prevención de Errores**

```haskell
-- Sin signatura - fácil de cometer errores
miFuncion x y = x + y  -- ¿Qué tipos acepta?

-- Con signatura - cristalino
miFuncion :: Int -> Int -> Int
miFuncion x y = x + y  -- ¡Obvio que son enteros!
```

#### **📖 2. Documentación Automática**

```haskell
-- La signatura ES la documentación
calcularAreaCirculo :: Double -> Double
--                     ↑         ↑
--                   radio     área

-- Inmediatamente sabes qué hace sin leer el código
```

#### **⚡ 3. Optimización del Compilador**

```haskell
-- Haskell puede optimizar mejor cuando conoce los tipos exactos
velocidadLuz :: Double
velocidadLuz = 299792458.0  -- Compilador optimiza para Double específicamente
```

### **Casos Especiales:**

#### **🎭 Tipos Polimórficos (Genéricos)**

```haskell
-- 'a' puede ser cualquier tipo
primero :: [a] -> a
primero (x:_) = x

-- Funciona con cualquier lista:
-- primero [1,2,3] :: Int
-- primero ["a","b"] :: String
-- primero [True,False] :: Bool
```

#### **📦 Funciones que No Retornan Valor Útil**

```haskell
-- IO () significa "hace algo pero no retorna valor útil"
saludar :: String -> IO ()
saludar nombre = putStrLn ("Hola, " ++ nombre)
```

#### **🔗 Funciones de Orden Superior**

```haskell
-- Función que recibe otra función como parámetro
aplicarDosVeces :: (a -> a) -> a -> a
--                  ↑         ↑   ↑
--               función    input output
aplicarDosVeces f x = f (f x)

-- Uso: aplicarDosVeces (*2) 5 = 20
```

### **📝 Buenas Prácticas:**

#### **✅ Siempre escribe signaturas para funciones públicas:**

```haskell
-- ✅ Bien - función documentada
calcularImpuesto :: Double -> Double -> Double
calcularImpuesto precio porcentaje = precio * (porcentaje / 100)

-- ❌ Mal - ¿qué tipos acepta?
calcularImpuesto precio porcentaje = precio * (porcentaje / 100)
```

#### **✅ Usa nombres descriptivos en signaturas complejas:**

```haskell
-- ✅ Muy claro
procesarPedido :: ClienteId -> [Producto] -> Descuento -> PrecioFinal
procesarPedido clienteId productos descuento = ...

-- ❌ Confuso
procesarPedido :: Int -> [String] -> Double -> Double
procesarPedido x y z = ...
```

### **🧪 Experimentos en GHCi:**

```haskell
-- Ver signatura de funciones existentes
*Main> :type length
length :: Foldable t => t a -> Int

*Main> :type head
head :: [a] -> a

*Main> :type (+)
(+) :: Num a => a -> a -> a

-- Crear función sin signatura explícita
*Main> let misterio x y = x + y
*Main> :type misterio
misterio :: Num a => a -> a -> a  -- ¡Haskell infirió el tipo!
```

## Definición de Funciones

En Haskell, las funciones son **ciudadanos de primera clase** y la unidad fundamental de construcción. A diferencia de los lenguajes imperativos donde defines procedimientos que modifican estado, en Haskell defines **transformaciones** que toman datos y producen nuevos datos.

**🔍 ¿Qué significa "ciudadanos de primera clase"?**

Significa que las funciones en Haskell pueden:

- Ser asignadas a variables
- Pasarse como parámetros a otras funciones
- Retornarse como valores de otras funciones
- Almacenarse en estructuras de datos

### **🎯 Anatomía de una Función**

```haskell
-- Estructura básica:
-- 1. Signatura de tipos (opcional pero recomendada)
-- 2. Definición de la función

nombreFuncion :: TipoEntrada -> TipoSalida
nombreFuncion parametro = expresion
```

**🔍 Desglose:**

- **`nombreFuncion`**: Identificador único para la función
- **`::`**: Operador que separa el nombre del tipo (se lee "tiene tipo")
- **`TipoEntrada -> TipoSalida`**: Signatura que especifica qué recibe y qué retorna
- **`parametro`**: Variable que representa el valor de entrada
- **`=`**: No es asignación, es **definición** (dice "es igual a")
- **`expresion`**: Código que calcula el resultado

### **📝 Funciones Simples**

```haskell
-- Función que duplica un número
doble :: Int -> Int
doble x = x * 2

-- Función que saluda
saludar :: String -> String
saludar nombre = "Hola, " ++ nombre ++ "!"

-- Función que verifica si un número es positivo
esPositivo :: Int -> Bool
esPositivo n = n > 0

-- Función constante (siempre retorna lo mismo)
siempre42 :: Int
siempre42 = 42
```

**🔍 Explicación de cada ejemplo:**

1. **`doble x = x * 2`**: Toma un entero `x` y lo multiplica por 2
2. **`"Hola, " ++ nombre ++ "!"`**: El operador `++` concatena strings
3. **`n > 0`**: El operador `>` compara y retorna `True` o `False`
4. **`siempre42 = 42`**: Función sin parámetros (constante)

### **🔗 Funciones con Múltiples Parámetros**

**🔍 ¿Cómo funciona esto internamente?**

En Haskell, todas las funciones técnicamente toman **un solo parámetro**, pero gracias al **currying** podemos simular múltiples parámetros.

**¿Qué es "currying"?**
Currying es una técnica que convierte una función de múltiples parámetros en una cadena de funciones que toman un parámetro cada una.

```haskell
-- Función con dos parámetros
sumar :: Int -> Int -> Int
sumar x y = x + y

-- Lo que realmente pasa internamente:
-- sumar :: Int -> (Int -> Int)
-- sumar = \x -> (\y -> x + y)
```

**🧪 Ejemplos prácticos:**

```haskell
-- Función con dos parámetros
sumar :: Int -> Int -> Int
sumar x y = x + y

-- Función con tres parámetros
promedioTres :: Double -> Double -> Double -> Double
promedioTres x y z = (x + y + z) / 3

-- Función más compleja
calcularPrecioFinal :: Double -> Double -> Double -> Double
calcularPrecioFinal precio descuento impuesto =
    precio * (1 - descuento/100) * (1 + impuesto/100)
```

**🔍 Desglose de `calcularPrecioFinal`:**

- **`descuento/100`**: Convierte porcentaje a decimal (20% → 0.2)
- **`(1 - descuento/100)`**: Factor de descuento (20% descuento → 0.8)
- **`(1 + impuesto/100)`**: Factor de impuesto (15% impuesto → 1.15)
- **`precio * factor1 * factor2`**: Aplica ambos factores al precio

### **⚡ Currying y Aplicación Parcial**

**🔍 ¿Qué es "Aplicación Parcial"?**

Aplicación parcial significa **dar solo algunos de los parámetros** a una función, creando una **nueva función** que espera los parámetros restantes.

```haskell
-- Función original
multiplicar :: Int -> Int -> Int
multiplicar x y = x * y

-- Aplicación parcial - crear nuevas funciones
doble :: Int -> Int
doble = multiplicar 2  -- Fijamos x = 2, falta y

triple :: Int -> Int
triple = multiplicar 3  -- Fijamos x = 3, falta y

-- Uso
resultado1 = doble 5     -- multiplicar 2 5 = 10
resultado2 = triple 4    -- multiplicar 3 4 = 12
```

**🔍 ¿Qué es `map`?**

`map` es una función que **aplica una función a cada elemento de una lista**:

```haskell
-- map :: (a -> b) -> [a] -> [b]
-- map función lista = nuevaLista

-- También puedes hacer aplicación parcial inline
duplicarLista = map (multiplicar 2) [1,2,3,4]  -- [2,4,6,8]
--                   ↑
--          aplicación parcial de multiplicar

-- Paso a paso:
-- map (multiplicar 2) [1,2,3,4]
-- = [multiplicar 2 1, multiplicar 2 2, multiplicar 2 3, multiplicar 2 4]
-- = [2, 4, 6, 8]
```

### **🔄 Diferentes Formas de Definir Funciones**

#### **1. Definición Directa**

```haskell
-- La más simple y clara
cuadrado :: Int -> Int
cuadrado x = x * x
```

**🔍 Explicación:**

- **`cuadrado`**: Nombre de la función
- **`:: Int -> Int`**: Signatura (recibe Int, retorna Int)
- **`x`**: Parámetro de entrada
- **`x * x`**: Expresión que calcula el resultado

#### **2. Usando `where` (Variables Locales)**

**🔍 ¿Qué es `where`?**

`where` te permite definir **variables auxiliares** que solo existen dentro de esa función.

```haskell
-- Para cálculos complejos con variables intermedias
areaTriangulo :: Double -> Double -> Double -> Double
areaTriangulo a b c = sqrt (s * (s - a) * (s - b) * (s - c))
  where
    s = (a + b + c) / 2  -- Semi-perímetro (variable local)
```

**🔍 ¿Qué es `sqrt`?**
`sqrt` es la función **raíz cuadrada** integrada en Haskell.

**🔍 ¿Qué es la fórmula?**
Esta es la **fórmula de Herón** para calcular el área de un triángulo:

- `s` = semi-perímetro = (a + b + c) / 2
- área = √(s × (s-a) × (s-b) × (s-c))

**💡 ¿Por qué usar `where`?**

- Evita repetir el cálculo de `s`
- Hace el código más legible
- Las variables son privadas a la función

#### **3. Usando `let...in` (Expresiones Locales)**

**🔍 ¿Qué es `let...in`?**

`let...in` es otra forma de crear variables locales, pero funciona como una **expresión**:

- **`let`**: "Déjame definir estas variables"
- **`in`**: "Y úsalas en esta expresión"

```haskell
-- Para expresiones más concisas
volumenCilindro :: Double -> Double -> Double
volumenCilindro radio altura =
  let areaBase = pi * radio * radio
      volumen = areaBase * altura
  in volumen
```

**🔍 ¿Qué es `pi`?**
`pi` es la constante matemática π (3.14159...) integrada en Haskell.

**🆚 Diferencia entre `where` y `let...in`:**

```haskell
-- Con where (variables al final)
funcionWhere :: Int -> Int
funcionWhere x = resultado
  where
    resultado = x * 2 + 1

-- Con let...in (variables al principio)
funcionLet :: Int -> Int
funcionLet x =
  let resultado = x * 2 + 1
  in resultado
```

#### **4. Composición de Funciones**

**🔍 ¿Qué es "Composición de Funciones"?**

La **composición** significa **combinar funciones** para crear una nueva función. Es como conectar tuberías: la salida de una función se convierte en la entrada de la siguiente.

**🔧 El operador `(.)`**

```haskell
-- Usando el operador de composición (.)
duplicarYSumar1 :: Int -> Int
duplicarYSumar1 = (+1) . (*2)  -- Primero multiplica por 2, luego suma 1
```

**🔍 ¿Qué son `(+1)` y `(*2)`?**

Son **secciones de operador** - operadores convertidos en funciones:

```haskell
(+1)  -- Función que suma 1: \x -> x + 1
(*2)  -- Función que multiplica por 2: \x -> x * 2
(-3)  -- Función que resta 3: \x -> x - 3
```

**🔍 ¿Qué es `\x -> x + 1`?**

Es una **función lambda** (función anónima):

- **`\`**: Representa la letra griega λ (lambda)
- **`x`**: Parámetro de la función
- **`->`**: Separador entre parámetros y cuerpo
- **`x + 1`**: Cuerpo de la función

**🔄 Dirección de lectura de la composición:**

```haskell
-- Se lee de DERECHA a IZQUIERDA
(+1) . (*2)
-- 1. Primero aplica (*2): multiplica por 2
-- 2. Luego aplica (+1): suma 1

-- Para el valor 5:
-- (+1) . (*2) $ 5
-- = (+1) ((*2) 5)
-- = (+1) 10
-- = 11
```

**📖 Equivalencias (todas hacen lo mismo):**

```haskell
-- Composición elegante
duplicarYSumar1 = (+1) . (*2)

-- Composición explícita
duplicarYSumar1' = \x -> (+1) ((*2) x)

-- Forma tradicional
duplicarYSumar1'' x = (x * 2) + 1
```

### **🎭 Funciones de Orden Superior**

**🔍 ¿Qué significa "Orden Superior"?**

Una función de **orden superior** es una función que puede:

1. **Recibir otras funciones** como parámetros, O
2. **Retornar funciones** como resultado

```haskell
-- Función que aplica otra función dos veces
aplicarDosVeces :: (a -> a) -> a -> a
aplicarDosVeces f x = f (f x)

-- Función que combina dos funciones
combinar :: (a -> b) -> (b -> c) -> a -> c
combinar f g x = g (f x)
```

**🔍 Desglosando las signaturas:**

**`aplicarDosVeces :: (a -> a) -> a -> a`**

- **`(a -> a)`**: Una función que recibe tipo `a` y retorna tipo `a`
- **`-> a`**: Recibe un valor de tipo `a`
- **`-> a`**: Retorna un valor de tipo `a`

**`combinar :: (a -> b) -> (b -> c) -> a -> c`**

- **`(a -> b)`**: Primera función (de tipo a a tipo b)
- **`-> (b -> c)`**: Segunda función (de tipo b a tipo c)
- **`-> a -> c`**: Recibe tipo a, retorna tipo c

**🧪 Ejemplos paso a paso:**

```haskell
-- Uso práctico
resultado1 = aplicarDosVeces (*2) 5      -- (5*2)*2 = 20
resultado2 = aplicarDosVeces (+1) 10     -- (10+1)+1 = 12

-- Desglose de aplicarDosVeces (*2) 5:
-- 1. f = (*2), x = 5
-- 2. aplicarDosVeces f x = f (f x)
-- 3. = (*2) ((*2) 5)
-- 4. = (*2) 10
-- 5. = 20
```

### **🧪 Funciones Polimórficas**

**🔍 ¿Qué significa "Polimórfica"?**

Una función **polimórfica** puede trabajar con **cualquier tipo** de datos. Usa **variables de tipo** (como `a`, `b`, `c`) en lugar de tipos específicos.

```haskell
-- Función que retorna el primer elemento de una lista
primero :: [a] -> a
primero (x:_) = x

-- Función que intercambia elementos de una tupla
intercambiar :: (a, b) -> (b, a)
intercambiar (x, y) = (y, x)

-- Función identidad (retorna lo mismo que recibe)
identidad :: a -> a
identidad x = x
```

**🔍 ¿Qué es `(x:_)` en `primero (x:_) = x`?**

Esto es **pattern matching** (coincidencia de patrones):

- **`x`**: El primer elemento de la lista (cabeza)
- **`:`**: Constructor de lista que separa cabeza de cola
- **`_`**: "Wildcard" - significa "no me importa el resto"

```haskell
-- Para la lista [1,2,3,4]:
-- x = 1 (primer elemento)
-- _ = [2,3,4] (resto, que ignoramos)
```

**🧪 La misma función funciona con cualquier tipo:**

```haskell
-- Uso con diferentes tipos
numero = primero [1,2,3]        -- 1 :: Int
texto = primero ["a","b","c"]   -- "a" :: String
booleano = primero [True,False] -- True :: Bool
```

### **🔧 Operadores como Funciones**

**🔍 ¿Los operadores son funciones en Haskell?**

¡Sí! En Haskell, **todos los operadores son funciones** con sintaxis especial.

```haskell
-- Estos son equivalentes:
suma1 = 5 + 3               -- Notación infija (normal)
suma2 = (+) 5 3             -- Notación prefija (como función)

-- Puedes crear tus propios operadores
(|>) :: a -> (a -> b) -> b
x |> f = f x

-- Uso: pipeline de transformaciones
resultado = 5 |> (*2) |> (+1) |> show  -- "11"
```

**🔍 ¿Qué hace el operador `|>`?**

- Toma un valor `x` y una función `f`
- Aplica la función al valor: `f x`
- Permite escribir "pipelines" de izquierda a derecha

**📖 Leyendo el pipeline paso a paso:**

```haskell
5 |> (*2) |> (+1) |> show
-- 1. Empieza con 5
-- 2. 5 |> (*2): aplica (*2) a 5 = 10
-- 3. 10 |> (+1): aplica (+1) a 10 = 11
-- 4. 11 |> show: aplica show a 11 = "11"
```

**🔍 ¿Qué es `show`?**
`show` convierte cualquier valor a su representación en texto:

```haskell
show 42      -- "42"
show True    -- "True"
show [1,2,3] -- "[1,2,3]"
```

### **🎯 Buenas Prácticas**

#### **✅ Nombres Descriptivos**

```haskell
-- ✅ Bien - nombres claros que explican qué hace la función
calcularDescuento :: Double -> Double -> Double
esPasswordValida :: String -> Bool
convertirCelsiusAFahrenheit :: Double -> Double

-- ❌ Mal - nombres confusos sin contexto
calc :: Double -> Double -> Double
check :: String -> Bool
conv :: Double -> Double
```

#### **✅ Funciones Pequeñas y Enfocadas**

```haskell
-- ✅ Bien - una responsabilidad por función
calcularImpuesto :: Double -> Double
calcularImpuesto precio = precio * 0.15

calcularPrecioConImpuesto :: Double -> Double
calcularPrecioConImpuesto precio = precio + calcularImpuesto precio

-- ❌ Mal - función que hace demasiado
calcularTodo precio descuento impuesto envio = precio - descuento + impuesto + envio
```

#### **✅ Usar Guards para Lógica Condicional**

**🔍 ¿Qué son "Guards"?**

Guards son una forma de escribir **lógica condicional** más legible que `if-then-else`:

```haskell
-- ✅ Bien - guards claros
clasificarEdad :: Int -> String
clasificarEdad edad
  | edad < 13    = "Niño"
  | edad < 20    = "Adolescente"
  | edad < 65    = "Adulto"
  | otherwise    = "Adulto mayor"
```

**🔍 Estructura de guards:**

- **`|`**: Símbolo que introduce una condición (se lee "si")
- **`edad < 13`**: Condición booleana
- **`=`**: Si la condición es verdadera, retorna este valor
- **`otherwise`**: Caso por defecto (equivale a `True`)

### **🧪 Experimentos en GHCi**

```haskell
-- Definir funciones en el REPL
*Main> let cuadrado x = x * x
*Main> cuadrado 5
25

-- Ver tipos de funciones
*Main> :type cuadrado
cuadrado :: Num a => a -> a

-- Aplicación parcial
*Main> let multiplicarPor3 = (*) 3
*Main> multiplicarPor3 7
21

-- Composición de funciones
*Main> let f = (+1) . (*2)
*Main> f 5
11

-- Función de orden superior
*Main> let aplicarDosVeces f x = f (f x)
*Main> aplicarDosVeces (+1) 10
12
```

**🔍 ¿Qué significa `Num a => a -> a`?**

- **`Num a`**: Restricción de clase de tipos (constraint)
- **`=>`**: "Implica que" o "dado que"
- **`a -> a`**: Tipo de la función

Se lee: "Para cualquier tipo `a` que sea una instancia de `Num`, la función toma un `a` y retorna un `a`"

### **🎯 Conceptos Clave**

#### **🔒 Pureza**

**🔍 ¿Qué significa que una función sea "pura"?**

Una función pura:

- **Siempre** retorna el mismo resultado para las mismas entradas
- **No tiene efectos secundarios** (no modifica variables globales, no imprime, no lee archivos)
- Es **predecible** y **fácil de testear**

```haskell
-- ✅ Función pura
cuadrado :: Int -> Int
cuadrado x = x * x  -- Siempre da el mismo resultado

-- ❌ Función impura (en otros lenguajes)
-- int contador = 0;
-- int incrementar() { return ++contador; }  // Resultado cambia cada vez
```

#### **🧮 Currying Automático**

**🔍 ¿Por qué todas estas definiciones son equivalentes?**

```haskell
-- Estas tres definiciones son equivalentes:
sumar1 :: Int -> Int -> Int
sumar1 x y = x + y

sumar2 :: Int -> (Int -> Int)  -- Paréntesis explícitos
sumar2 x y = x + y

sumar3 = \x -> \y -> x + y     -- Funciones lambda explícitas
```

Porque en Haskell:

1. **`Int -> Int -> Int`** es azúcar sintáctico para **`Int -> (Int -> Int)`**
2. **Currying es automático** - todas las funciones toman un parámetro
3. **Las funciones lambda** muestran la estructura real

#### **🔗 Composición Natural**

**🔍 ¿Por qué preferir composición?**

```haskell
-- En vez de anidar llamadas (difícil de leer)...
resultado = funcion3 (funcion2 (funcion1 x))

-- Usa composición (más claro y reutilizable)
nuevaFuncion = funcion3 . funcion2 . funcion1
resultado = nuevaFuncion x
```

**Ventajas de la composición:**

- **Más legible**: Se lee de derecha a izquierda como matemáticas
- **Reutilizable**: Puedes usar `nuevaFuncion` en muchos lugares
- **Eficiente**: Haskell puede optimizar la composición
