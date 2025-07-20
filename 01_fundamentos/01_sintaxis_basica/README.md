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

## Funciones Puras

Las **funciones puras** son el corazón de Haskell y uno de los conceptos más importantes de la programación funcional. Entender qué hace pura a una función te ayudará a escribir código más predecible, testeable y mantenible.

### **🔍 ¿Qué es una Función Pura?**

Una función es **pura** cuando cumple **dos requisitos fundamentales**:

1. **Determinista**: Siempre retorna el **mismo resultado** para las **mismas entradas**
2. **Sin efectos secundarios**: No modifica nada fuera de la función ni interactúa con el mundo exterior

```haskell
-- ✅ Función PURA
sumar :: Int -> Int -> Int
sumar x y = x + y

-- Siempre que llames sumar 3 5, obtienes 8
-- No modifica variables globales, no imprime, no lee archivos
```

### **🎯 Características de las Funciones Puras**

#### **✅ 1. Determinismo Total**

```haskell
-- ✅ PURA - Siempre el mismo resultado
cuadrado :: Int -> Int
cuadrado x = x * x

-- cuadrado 5 SIEMPRE será 25
-- cuadrado 3 SIEMPRE será 9

-- ✅ PURA - Resultado depende solo de las entradas
multiplicar :: Int -> Int -> Int
multiplicar a b = a * b

-- ✅ PURA - Usa solo los parámetros
esMayor :: Int -> Int -> Bool
esMayor x y = x > y
```

#### **❌ 2. Cero Efectos Secundarios**

```haskell
-- ❌ IMPURA (en otros lenguajes) - Modifica variable global
-- int contador = 0;
-- int incrementar() {
--     contador++;  // ¡Efecto secundario!
--     return contador;
-- }

-- ❌ IMPURA (en otros lenguajes) - Imprime en pantalla
-- int saludar(String nombre) {
--     System.out.println("Hola " + nombre);  // ¡Efecto secundario!
--     return nombre.length();
-- }

-- ❌ IMPURA (en otros lenguajes) - Lee del sistema
-- int obtenerTiempoActual() {
--     return System.currentTimeMillis();  // ¡Resultado cambia!
-- }
```

### **🧪 Ejemplos Prácticos: Puras vs Impuras**

#### **✅ Funciones Puras en Haskell**

```haskell
-- ✅ PURA - Solo calcula basado en entradas
calcularDescuento :: Double -> Double -> Double
calcularDescuento precio porcentaje = precio * (porcentaje / 100)

-- ✅ PURA - Solo transforma datos
procesarTexto :: String -> String
procesarTexto texto = map toUpper texto
-- map aplica toUpper a cada carácter

-- ✅ PURA - Solo evalúa condiciones
esPasswordSegura :: String -> Bool
esPasswordSegura password =
    length password >= 8 &&
    any isDigit password &&
    any isUpper password

-- ✅ PURA - Solo operaciones matemáticas
calcularAreaCirculo :: Double -> Double
calcularAreaCirculo radio = pi * radio * radio

-- ✅ PURA - Solo manipula listas
filtrarPares :: [Int] -> [Int]
filtrarPares numeros = filter even numeros
```

**🔍 Explicación de funciones usadas:**

- **`toUpper`**: Convierte un carácter a mayúscula
- **`any`**: Verifica si algún elemento de una lista cumple una condición
- **`isDigit`**: Verifica si un carácter es un dígito (0-9)
- **`isUpper`**: Verifica si un carácter es mayúscula
- **`filter`**: Filtra elementos de una lista que cumplen una condición
- **`even`**: Verifica si un número es par

#### **❌ Funciones Impuras (Conceptual)**

```haskell
-- ❌ IMPURA - Dependería del tiempo actual (si existiera)
-- obtenerEdad :: String -> IO Int  -- IO indica impureza
-- obtenerEdad fechaNacimiento = do
--     fechaActual <- obtenerFechaActual  -- ¡Efecto secundario!
--     return (calcularDiferencia fechaActual fechaNacimiento)

-- ❌ IMPURA - Imprimiría en pantalla (si existiera)
-- saludarYContar :: String -> IO Int
-- saludarYContar nombre = do
--     putStrLn ("Hola, " ++ nombre)  -- ¡Efecto secundario!
--     return (length nombre)

-- ❌ IMPURA - Leería de archivo (si existiera)
-- leerConfiguracion :: String -> IO String
-- leerConfiguracion archivo = do
--     contenido <- readFile archivo  -- ¡Efecto secundario!
--     return contenido
```

**🔍 ¿Qué es `IO`?**
`IO` es el tipo que Haskell usa para marcar funciones que **SÍ tienen efectos secundarios**. Es la forma de Haskell de separar claramente el mundo puro del impuro.

### **🔐 ¿Cómo Haskell Garantiza la Pureza?**

#### **🚫 1. Inmutabilidad Por Defecto**

```haskell
-- En Haskell, los valores son inmutables
x :: Int
x = 5
-- x = 10  -- ¡ERROR! No puedes cambiar x

-- Para "cambiar" algo, creates un nuevo valor
lista :: [Int]
lista = [1, 2, 3]

nuevaLista :: [Int]
nuevaLista = 0 : lista  -- [0,1,2,3] - lista original intacta
```

#### **🔍 2. Sistema de Tipos que Rastrea Efectos**

```haskell
-- Función pura - sin marcas especiales
pura :: Int -> Int -> Int
pura x y = x + y

-- Función impura - marcada con IO
impura :: String -> IO ()
impura mensaje = putStrLn mensaje

-- El compilador NUNCA permitirá esto:
-- intentoIncorrecto :: Int -> Int
-- intentoIncorrecto x = putStrLn "Hola"  -- ¡ERROR DE TIPO!
```

#### **⚡ 3. Evaluación Perezosa**

```haskell
-- Las expresiones no se evalúan hasta que se necesitan
listaInfinita :: [Int]
listaInfinita = [1..]  -- ¡Lista infinita!

primerosTres :: [Int]
primerosTres = take 3 listaInfinita  -- Solo evalúa [1,2,3]

-- Esto es seguro porque las funciones son puras
-- No hay efectos secundarios que ejecutar "accidentalmente"
```

### **🎯 Ventajas de las Funciones Puras**

#### **🧪 1. Testeo Extremadamente Simple**

```haskell
-- ✅ Función pura - fácil de testear
calcularImpuesto :: Double -> Double
calcularImpuesto precio = precio * 0.15

-- Test:
-- calcularImpuesto 100.0 == 15.0  ✓
-- calcularImpuesto 200.0 == 30.0  ✓
-- ¡Siempre funcionará igual!
```

Comparado con funciones impuras (en otros lenguajes):

```javascript
// ❌ Función impura - difícil de testear
function calcularImpuestoConLog(precio) {
  console.log("Calculando impuesto para: " + precio); // Efecto secundario
  const impuesto = precio * 0.15;
  console.log("Impuesto calculado: " + impuesto); // Efecto secundario
  return impuesto;
}

// Para testear necesitas:
// - Capturar la salida de console.log
// - Mockear console.log
// - Limpiar estado entre tests
```

#### **🧩 2. Composición Natural**

```haskell
-- Las funciones puras se componen perfectamente
calcularDescuento :: Double -> Double
calcularDescuento precio = precio * 0.1

aplicarDescuento :: Double -> Double
aplicarDescuento precio = precio - calcularDescuento precio

calcularImpuesto :: Double -> Double
calcularImpuesto precio = precio * 0.15

-- Composición limpia
precioFinal :: Double -> Double
precioFinal = (+) <$> aplicarDescuento <*> calcularImpuesto
-- El operador `<$>` y `(<*>)` son parte de la **aplicación de funciones en contexto** (Functor y Applicative)
-- Esto significa: "aplica aplicarDescuento y calcularImpuesto al mismo precio"
-- Es decir, hacen lo siguiente:
-- precioFinal precio = aplicarDescuento precio + calcularImpuesto precio

-- O más simple:
precioFinalSimple :: Double -> Double
precioFinalSimple precio =
    let conDescuento = aplicarDescuento precio
        impuesto = calcularImpuesto conDescuento
    in conDescuento + impuesto
```

#### **🔄 3. Paralelización Segura**

```haskell
-- ✅ Seguro paralelizar - no hay efectos secundarios
procesarNumeros :: [Int] -> [Int]
procesarNumeros = map (\x -> x * x + 1)

-- En un mundo ideal:
-- resultado = procesarNumeros [1..1000000] `usando` todasLasCPUs
-- ¡No hay race conditions porque no hay estado mutable!
```

#### **🧠 4. Razonamiento Local**

```haskell
-- Para entender esta función, solo necesitas ver SU código
calcularPromedio :: [Double] -> Double
calcularPromedio numeros = sum numeros / fromIntegral (length numeros)

-- No necesitas preocuparte por:
-- - Variables globales que podrían cambiar
-- - Archivos que se podrían modificar
-- - Estado de red
-- - Tiempo actual
-- ¡Solo las matemáticas!
```

### **🔧 Patrones Comunes con Funciones Puras**

#### **📊 1. Transformaciones de Datos**

```haskell
-- Transformar una lista de nombres a saludos
saludarTodos :: [String] -> [String]
saludarTodos nombres = map (\nombre -> "Hola, " ++ nombre ++ "!") nombres

-- Filtrar y transformar
procesarEdades :: [Int] -> [String]
procesarEdades edades =
    map (\edad -> show edad ++ " años")
    (filter (>= 18) edades)

-- Uso:
-- saludarTodos ["Ana", "Luis"] = ["Hola, Ana!", "Hola, Luis!"]
-- procesarEdades [15, 20, 17, 25] = ["20 años", "25 años"]
```

#### **🔢 2. Cálculos Complejos**

```haskell
-- Fibonacci puro (recursivo)
fibonacci :: Int -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- Factorial puro
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

-- Suma de cuadrados
sumaCuadrados :: [Int] -> Int
sumaCuadrados numeros = sum (map (^2) numeros)
```

**🔍 ¿Qué es `(^2)`?**
Es una **sección de operador** para elevar al cuadrado:

```haskell
(^2) 3  -- 3^2 = 9
(^2) 5  -- 5^2 = 25
```

#### **🎭 3. Validaciones y Verificaciones**

```haskell
-- Validar email (versión simplificada)
esEmailValido :: String -> Bool
esEmailValido email =
    '@' `elem` email &&
    '.' `elem` email &&
    length email > 5

-- Verificar si una lista está ordenada
estaOrdenada :: [Int] -> Bool
estaOrdenada [] = True
estaOrdenada [_] = True
estaOrdenada (x:y:resto) = x <= y && estaOrdenada (y:resto)

-- Verificar si todos los elementos cumplen una condición
todosSonPositivos :: [Int] -> Bool
todosSonPositivos = all (> 0)
```

**🔍 ¿Qué es `elem`?**
`elem` verifica si un elemento está en una lista:

```haskell
'a' `elem` "hola"     -- True
'z' `elem` "hola"     -- False
3 `elem` [1,2,3,4]    -- True
```

**🔍 ¿Qué es `all`?**
`all` verifica si todos los elementos de una lista cumplen una condición:

```haskell
all even [2,4,6,8]    -- True (todos son pares)
all even [2,4,5,8]    -- False (5 no es par)
all (> 0) [1,2,3]     -- True (todos son positivos)
```

### **⚖️ Funciones Puras vs Mundo Real**

#### **🤔 "¿Pero necesito hacer cosas reales como leer archivos y imprimir!"**

¡Exacto! Por eso Haskell tiene el sistema `IO` que **separa claramente** las funciones puras de las impuras:

```haskell
-- ✅ Función pura - hace el cálculo
procesarDatos :: String -> [String]
procesarDatos contenido =
    filter (not . null)
    (map (take 10)
    (lines contenido))

-- ❌ Función impura - interactúa con el mundo
procesarArchivo :: FilePath -> IO [String]
procesarArchivo archivo = do
    contenido <- readFile archivo  -- Efecto secundario
    return (procesarDatos contenido)  -- Usa la función pura
```

**🎯 La filosofía de Haskell:**

1. **Máximo código en funciones puras** (fácil de testear y razonar)
2. **Mínimo código en funciones IO** (solo lo necesario para interactuar con el mundo)
3. **Separación clara** entre ambos mundos

### **🧪 Ejercicios para Practicar**

#### **Básicos:**

```haskell
-- 1. Función que duplica todos los números de una lista
duplicarTodos :: [Int] -> [Int]
duplicarTodos = ?

-- 2. Función que cuenta cuántos números son pares
contarPares :: [Int] -> Int
contarPares = ?

-- 3. Función que convierte grados Celsius a Fahrenheit
celsiusAFahrenheit :: Double -> Double
celsiusAFahrenheit = ?
```

#### **Intermedios:**

````haskell
-- 4. Función que encuentra el máximo de una lista
maximo :: [Int] -> Int
maximo = foldr1 max
-- foldr1 es una función de Haskell que aplica una función binaria (como max) a los elementos de una lista, reduciéndola a un solo valor.
-- En términos simples, toma una lista y devuelve el elemento más grande.

-- 5. Función que revierte una lista sin usar reverse
revertir :: [a] -> [a]
revertir = foldl (flip (:)) []
-- foldl es una función que aplica una función binaria a cada elemento de una lista, acumulando el resultado.
-- flip toma una función y cambia el orden de sus argumentos, así que (flip (:)) agrega elementos al principio de la lista acumulada.
-- (:) es el constructor de listas que agrega un elemento al inicio de una lista.
-- [] es la lista vacía.

-- 6. Función que calcula el promedio de una lista
promedio :: [Double] -> Double
promedio xs = suma / cuenta
  where
    suma = sum xs
    cuenta = fromIntegral (length xs)
-- fromIntegral convierte un entero a un número de punto flotante
-- length cuenta el número de elementos en la lista
-- sum suma todos los elementos de la lista
```

### **🎯 Conceptos Clave**

#### **🔑 Función Pura = Función Matemática**

En matemáticas, una función **siempre** da el mismo resultado:

- f(x) = x²
- f(3) = 9 (siempre)
- f(5) = 25 (siempre)

En Haskell, las funciones son **exactamente igual**:

```haskell
f :: Int -> Int
f x = x * x

-- f 3 = 9 (siempre)
-- f 5 = 25 (siempre)
````

#### **🛡️ Pureza = Seguridad**

- **Sin sorpresas**: La función hace exactamente lo que dice su signatura
- **Sin dependencias ocultas**: Solo depende de sus parámetros
- **Sin efectos ocultos**: Solo retorna un valor, nada más

#### **🧠 Pureza = Simplicidad Mental**

```haskell
-- Para entender qué hace esta función:
calcularTotal :: [Double] -> Double -> Double
calcularTotal precios descuento =
    (sum precios) * (1 - descuento/100)

-- Solo necesitas leer ESTA función
-- No necesitas revisar:
-- - Variables globales
-- - Estado de la aplicación
-- - Archivos de configuración
-- - Base de datos
-- ¡Solo matemáticas!
```

### **🚀 Siguiente Paso**

Con las funciones puras dominadas, el próximo paso es aprender sobre **comentarios y documentación** para hacer tu código aún más claro y mantenible.

**¡Ya entiendes el corazón de la programación funcional!** 🎉 Las funciones puras son la base sobre la que se construye todo lo demás en Haskell.

### **📝 Comentarios y Documentación en Haskell**

La documentación es fundamental para escribir código mantenible y comprensible. Haskell ofrece varias formas de documentar tu código, desde comentarios simples hasta documentación formal que se puede generar automáticamente.

### **💬 Tipos de Comentarios**

#### **1. Comentarios de Una Línea**

```haskell
-- Este es un comentario de una línea
edad :: Int
edad = 25  -- También puedes comentar al final de una línea

-- Los comentarios explican QUÉ hace el código y POR QUÉ
calcularDescuento :: Double -> Double -> Double
calcularDescuento precio porcentaje = precio * (porcentaje / 100)
-- Convierte porcentaje a decimal y calcula el descuento
```

**🔍 Explicación:**

- **`--`**: Inicia un comentario de línea
- Todo lo que sigue después de `--` es ignorado por el compilador
- Puedes usar comentarios al final de líneas de código

#### **2. Comentarios de Múltiples Líneas**

```haskell
{-
Este es un comentario
de múltiples líneas.
Puedes escribir párrafos completos aquí.
Útil para explicaciones largas.
-}

calcularAreaTriangulo :: Double -> Double -> Double -> Double
calcularAreaTriangulo a b c = sqrt (s * (s - a) * (s - b) * (s - c))
  where
    s = (a + b + c) / 2
{-
Esta función usa la fórmula de Herón para calcular
el área de un triángulo dados sus tres lados.
Primero calcula el semi-perímetro (s) y luego
aplica la fórmula: área = √(s(s-a)(s-b)(s-c))
-}
```

**🔍 Explicación:**

- **`{-`**: Inicia un comentario de bloque
- **`-}`**: Termina un comentario de bloque
- Puede abarcar múltiples líneas
- Los comentarios de bloque pueden anidarse (comentarios dentro de comentarios)

#### **3. Comentarios Anidados**

```haskell
{-
Este es el comentario principal
{- Este es un comentario anidado
   que explica algo específico -}
Y aquí continúa el comentario principal
-}

fibonacci :: Int -> Integer
fibonacci n = fib n
  where
    fib 0 = 0
    fib 1 = 1
    fib k = fib (k-1) + fib (k-2)
{-
Implementación recursiva de Fibonacci
{- Nota: Esta implementación es ineficiente
   para números grandes debido a la
   recalculación repetida -}
Casos base: fib(0) = 0, fib(1) = 1
Caso recursivo: fib(n) = fib(n-1) + fib(n-2)
-}
```

### **📚 Documentación con Haddock**

**🔍 ¿Qué es Haddock?**

Haddock es la herramienta estándar de Haskell para **generar documentación automática** a partir de comentarios especiales en el código. Es como Javadoc para Java o JSDoc para JavaScript.

#### **📖 Documentación de Módulos**

```haskell
{-|
Módulo      : Matematicas.Basicas
Descripción : Funciones matemáticas básicas para cálculos comunes
Copyright   : (c) Tu Nombre, 2024
Licencia    : MIT
Mantenedor  : tu.email@ejemplo.com
Estabilidad : experimental

Este módulo proporciona funciones matemáticas básicas
incluyendo operaciones geométricas y estadísticas simples.

== Ejemplo de uso:

>>> calcularAreaCirculo 5.0
78.53981633974483

>>> promedio [1, 2, 3, 4, 5]
3.0
-}

module Matematicas.Basicas
  ( calcularAreaCirculo
  , promedio
  , factorial
  ) where
```

**🔍 Explicación de la sintaxis:**

- **`{-|`**: Inicia un comentario de documentación de módulo
- **`-}`**: Termina el comentario de documentación
- **`>>>`**: Marca ejemplos de código que se pueden probar automáticamente
- Las secciones como `== Ejemplo de uso:` crean encabezados en la documentación

#### **📝 Documentación de Funciones**

```haskell
-- | Calcula el área de un círculo dado su radio.
--
-- Esta función toma el radio de un círculo y retorna su área
-- usando la fórmula: área = π × radio²
--
-- >>> calcularAreaCirculo 5.0
-- 78.53981633974483
--
-- >>> calcularAreaCirculo 0
-- 0.0
--
-- /Nota:/ Para radios negativos, la función retorna un valor positivo
-- ya que el área siempre es positiva.
calcularAreaCirculo :: Double -> Double
calcularAreaCirculo radio = pi * radio * radio

-- | Calcula el promedio aritmético de una lista de números.
--
-- == Precondiciones:
-- * La lista no debe estar vacía
--
-- == Postcondiciones:
-- * El resultado está entre el mínimo y máximo de la lista
--
-- === Ejemplos:
--
-- >>> promedio [1, 2, 3, 4, 5]
-- 3.0
--
-- >>> promedio [10.5, 20.3, 15.7]
-- 15.5
--
-- __Advertencia:__ Esta función fallará con una lista vacía.
promedio :: [Double] -> Double
promedio [] = error "No se puede calcular el promedio de una lista vacía"
promedio xs = sum xs / fromIntegral (length xs)
```

**🔍 Explicación de la sintaxis:**

- **`-- |`**: Inicia documentación de función (equivale a `{-|` pero para una línea)
- **`== Sección:`**: Crea encabezados de sección
- **`=== Subsección:`**: Crea subsecciones
- **`/cursiva/`**: Texto en cursiva
- **`__negrita__`**: Texto en negrita
- **`>>> código`**: Ejemplos ejecutables

#### **📊 Documentación de Tipos de Datos**

```haskell
-- | Representa la información básica de una persona.
--
-- Este tipo de dato encapsula los atributos fundamentales
-- que identifican a una persona en el sistema.
data Persona = Persona
  { -- | El nombre completo de la persona
    nombre :: String

  , -- | La edad en años (debe ser >= 0)
    edad :: Int

  , -- | Indica si la persona es mayor de edad
    --
    -- * 'True' si edad >= 18
    -- * 'False' si edad < 18
    esMayorDeEdad :: Bool

  , -- | Lista de habilidades o competencias
    --
    -- Ejemplo: @["Haskell", "JavaScript", "Matemáticas"]@
    habilidades :: [String]
  } deriving (Show, Eq)

-- | Crea una nueva persona con validación básica.
--
-- Esta función construye un objeto 'Persona' y automáticamente
-- determina si es mayor de edad basándose en la edad proporcionada.
--
-- == Parámetros:
-- * @nombreCompleto@ - El nombre de la persona (no debe estar vacío)
-- * @edadAnios@ - La edad en años (debe ser >= 0)
-- * @listaHabilidades@ - Lista de habilidades (puede estar vacía)
--
-- == Ejemplos:
--
-- >>> crearPersona "Ana García" 25 ["Python", "SQL"]
-- Persona {nombre = "Ana García", edad = 25, esMayorDeEdad = True, habilidades = ["Python","SQL"]}
--
-- >>> crearPersona "Luis Pérez" 16 ["Fútbol"]
-- Persona {nombre = "Luis Pérez", edad = 16, esMayorDeEdad = False, habilidades = ["Fútbol"]}
crearPersona :: String -> Int -> [String] -> Persona
crearPersona nombreCompleto edadAnios listaHabilidades
  | null nombreCompleto = error "El nombre no puede estar vacío"
  | edadAnios < 0 = error "La edad no puede ser negativa"
  | otherwise = Persona
      { nombre = nombreCompleto
      , edad = edadAnios
      , esMayorDeEdad = edadAnios >= 18
      , habilidades = listaHabilidades
      }
```

**🔍 ¿Qué es `deriving (Show, Eq)`?**

- **`deriving`**: Palabra clave que le dice a Haskell que genere automáticamente implementaciones de ciertas funciones
- **`Show`**: Permite convertir el tipo a String (para imprimirlo)
- **`Eq`**: Permite comparar dos valores del tipo por igualdad (con `==` y `/=`)

**🔍 ¿Qué es `@código@`?**

- Es una forma de marcar código dentro de la documentación
- Se renderiza con una fuente monoespaciada en la documentación generada

### **🎨 Estilos de Documentación**

#### **📋 Documentación Concisa**

```haskell
-- | Duplica un número.
doble :: Int -> Int
doble x = x * 2

-- | Verifica si un número es par.
esPar :: Int -> Bool
esPar n = n `mod` 2 == 0
```

**🔍 ¿Qué es `mod`?**

- **`mod`**: Operador que calcula el resto de una división
- **`n `mod` 2`**: Notación infija - equivale a `mod n 2`
- Si `n mod 2 == 0`, entonces `n` es par

#### **📖 Documentación Detallada**

```haskell
-- | Encuentra el elemento máximo en una lista no vacía.
--
-- Esta función recorre toda la lista comparando elementos
-- y retorna el valor más grande encontrado.
--
-- == Complejidad:
-- * Tiempo: O(n) donde n es el tamaño de la lista
-- * Espacio: O(1) - usa espacio constante
--
-- == Comportamiento:
--
-- * Para listas de un elemento, retorna ese elemento
-- * Para listas vacías, lanza una excepción
-- * Para listas con elementos iguales, retorna cualquiera de ellos
--
-- === Ejemplos típicos:
--
-- >>> maximo [1, 5, 3, 9, 2]
-- 9
--
-- >>> maximo [-1, -5, -3]
-- -1
--
-- >>> maximo [42]
-- 42
--
-- === Casos especiales:
--
-- >>> maximo []
-- *** Exception: Prelude.maximum: empty list
maximo :: [Int] -> Int
maximo [] = error "No se puede encontrar el máximo de una lista vacía"
maximo xs = maximum xs
```

**🔍 ¿Qué es `maximum`?**

- **`maximum`**: Función predefinida de Haskell que encuentra el elemento máximo en una lista
- Equivale a `foldr1 max` - aplica la función `max` entre todos los elementos

### **🧪 Pruebas en la Documentación (Doctests)**

#### **🔬 ¿Qué son los Doctests?**

Los **doctests** son ejemplos de código en la documentación que se pueden **ejecutar automáticamente** como pruebas. Es una forma de asegurar que tu documentación esté siempre actualizada.

```haskell
-- | Calcula el factorial de un número.
--
-- El factorial de n (escrito n!) es el producto de todos
-- los números enteros positivos menores o iguales a n.
--
-- === Definición matemática:
-- * 0! = 1 (por definición)
-- * n! = n × (n-1)! para n > 0
--
-- >>> factorial 0
-- 1
--
-- >>> factorial 1
-- 1
--
-- >>> factorial 5
-- 120
--
-- >>> factorial 10
-- 3628800
--
-- Para números negativos, se lanza un error:
--
-- >>> factorial (-1)
-- *** Exception: El factorial no está definido para números negativos
factorial :: Integer -> Integer
factorial n
  | n < 0 = error "El factorial no está definido para números negativos"
  | n == 0 = 1
  | otherwise = n * factorial (n - 1)
```

#### **🧪 Ejecutar Doctests**

Para ejecutar los doctests, necesitas instalar la herramienta `doctest`:

```bash
# Instalar doctest
cabal install doctest

# Ejecutar doctests en un archivo
doctest MiModulo.hs

# Ejemplo de salida:
# Examples: 4  Tried: 4  Errors: 0  Failures: 0
```

### **🎯 Buenas Prácticas de Documentación**

#### **✅ 1. Documenta la Intención, No Solo la Implementación**

```haskell
-- ❌ Mal - solo describe QUÉ hace el código
-- | Multiplica x por 2 y suma 1.
procesarNumero :: Int -> Int
procesarNumero x = x * 2 + 1

-- ✅ Bien - explica POR QUÉ y el propósito
-- | Convierte un índice de base-0 a base-1.
--
-- En muchos contextos matemáticos y de usuario final,
-- los índices empiezan en 1 en lugar de 0.
-- Esta función realiza esa conversión.
--
-- >>> convertirABase1 0
-- 1
-- >>> convertirABase1 5
-- 11
convertirABase1 :: Int -> Int
convertirABase1 x = x * 2 + 1
```

#### **✅ 2. Incluye Ejemplos Prácticos**

```haskell
-- | Filtra elementos de una lista basándose en una condición.
--
-- === Casos de uso comunes:
--
-- Filtrar números pares:
-- >>> filtrarPares [1,2,3,4,5,6]
-- [2,4,6]
--
-- Filtrar strings no vacíos:
-- >>> filter (not . null) ["", "hola", "", "mundo"]
-- ["hola","mundo"]
--
-- Filtrar elementos mayores que un valor:
-- >>> filter (> 5) [1,3,7,2,9,4]
-- [7,9]
filtrarPares :: [Int] -> [Int]
filtrarPares = filter even
```

**🔍 ¿Qué es `not . null`?**

- Es **composición de funciones**: `not . null`
- **`null`**: Verifica si una lista está vacía
- **`not`**: Invierte un valor booleano
- **`not . null`**: Verifica si una lista NO está vacía

#### **✅ 3. Documenta Precondiciones y Postcondiciones**

```haskell
-- | Calcula la raíz cuadrada usando el método de Newton.
--
-- == Precondiciones:
-- * El número debe ser >= 0
-- * Se requiere precisión > 0
--
-- == Postcondiciones:
-- * El resultado r satisface: |r² - n| < precisión
-- * El resultado es >= 0
--
-- == Complejidad:
-- * Tiempo: O(log(precisión)) iteraciones
-- * Espacio: O(1)
--
-- >>> abs (raizCuadrada 25.0 0.001 ** 2 - 25.0) < 0.001
-- True
--
-- >>> raizCuadrada 0.0 0.1
-- 0.0
raizCuadrada :: Double -> Double -> Double
raizCuadrada numero precision
  | numero < 0 = error "No se puede calcular la raíz cuadrada de un número negativo"
  | precision <= 0 = error "La precisión debe ser mayor que cero"
  | numero == 0 = 0
  | otherwise = newtonMethod numero precision
  where
    newtonMethod n prec =
      let aproximacion = n / 2
          iteracion x = (x + n / x) / 2
          esLoPreciso x = abs (x * x - n) < prec
      in until esLoPreciso iteracion aproximacion
```

**🔍 ¿Qué es `until`?**

- **`until`**: Función que repite una operación hasta que se cumple una condición
- **`until condicion funcion valorInicial`**: Aplica `funcion` repetidamente hasta que `condicion` sea `True`

#### **✅ 4. Usa Jerarquía de Encabezados**

```haskell
-- | Funciones para manipular listas de números.
--
-- = Operaciones Básicas
--
-- Estas funciones realizan operaciones simples sobre listas.
--
-- == Estadísticas
--
-- === Medidas de tendencia central
--
-- Funciones para calcular promedio, mediana, etc.
--
-- === Medidas de dispersión
--
-- Funciones para calcular varianza, desviación estándar, etc.
--
-- = Operaciones Avanzadas
--
-- Funciones más complejas que requieren algoritmos especializados.

-- | Calcula el promedio aritmético.
promedio :: [Double] -> Double
promedio xs = sum xs / fromIntegral (length xs)

-- | Calcula la mediana de una lista ordenada.
mediana :: [Double] -> Double
mediana [] = error "No se puede calcular la mediana de una lista vacía"
mediana xs =
  let ordenados = sort xs
      n = length ordenados
      medio = n `div` 2
  in if even n
     then (ordenados !! (medio - 1) + ordenados !! medio) / 2
     else ordenados !! medio
```

**🔍 ¿Qué es `div`?**

- **`div`**: División entera (descarta la parte decimal)
- **`n `div` 2`**: Notación infija - equivale a `div n 2`

**🔍 ¿Qué es `!!`?**

- **`!!`**: Operador de indexación para listas
- **`lista !! indice`**: Obtiene el elemento en la posición `indice`
- **¡Cuidado!**: Los índices empiezan en 0

**🔍 ¿Qué es `sort`?**

- **`sort`**: Función que ordena una lista de menor a mayor
- Requiere `import Data.List` para usarla

### **🛠️ Herramientas de Documentación**

#### **📚 Generando Documentación con Haddock**

```bash
# Generar documentación HTML
haddock --html --hyperlinked-source MiModulo.hs

# Generar documentación para todo un proyecto
cabal haddock

# Generar documentación con enlaces a código fuente
stack haddock --haddock-arguments --hyperlinked-source
```

#### **🔍 Verificando Documentación**

```bash
# Verificar que todos los exports estén documentados
haddock --hoogle MiModulo.hs

# Ejecutar doctests
doctest MiModulo.hs

# Verificar estilo de documentación
hlint MiModulo.hs
```

### **🎯 Comentarios vs Documentación: ¿Cuándo Usar Cada Uno?**

#### **💬 Usa Comentarios Simples Para:**

```haskell
-- Explicaciones de algoritmos complejos
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) =
  -- Partición: menores a la izquierda, mayores a la derecha
  let menores = quicksort [y | y <- xs, y <= x]
      mayores = quicksort [y | y <- xs, y > x]
  -- Concatenar: menores + pivote + mayores
  in menores ++ [x] ++ mayores

-- TODOs y notas temporales
procesarDatos :: [String] -> [String]
procesarDatos datos =
  -- TODO: Optimizar este filtro - muy lento para listas grandes
  filter (not . null) datos
```

#### **📚 Usa Documentación Haddock Para:**

```haskell
-- API pública que otros usarán
-- | Ordena una lista usando el algoritmo quicksort.
--
-- Este algoritmo tiene complejidad promedio O(n log n)
-- y complejidad peor caso O(n²).
--
-- >>> quicksort [3,1,4,1,5]
-- [1,1,3,4,5]
quicksort :: [Int] -> [Int]

-- Funciones exportadas del módulo
-- | Filtra elementos vacíos de una lista de strings.
--
-- Útil para limpiar datos de entrada que pueden
-- contener strings vacíos no deseados.
--
-- >>> filtrarVacios ["", "hola", "", "mundo", ""]
-- ["hola","mundo"]
filtrarVacios :: [String] -> [String]
```

### **🚀 Próximo Paso**

¡Excelente! Has completado toda la **Sintaxis Básica de Haskell**. Ahora conoces:

- ✅ Tipos de datos básicos
- ✅ Signatura de tipos
- ✅ Definición de funciones
- ✅ Funciones puras
- ✅ Comentarios y documentación

**El siguiente paso es:** **1.3 Funciones y Operadores** donde aprenderás sobre:

- Operadores aritméticos y lógicos
- Precedencia de operadores
- Funciones predefinidas
- Operadores personalizados

**¡Ya tienes las bases sólidas para continuar tu viaje en Haskell!** 🎉
