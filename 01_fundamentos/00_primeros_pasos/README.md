# Fundamentos del lenguaje Haskell

## ¿Qué es programación funcional?

En informática, la programación funcional es un paradigma de programación en donde los programas se componen de la aplicación y composición de funciones. A diferencia de la programación imperativa, donde se define una secuencia de instrucciones que cambian el estado del programa, en la programación funcional se enfatiza el uso de funciones puras y la inmutabilidad de los datos.

A grandes rasgos, el paradigma funcional consiste en que las funciones son ciudadanos de primer orden, esto quiere decir que, funciones pueden recibir como argumento otras funciones, las funciones pueden tener como valor de retorno otras funciones, y las funciones pueden ser asignadas a variables. Además, la programación funcional promueve el uso de funciones puras, que son aquellas que no tienen efectos secundarios y siempre devuelven el mismo resultado para los mismos argumentos.

## Hello World en Haskell

Para crear un proyecto en Haskell vamos a usar en esta ocasión **Stack**, el cual es un gestor de proyectos y dependencias para Haskell, similar a lo que es **npm** para JavaScript o **pip** para Python.

Para esto vamos a ejecutar los siguientes comandos en la terminal:

```bash
# Crear un nuevo proyecto
stack new hello-world
```

Esto nos creará una carpeta llamada `hello-world` con la siguiente estructura:

```text
C:.
|   .gitignore
|   CHANGELOG.md
|   hello-world.cabal
|   LICENSE
|   package.yaml
|   README.md
|   Setup.hs
|   stack.yaml
|
+---app
|       Main.hs
|
+---src
|       Lib.hs
|
\---test
        Spec.hs
```

El archivo `Main.hs` es el punto de entrada de nuestra aplicación, y es donde vamos a escribir nuestro primer programa en Haskell.

Contiene el siguiente código:

```haskell
module Main (main) where

import Lib

main :: IO ()
main = someFunc
```

### Disección del código

Analicemos línea por línea nuestro primer programa en Haskell:

```haskell
module Main (main) where
```

**🏗️ `module Main`**: Define un **módulo** llamado `Main`. Los módulos son como namespaces - organizan y agrupan funciones relacionadas. El módulo `Main` es especial porque es el punto de entrada de nuestra aplicación.

**📤 `(main)`**: Lista de **exportación** - especifica qué funciones de este módulo pueden ser usadas por otros módulos. Solo exportamos `main` porque es lo único que necesita el mundo exterior.

**🔗 `where`**: Palabra clave que introduce las definiciones del módulo. Todo lo que viene después pertenece a este módulo.

```haskell
import Lib
```

**📥 `import Lib`**: Importa todas las funciones públicas del módulo `Lib` (ubicado en `src/Lib.hs`). Es como `import` en Python o `require` en Node.js, pero más poderoso - puedes importar funciones específicas, renombrarlas, etc.

```haskell
main :: IO ()
```

**🎯 Signatura de tipo**: `main` es una función que:

- **`IO`**: Realiza operaciones de entrada/salida (input/output)
- **`()`**: No retorna un valor útil (equivalente a `void` en otros lenguajes)
- **`::`**: Se lee como "tiene tipo" - separa el nombre de la función de su tipo

```haskell
main = someFunc
```

**⚙️ Definición de función**: `main` simplemente llama a `someFunc` (definida en el módulo `Lib`). En Haskell, el signo `=` no es asignación - es **definición**. Estamos diciendo que `main` **es** `someFunc`.

### 🤔 Conceptos que Apuntan al Futuro

- **📦 Módulos**: Sistema de organización más poderoso que clases
- **🔍 Tipos explícitos**: Haskell puede inferir tipos, pero escribirlos es buena práctica
- **🌊 IO Monad**: Forma elegante de manejar efectos secundarios en un lenguaje puro
- **🎭 Funciones como valores**: `main = someFunc` trata funciones como datos

```haskell
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "Hello Haskell!"
```

### 🔍 Análisis del módulo Lib

```haskell
module Lib
    ( someFunc
    ) where
```

**📋 Exportación explícita**: A diferencia de `Main` que exportaba solo una función, aquí vemos la sintaxis **multilínea** para exportaciones. Los paréntesis pueden contener múltiples funciones separadas por comas - muy útil cuando tu módulo crece.

```haskell
someFunc :: IO ()
someFunc = putStrLn "someFunc"
```

**🖨️ `putStrLn`**: Función integrada de Haskell que:

- **`put`**: Poner/colocar
- **`Str`**: String (cadena de texto)
- **`Ln`**: Line (nueva línea al final)

Es el equivalente a `console.log()` en JavaScript o `print()` en Python, pero **type-safe**.

**🎯 Primera función real**: `someFunc` no solo llama a otra función - **es** la llamada a `putStrLn`. En Haskell, `putStrLn "someFunc"` es una **acción** que, cuando se ejecuta, imprime texto.

### 🚀 Conceptos Nuevos

- **🏭 Funciones de librería**: `putStrLn` viene de la librería estándar
- **📝 Acciones vs Valores**: `putStrLn "texto"` es una **acción IO**, no un string
- **🔗 Composición simple**: Un módulo puede ser tan simple como una función que llama a otra

### 🤔 ¿Por qué separar Main y Lib?

- **🎯 Main**: Punto de entrada, orquesta la aplicación
- **📚 Lib**: Lógica reutilizable, funciones puras
- **🧪 Separación**: Facilita testing y reutilización

## Ejecutando el programa

Para ejecutar nuestro programa, primero debemos asegurarnos de estar en la carpeta del proyecto `hello-world`. Luego, abrimos Visual Studio Code desde esa carpeta para trabajar cómodamente.

```bash
# Navegar a la carpeta del proyecto
cd hello-world
# Abrir Visual Studio Code desde aquí
code .
```

Una vez dentro de Visual Studio Code, abrimos la terminal integrada y ejecutamos los siguientes comandos para compilar y ejecutar nuestro programa:

```bash
# Desde dentro de hello-world/
stack build
stack exec hello-world-exe
```

Esto compilará nuestro proyecto y ejecutará el programa, mostrando el mensaje "Hello Haskell!" en la terminal.

### Sobre la estructura del proyecto

Stack genera una estructura de proyecto profesional que sigue las mejores prácticas de Haskell. Veamos qué hace cada archivo y carpeta:

#### 📄 Archivos de Configuración (Raíz)

**📋 `package.yaml`**: Archivo principal de configuración del proyecto

- Define metadatos (nombre, versión, autor)
- Lista dependencias del proyecto
- Especifica configuración de compilación
- **Es como `package.json` en Node.js**

**⚙️ `stack.yaml`**: Configuración específica de Stack

- Especifica qué versión del compilador GHC usar
- Define el "resolver" (conjunto de paquetes compatibles)
- Configuración de build local

**📦 `hello-world.cabal`**: Archivo generado automáticamente

- **NO editar manualmente** - Stack lo genera desde `package.yaml`
- Formato tradicional de Cabal
- Es lo que GHC realmente lee para compilar

#### 📚 Documentación y Metadatos

**📖 `README.md`**: Documentación del proyecto

- Descripción del proyecto
- Instrucciones de instalación y uso
- **Personalízalo** para describir tu aplicación

**📝 `CHANGELOG.md`**: Historial de cambios

- Documenta nuevas features, fixes, breaking changes
- Fundamental para librerías públicas

**⚖️ `LICENSE`**: Licencia del proyecto

- Por defecto usa BSD3 (muy permisiva)
- Define cómo otros pueden usar tu código

#### 🔧 Archivos de Build

**🔨 `Setup.hs`**: Script de configuración de Cabal

- Raramente necesitas editarlo
- Permite customización avanzada del proceso de build

**🚫 `.gitignore`**: Archivos que Git debe ignorar

- Excluye archivos compilados (`.hi`, `.o`)
- Excluye carpeta `dist/` y `.stack-work/`

#### 📁 Estructura de Código

**🚀 `app/`**: Código de la aplicación ejecutable

- **`Main.hs`**: Punto de entrada principal
- Solo contiene lógica de arranque
- Llama a funciones definidas en `src/`

**📚 `src/`**: Código de librerías reutilizables

- **`Lib.hs`**: Funciones principales del proyecto
- Lógica de negocio
- Código que puede ser importado por otros módulos

**🧪 `test/`**: Tests unitarios e integración

- **`Spec.hs`**: Tests usando HSpec
- Stack puede ejecutar con `stack test`
- Fundamental para código de calidad

#### 🎯 Flujo de Trabajo Típico

```
📝 Escribes lógica en src/
    ↓
🚀 La llamas desde app/Main.hs
    ↓
🧪 La testeas en test/
    ↓
📦 Stack compila todo junto
```

#### 💡 Ventajas de esta Estructura

- **🔄 Separación clara**: App vs Librería vs Tests
- **📦 Reutilización**: `src/` puede ser usado por otros proyectos
- **🧪 Testing**: Tests aislados de la aplicación
- **📚 Documentación**: Todo bien organizado y documentado

**¡Esta estructura escala desde "Hello World" hasta aplicaciones enterprise!** 🚀

## 🧮 Proyecto: Calculadora Básica

Ahora que entendemos la estructura básica, vamos a crear nuestro primer proyecto práctico: una calculadora que nos enseñará los fundamentos de Haskell.

### 🎯 Objetivos del Proyecto

- ✅ Escribir funciones puras
- ✅ Entender tipos básicos (`Int`, `Double`, `Bool`)
- ✅ Usar signatura de tipos
- ✅ Probar funciones en GHCi
- ✅ Manejar operaciones matemáticas

### 🚀 Setup del Proyecto

Vamos a trabajar directamente en nuestro proyecto `hello-world` modificando los archivos existentes:

```bash
# Asegúrate de estar en la carpeta del proyecto
cd hello-world

# Abrir en VSCode
code .
```

### 📝 Creando la Calculadora

Edita el archivo `src/Lib.hs`:

```haskell
-- src/Lib.hs
module Lib
    ( sumar
    , restar
    , multiplicar
    , dividir
    , potencia
    , areaCirculo
    , areaRectangulo
    , mostrarCalculadora
    ) where

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

-- ========================================
-- FUNCIONES GEOMÉTRICAS
-- ========================================

-- Área de un círculo
areaCirculo :: Double -> Double
areaCirculo radio = pi * radio * radio

-- Área de un rectángulo
areaRectangulo :: Double -> Double -> Double
areaRectangulo largo ancho = largo * ancho

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
    putStrLn ""
    putStrLn "Funciones geométricas:"
    putStrLn ("Área círculo (radio=5): " ++ show (areaCirculo 5))
    putStrLn ("Área rectángulo (4x6): " ++ show (areaRectangulo 4 6))
```

### 🎯 Actualizando Main.hs

Edita `app/Main.hs` para usar nuestra calculadora:

```haskell
-- app/Main.hs
module Main (main) where

import Lib

main :: IO ()
main = do
    putStrLn "¡Bienvenido a Haskell! 🎉"
    putStrLn ""
    mostrarCalculadora
```

### 🏃‍♂️ Ejecutando la Calculadora

```bash
# Compilar y ejecutar
stack build
stack exec hello-world-exe
```

**Salida esperada:**

```
¡Bienvenido a Haskell! 🎉

=== 🧮 CALCULADORA HASKELL ===

Operaciones básicas:
5 + 3 = 8.0
10 - 4 = 6.0
6 * 7 = 42.0
15 / 3 = 5.0
2 ^ 8 = 256.0

Funciones geométricas:
Área círculo (radio=5): 78.53981633974483
Área rectángulo (4x6): 24.0
```

### 🎮 Jugando en el REPL

La verdadera diversión está en el REPL. Abre GHCi en tu proyecto:

```bash
stack ghci
```

Ahora puedes usar tu calculadora interactivamente:

```haskell
-- Cargar tu módulo
*Main Lib> :load src/Lib.hs

-- Probar funciones
*Main Lib> sumar 15 25
40.0

*Main Lib> potencia 3 4
81.0

*Main Lib> areaCirculo 10
314.1592653589793

-- Ver el tipo de una función
*Main Lib> :type sumar
sumar :: Double -> Double -> Double

-- Aplicación parcial (¡magia funcional!)
*Main Lib> let sumar5 = sumar 5
*Main Lib> sumar5 10
15.0

-- Composición de funciones
*Main Lib> sumar (multiplicar 3 4) (potencia 2 3)
20.0
```

### 🧪 Experimentos en REPL

Prueba estos experimentos para entender mejor Haskell:

```haskell
-- 1. Aplicación parcial
*Main Lib> let doble = multiplicar 2
*Main Lib> doble 7
14.0

-- 2. Funciones como argumentos (adelanto del futuro)
*Main Lib> map (multiplicar 3) [1, 2, 3, 4]
[3.0,6.0,9.0,12.0]

-- 3. Inferencia de tipos
*Main Lib> :type 42
42 :: Num a => a

*Main Lib> :type 3.14
3.14 :: Fractional a => a

-- 4. Operadores como funciones
*Main Lib> (+) 5 3
8

*Main Lib> (*) 4
<interactive>:1:1: error: [GHC-83865]
    • No instance for (Show (Double -> Double))
```

### 📚 Conceptos Clave Aprendidos

#### 🎯 **Signatura de Tipos**

```haskell
sumar :: Double -> Double -> Double
--       ↑        ↑        ↑
--    entrada1  entrada2  salida
```

#### 🔄 **Funciones Puras**

- Misma entrada → Misma salida
- Sin efectos secundarios
- Fáciles de testear y razonar

#### 🧮 **Aplicación Parcial**

```haskell
sumar 5 3     -- Aplicación total: 8.0
sumar 5       -- Aplicación parcial: función que espera un número
```

#### 📦 **Exportación de Módulos**

- Solo exportamos lo que otros necesitan usar
- Encapsulación funcional

### 🎯 Ejercicios para Practicar

1. **Agregar más funciones matemáticas**:

   ```haskell
   raizCuadrada :: Double -> Double
   factorial :: Int -> Int
   ```

   > En Haskell, existe una diferencia entre `Int` e `Integer`. `Int` es un entero de tamaño fijo (32 o 64 bits), mientras que `Integer` es un entero de precisión arbitraria. Para cálculos matemáticos simples, `Int` es suficiente, pero para números muy grandes, usa `Integer`.

2. **Funciones de validación**:

   ```haskell
   esPar :: Int -> Bool
   esPositivo :: Double -> Bool
   ```

3. **Conversor de unidades**:
   ```haskell
   celsiusAFahrenheit :: Double -> Double
   metrosAPies :: Double -> Double
   ```

---

> 💡 En Haskell no hay variables que cambien, no hay loops, no hay mutación. Solo funciones puras que transforman datos. ¡Esto es programación funcional en acción!
