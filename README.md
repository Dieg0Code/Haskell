# Haskell: Programación Funcional Moderna 🚀

[![Haskell](https://img.shields.io/badge/Haskell-5e5086?style=for-the-badge&logo=haskell&logoColor=white)](https://www.haskell.org/)
[![GHC](https://img.shields.io/badge/GHC-9.4+-blue?style=for-the-badge)](https://www.haskell.org/ghc/)
[![Cabal](https://img.shields.io/badge/Cabal-3.8+-green?style=for-the-badge)](https://www.haskell.org/cabal/)
[![Stack](https://img.shields.io/badge/Stack-2.13+-orange?style=for-the-badge)](https://docs.haskellstack.org/)

## 📖 Acerca de Haskell

Haskell es un **lenguaje de programación funcional puro** con evaluación perezosa (lazy evaluation) y un sistema de tipos estático extremadamente potente. Nombrado en honor al matemático Haskell Curry, este lenguaje representa uno de los paradigmas más elegantes y expresivos en el mundo de la programación.

### 🎯 Características Principales

- **🔒 Pureza Funcional**: Sin efectos secundarios, inmutabilidad por defecto
- **⚡ Evaluación Perezosa**: Computación bajo demanda para optimización automática
- **🛡️ Sistema de Tipos Avanzado**: Hindley-Milner con inferencia automática
- **🧮 Composición de Funciones**: Paradigma de "construir programas combinando funciones"
- **📦 Gestión de Efectos**: Mónadas para manejar I/O, estado y otros efectos de forma controlada

## 🛠️ Herramientas del Ecosistema Moderno

### Gestores de Proyectos y Dependencias

| Herramienta | Descripción                                      | Equivalente              |
| ----------- | ------------------------------------------------ | ------------------------ |
| **Cabal**   | Gestor oficial de paquetes y construcción        | `npm`, `cargo`           |
| **Stack**   | Herramienta de construcción con reproducibilidad | `yarn`, `pipenv`         |
| **Hackage** | Repositorio central de paquetes                  | `npmjs.com`, `crates.io` |

### Entorno de Desarrollo

- **GHC (Glasgow Haskell Compiler)**: Compilador principal con optimizaciones avanzadas
- **GHCi**: REPL interactivo para prototipado rápido
- **Haskell Language Server (HLS)**: LSP para IDEs modernos con autocompletado, refactoring y diagnósticos
- **Hoogle**: Motor de búsqueda por tipos y funciones
- **HLint**: Linter para sugerencias de mejora de código

## 🚀 Comandos Esenciales

### Gestión de Proyectos

```bash
# Inicializar proyectos
cabal init --interactive
stack new mi-proyecto

# Gestión de dependencias
cabal build
cabal run
cabal test
cabal repl

# Con Stack
stack build
stack exec mi-proyecto
stack test
stack ghci
```

### REPL y Desarrollo

```bash
# Iniciar REPL
ghci
cabal repl
stack ghci

# Comandos útiles en GHCi
:load archivo.hs     # Cargar archivo
:reload              # Recargar archivos
:type expresion      # Obtener tipo
:info funcion        # Información de función
:browse Modulo       # Explorar módulo
```

## 💡 Sintaxis Fundamental

### Declaración de Funciones

```haskell
-- Función simple con signatura de tipo
cuadrado :: Int -> Int
cuadrado x = x * x

-- Función con múltiples parámetros
suma :: Int -> Int -> Int
suma x y = x + y

-- Función de orden superior
aplicarDosVeces :: (a -> a) -> a -> a
aplicarDosVeces f x = f (f x)

-- Composición de funciones
(.) :: (b -> c) -> (a -> b) -> a -> c
resultado = (cuadrado . suma 3) 5  -- (5 + 3)²
```

### Tipos de Datos Algebraicos

```haskell
-- Tipo suma (union type)
data Color = Rojo | Verde | Azul | RGB Int Int Int
  deriving (Show, Eq)

-- Tipo producto con parámetros
data Persona = Persona
  { nombre :: String
  , edad :: Int
  , email :: String
  } deriving (Show, Eq)

-- Tipo recursivo
data Lista a = Vacia | Cons a (Lista a)
  deriving (Show, Eq)

-- Tipo paramétrico (Maybe es el equivalente a Optional/Nullable)
data Maybe a = Nothing | Just a
```

### Pattern Matching y Guardas

```haskell
-- Pattern matching exhaustivo
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Con guardas (guards)
clasificarEdad :: Int -> String
clasificarEdad edad
  | edad < 13    = "Niño"
  | edad < 20    = "Adolescente"
  | edad < 60    = "Adulto"
  | otherwise    = "Adulto mayor"

-- Pattern matching con tipos algebraicos
procesarColor :: Color -> String
procesarColor Rojo = "Color primario: rojo"
procesarColor Verde = "Color primario: verde"
procesarColor Azul = "Color primario: azul"
procesarColor (RGB r g b) = "RGB(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"
```

### Listas y Programación de Alto Nivel

```haskell
-- Listas y rangos
numeros = [1, 2, 3, 4, 5]
infinitos = [1..]
pares = [2, 4..20]

-- List comprehensions
cuadrados = [x^2 | x <- [1..10]]
filtrados = [x | x <- [1..20], even x, x > 10]
combinaciones = [(x, y) | x <- [1..3], y <- ['a'..'c']]

-- Funciones de orden superior
duplicados = map (*2) [1..5]              -- [2,4,6,8,10]
filtrados' = filter even [1..10]          -- [2,4,6,8,10]
suma = foldl (+) 0 [1..10]                -- 55
producto = foldr (*) 1 [1..5]             -- 120

-- Operadores de lista
cabeza = head [1,2,3]                     -- 1
cola = tail [1,2,3]                       -- [2,3]
concatenacion = [1,2] ++ [3,4]            -- [1,2,3,4]
elemento = 3 `elem` [1,2,3,4]             -- True
```

### Mónadas y Gestión de Efectos

```haskell
-- Maybe para manejar valores nulos
dividir :: Float -> Float -> Maybe Float
dividir _ 0 = Nothing
dividir x y = Just (x / y)

-- IO para efectos de entrada/salida
main :: IO ()
main = do
  putStrLn "¿Cómo te llamas?"
  nombre <- getLine
  putStrLn ("Hola, " ++ nombre ++ "!")

-- Either para manejo de errores
data Error = DivisionPorCero | NumeroNegativo

calcularRaiz :: Float -> Either Error Float
calcularRaiz x
  | x < 0     = Left NumeroNegativo
  | otherwise = Right (sqrt x)
```

## 🎯 Conceptos Clave del Paradigma Funcional

### ⚡ Evaluación Perezosa (Lazy Evaluation)

```haskell
-- Solo se evalúa cuando se necesita
infinitos = [1..]
primerosCinco = take 5 infinitos  -- [1,2,3,4,5]

-- Permite estructuras de datos infinitas
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)
```

### 🔄 Inmutabilidad y Persistencia

```haskell
-- Las estructuras de datos son inmutables por defecto
lista1 = [1, 2, 3]
lista2 = 0 : lista1  -- [0, 1, 2, 3] (lista1 no cambia)

-- Modificaciones crean nuevas estructuras
import qualified Data.Map as Map
mapa1 = Map.fromList [("a", 1), ("b", 2)]
mapa2 = Map.insert "c" 3 mapa1  -- mapa1 permanece intacto
```

### 🧩 Composición y Currying

```haskell
-- Todas las funciones están currificadas
sumar :: Int -> Int -> Int -> Int
sumar x y z = x + y + z

sumar3 = sumar 3      -- Int -> Int -> Int
sumar3y5 = sumar3 5   -- Int -> Int
resultado = sumar3y5 7 -- 15

-- Composición elegante
procesamiento = map ((*2) . (+1)) [1..5]  -- [4,6,8,10,12]
```

## 📚 Librerías Esenciales del Ecosistema

### 🏗️ Fundacionales

- **base**: Librería estándar de Haskell
- **containers**: Map, Set, IntMap, etc.
- **text**: Manejo eficiente de texto Unicode
- **bytestring**: Manejo eficiente de datos binarios

### 🧪 Testing y Calidad

- **hspec**: Framework de testing BDD
- **QuickCheck**: Property-based testing
- **tasty**: Framework de testing unificado
- **hlint**: Linter y sugerencias de código

### 🌐 Desarrollo Web y APIs

- **servant**: APIs type-safe con generación automática de documentación
- **wai/warp**: Abstracción de aplicaciones web
- **aeson**: Parsing y generación JSON
- **persistent**: ORM type-safe

### ⚡ Performance y Concurrencia

- **async**: Programación asíncrona
- **stm**: Software Transactional Memory
- **vector**: Arrays eficientes
- **parallel**: Paralelización automática

## 🎯 Objetivos de Aprendizaje

- [ ] 🏗️ Dominar sintaxis y semántica funcional
- [ ] 🔧 Comprender el sistema de tipos Hindley-Milner
- [ ] ⚡ Aplicar evaluación perezosa efectivamente
- [ ] 🧩 Trabajar con mónadas, functores y applicatives
- [ ] 🚀 Desarrollar aplicaciones reales con Servant/Yesod
- [ ] 🔍 Optimización, profiling y análisis de rendimiento
- [ ] 🌐 Integración con ecosistemas externos (FFI, C bindings)

---

> 💡 **Filosofía Haskell**: "Si compila, probablemente funcione correctamente". El sistema de tipos actúa como un asistente de pruebas que elimina entire clases de bugs en tiempo de compilación.

**¡Bienvenido al mundo de la programación funcional pura!**
