# 🗺️ Roadmap: De Programación Imperativa a Funcional Pura

> **Objetivo**: Dominar Haskell y la programación funcional desde cero, con enfoque práctico y proyectos reales.

## 🎯 Prerequisitos Mentales

Antes de empezar, olvida temporalmente estos conceptos imperativos:

- ❌ Variables que cambian
- ❌ Loops (for, while)
- ❌ Mutación de estado
- ❌ Efectos secundarios "libres"

Abraza estos conceptos funcionales:

- ✅ Todo es una función
- ✅ Datos inmutables
- ✅ Composición > Herencia
- ✅ Declarativo > Imperativo

---

## 📚 NIVEL 1: Fundamentos (Semanas 1-2)

### 🔥 1.1 Primeros Pasos

- **Teoría Mínima**: ¿Qué es programación funcional?
- **Práctica**:
  - Configurar GHC + VSCode
  - Tu primer "Hello World" en Haskell
  - Usar GHCi como calculadora avanzada

**Proyecto**: Calculadora básica en REPL

### 🔧 1.2 Sintaxis Básica

- **Conceptos**:
  - Funciones puras
  - Definición de funciones
  - Signatura de tipos básicos (`Int`, `String`, `Bool`)
  - Comentarios y documentación

**Proyecto**: Biblioteca de funciones matemáticas simples

### 🎲 1.3 Tipos Básicos y Operadores

- **Conceptos**:
  - Números (`Int`, `Integer`, `Float`, `Double`)
  - Caracteres y Strings
  - Booleanos y operadores lógicos
  - Comparaciones y precedencia

**Proyecto**: Validador de datos (edad, email, contraseña)

---

## 🏗️ NIVEL 2: Estructuras Fundamentales (Semanas 3-4)

### 📋 2.1 Listas - Tu Nueva Obsesión

- **Conceptos**:
  - Listas como estructura fundamental
  - Construcción con `:` y `[]`
  - Pattern matching básico
  - Recursión simple

**Proyecto**: Implementar funciones básicas de lista (length, reverse, sum)

### 🎯 2.2 Pattern Matching Mastery

- **Conceptos**:
  - Coincidencia de patrones exhaustiva
  - Guardas (guards)
  - Casos base y recursivos
  - Wildcard patterns

**Proyecto**: Parser de comandos simples

### 🔄 2.3 Recursión como Pensamiento

- **Conceptos**:
  - Recursión vs iteración
  - Casos base críticos
  - Recursión de cola (tail recursion)
  - Divide y vencerás

**Proyecto**: Suite de algoritmos recursivos (fibonacci, factorial, quicksort)

---

## 🚀 NIVEL 3: Funciones de Orden Superior (Semanas 5-6)

### ⚡ 3.1 Map, Filter, Fold - La Trinidad

- **Conceptos**:
  - `map` para transformaciones
  - `filter` para selección
  - `foldl` y `foldr` para agregación
  - Composición de operaciones

**Proyecto**: Analizador de datos CSV (procesamiento de ventas)

### 🔗 3.2 Composición de Funciones

- **Conceptos**:
  - Operador de composición `(.)`
  - Point-free style
  - Currying y aplicación parcial
  - Funciones como ciudadanos de primera clase

**Proyecto**: Pipeline de transformación de texto (markdown → HTML básico)

### 🏭 3.3 Funciones Lambda y Aplicación Parcial

- **Conceptos**:
  - Funciones anónimas
  - Currying automático
  - Aplicación parcial práctica
  - Secciones de operadores

**Proyecto**: Configurador de funciones personalizadas

---

## 🏛️ NIVEL 4: Sistema de Tipos Intermedio (Semanas 7-8)

### 🏗️ 4.1 Tipos de Datos Algebraicos (ADTs)

- **Conceptos**:
  - `data` declarations
  - Sum types (OR lógico)
  - Product types (AND lógico)
  - Record syntax

**Proyecto**: Sistema de gestión de inventario con tipos seguros

### 🔍 4.2 Maybe y Either - Adiós NullPointerException

- **Conceptos**:
  - `Maybe` para valores opcionales
  - `Either` para manejo de errores
  - Pattern matching con tipos suma
  - Chainear operaciones seguras

**Proyecto**: Calculadora que maneja errores elegantemente

### 📦 4.3 Tipos Paramétricos y Polimorfismo

- **Conceptos**:
  - Generics de Haskell
  - Variables de tipo (`a`, `b`, `c`)
  - Funciones polimórficas
  - Constraints básicos

**Proyecto**: Implementar estructura de datos genérica (Stack, Queue)

---

## 🧩 NIVEL 5: Mónadas y Efectos (Semanas 9-11)

### 🎭 5.1 Entendiendo las Mónadas (Sin el Miedo)

- **Conceptos**:
  - ¿Qué problema resuelven las mónadas?
  - Patrón de chainear operaciones
  - `do` notation básica
  - Leyes de mónadas (intuitivamente)

**Proyecto**: Simulador de lanzamiento de dados (IO básico)

### 💾 5.2 IO - Interactuando con el Mundo Real

- **Conceptos**:
  - `IO` monad
  - `putStrLn`, `getLine`, `readFile`
  - `do` notation para secuencias
  - Separación pura/impura

**Proyecto**: Aplicación CLI para gestión de tareas (TODO list)

### 🔧 5.3 Otras Mónadas Útiles

- **Conceptos**:
  - `State` monad para estado mutable
  - `Reader` monad para configuración
  - `Writer` monad para logging
  - Transformadores básicos

**Proyecto**: Intérprete de un mini-lenguaje de programación

---

## 🏗️ NIVEL 6: Arquitectura de Aplicaciones (Semanas 12-14)

### 📁 6.1 Módulos y Organización

- **Conceptos**:
  - Sistema de módulos
  - Exports e imports
  - Namespaces
  - Cabal packages

**Proyecto**: Refactorizar proyectos anteriores en módulos

### 🧪 6.2 Testing en Haskell

- **Conceptos**:
  - HSpec para testing unitario
  - QuickCheck para property testing
  - Doctest para ejemplos ejecutables
  - Testing de funciones puras

**Proyecto**: Suite completa de tests para biblioteca matemática

### 📊 6.3 Parsing y Procesamiento de Datos

- **Conceptos**:
  - Parsec básico
  - JSON con Aeson
  - Expresiones regulares
  - Validación de datos

**Proyecto**: Parser de logs de servidor web con estadísticas

---

## 🌐 NIVEL 7: Desarrollo Web y APIs (Semanas 15-17)

### 🕸️ 7.1 Servant - APIs Type-Safe

- **Conceptos**:
  - Type-level programming básico
  - Definir APIs con tipos
  - Handlers automáticos
  - Documentación automática

**Proyecto**: API REST para gestión de biblioteca

### 🗄️ 7.2 Persistencia de Datos

- **Conceptos**:
  - Persistent ORM
  - Migraciones automáticas
  - Query type-safe
  - Conexión con PostgreSQL/SQLite

**Proyecto**: Backend completo con base de datos

### 🔐 7.3 Autenticación y Middleware

- **Conceptos**:
  - JWT tokens
  - Middleware de autenticación
  - CORS y headers
  - Rate limiting

**Proyecto**: API segura con autenticación completa

---

## 🚀 NIVEL 8: Optimización y Performance (Semanas 18-19)

### ⚡ 8.1 Evaluación Perezosa Avanzada

- **Conceptos**:
  - Lazy vs Strict evaluation
  - Space leaks y cómo evitarlos
  - `seq` y `deepseq`
  - Profiling básico

**Proyecto**: Optimizar algoritmos con problemas de memoria

### 🏎️ 8.2 Estructuras de Datos Eficientes

- **Conceptos**:
  - `Vector` vs listas
  - `Text` vs `String`
  - `Map` y `Set` eficientes
  - `ByteString` para datos binarios

**Proyecto**: Benchmarking de diferentes implementaciones

---

## 🏆 NIVEL 9: Proyecto Final (Semanas 20-22)

### 🎯 9.1 Aplicación Completa

Elegir uno de estos proyectos:

1. **🏪 E-commerce Backend**

   - API REST completa
   - Base de datos
   - Autenticación
   - Sistema de pagos mock

2. **📊 Analizador de Datos Financieros**

   - Parser de archivos CSV/JSON
   - Cálculos estadísticos
   - Visualización básica
   - Reports automatizados

3. **🎮 Juego de Estrategia**

   - Lógica de juego compleja
   - IA básica
   - Estado de juego inmutable
   - Interfaz terminal

4. **🤖 Chatbot/CLI Tool**
   - Parsing de comandos naturales
   - Integración con APIs externas
   - Sistema de plugins
   - Configuración persistente

---

## 📈 Cronograma Sugerido

| Semanas | Nivel            | Enfoque             | Tiempo/Día  |
| ------- | ---------------- | ------------------- | ----------- |
| 1-2     | Fundamentos      | Sintaxis + REPL     | 1-2 horas   |
| 3-4     | Estructuras      | Listas + Recursión  | 1-2 horas   |
| 5-6     | Funciones HO     | Map/Filter/Fold     | 1.5-2 horas |
| 7-8     | Tipos Intermedio | ADTs + Maybe/Either | 1.5-2 horas |
| 9-11    | Mónadas          | IO + State          | 2-2.5 horas |
| 12-14   | Arquitectura     | Módulos + Testing   | 2-2.5 horas |
| 15-17   | Web/APIs         | Servant + Databases | 2.5-3 horas |
| 18-19   | Performance      | Optimización        | 2-3 horas   |
| 20-22   | Proyecto Final   | Aplicación real     | 3-4 horas   |

## 🎯 Objetivos por Milestone

### 🏁 Milestone 1 (Semana 4): "Pensamiento Funcional"

- Puedes resolver problemas sin loops
- Entiendes recursión intuitivamente
- Pattern matching es natural

### 🏁 Milestone 2 (Semana 8): "Type Safety Master"

- Usas el sistema de tipos para prevenir bugs
- Manejas errores sin excepciones
- Diseñas APIs type-safe

### 🏁 Milestone 3 (Semana 14): "Arquitecto Funcional"

- Organizas código en módulos lógicos
- Escribes tests comprehensivos
- Parseas datos complejos

### 🏁 Milestone 4 (Semana 22): "Haskell Developer"

- Puedes construir aplicaciones reales
- Entiendes trade-offs de performance
- Contribuyes a proyectos open source

## 🛠️ Herramientas por Nivel

- **Niveles 1-2**: GHCi + VSCode
- **Niveles 3-4**: Stack + HLint
- **Niveles 5-6**: HSpec + QuickCheck
- **Niveles 7-8**: Servant + Persistent
- **Nivel 9**: Criterion + Profiling

## 📚 Recursos Complementarios

### 📖 Lecturas por Nivel

- **1-2**: Learn You a Haskell (caps 1-4)
- **3-4**: Learn You a Haskell (caps 5-8)
- **5-6**: Learn You a Haskell (caps 9-12)
- **7-9**: Real World Haskell + documentación oficial

### 🎥 Videos Recomendados

- Computerphile: Haskell series
- Erik Meijer: Functional Programming
- Simon Peyton Jones: Adventure with Types

---

> 💡 **Filosofía del Roadmap**: "Cada concepto se practica inmediatamente con un proyecto real. La teoría sin práctica es inútil, pero la práctica sin entender el 'por qué' es frágil."

**¡Empecemos el viaje hacia la programación funcional pura!**
