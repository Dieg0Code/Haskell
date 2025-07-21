# Listas en Haskell

En Haskell, las listas son una de las estructuras de datos más fundamentales y versátiles. Permiten almacenar colecciones de elementos del mismo tipo y son ampliamente utilizadas en la programación funcional.

## Listas como estructura fundamental

### **🌟 ¿Por qué las Listas son Tan Importantes?**

En Haskell, las **listas** no son solo otra estructura de datos - son **el corazón** del lenguaje. Si vienes de programación imperativa donde usabas arrays, loops y mutación, prepárate para un cambio completo de mentalidad.

#### **🔍 La Filosofía de las Listas en Haskell**

```haskell
-- En otros lenguajes, trabajas con arrays:
-- int[] numeros = {1, 2, 3, 4, 5};
-- for(int i = 0; i < numeros.length; i++) { ... }

-- En Haskell, trabajas con listas:
numeros :: [Int]
numeros = [1, 2, 3, 4, 5]

-- Y en lugar de loops, usas recursión y funciones
procesar :: [Int] -> [Int]
procesar [] = []
procesar (x:xs) = (x * 2) : procesar xs
```

**🔍 ¿Qué significa la sintaxis `[Int]`?**

- **`[`** y **`]`**: Indican que es una **lista**
- **`Int`**: El **tipo** de elementos que contiene la lista
- **`[Int]`**: Se lee como "lista de enteros"
- **Generalización**: `[a]` significa "lista de elementos del tipo `a`"

**🔍 ¿Qué significa `:: [Int]`?**

- **`::`**: Operador de **anotación de tipo** (type annotation)
- Se lee como: "tiene el tipo"
- **`numeros :: [Int]`**: "numeros tiene el tipo lista de enteros"
- **Opcional**: Haskell puede inferir tipos, pero es buena práctica escribirlos

**🔍 ¿Qué significa `(x:xs)` en el código?**

- **`x`**: Variable que captura el **primer elemento** (head)
- **`:`**: Operador **constructor de lista** (lo veremos en detalle después)
- **`xs`**: Variable que captura el **resto de la lista** (tail)
- **`(x:xs)`**: Patrón que significa "lista no vacía con head x y tail xs"
- **Convención**: `xs` se pronuncia "equis-es" (plural de x)

Las listas son **inmutables**, **homogéneas**, **dinámicas**, **lazy** y **recursivas por naturaleza**. Estas características las hacen únicas y poderosas en el paradigma funcional.

**🔍 ¿Qué hace diferente a las listas de Haskell?**

1. **Son inmutables**: Una vez creada, nunca cambia
2. **Son homogéneas**: Todos los elementos del mismo tipo
3. **Son dinámicas**: Pueden crecer sin límite predefinido
4. **Son lazy**: Se evalúan solo cuando se necesitan
5. **Son recursivas**: Su definición es recursiva por naturaleza

**🔍 ¿Qué significa "inmutables"?**

- **Inmutable**: No puede cambiar después de crearse
- **NO puedes** modificar elementos existentes
- **NO puedes** agregar/quitar elementos de la lista original
- **SÍ puedes** crear **nuevas listas** basadas en las existentes

```haskell
lista_original = [1, 2, 3]
-- ❌ Esto no existe en Haskell:
-- lista_original[0] = 99  -- No puedes "modificar"

-- ✅ En su lugar, creas una nueva lista:
lista_nueva = 99 : [2, 3]  -- [99, 2, 3]
-- lista_original sigue siendo [1, 2, 3]
```

**🔍 ¿Qué significa "homogéneas"?**

- **Homogéneo**: Todos los elementos son del **mismo tipo**
- **Una lista de enteros** solo puede contener enteros
- **Una lista de strings** solo puede contener strings
- **NO puedes** mezclar tipos diferentes en la misma lista

```haskell
-- ✅ Homogéneas - OK:
enteros = [1, 2, 3, 4]        -- Todos Int
decimales = [1.1, 2.2, 3.3]  -- Todos Double
letras = ['a', 'b', 'c']      -- Todos Char

-- ❌ Heterogéneas - ERROR:
-- mixta = [1, 'a', True]  -- ¡No compila! Int, Char, Bool mezclados
```

**🔍 ¿Qué significa "dinámicas"?**

- **Dinámico**: El **tamaño** puede cambiar durante la ejecución
- **NO necesitas** declarar el tamaño de antemano
- **Pueden crecer** sin límite (hasta que se agote la memoria)
- **Diferentes** de arrays de tamaño fijo

```haskell
-- Empiezas con una lista pequeña:
pequeña = [1, 2]

-- Puedes crear listas más grandes:
grande = 0 : pequeña  -- [0, 1, 2]
muy_grande = [-1] ++ grande  -- [-1, 0, 1, 2]

-- Sin límite predefinido:
infinita = [1..]  -- [1, 2, 3, 4, 5, 6, 7, 8, 9, ...]
```

**🔍 ¿Qué es `++` en el ejemplo anterior?**

- **`++`**: Operador de **concatenación** de listas
- **`lista1 ++ lista2`**: Une dos listas en una nueva lista
- **`[1, 2] ++ [3, 4] = [1, 2, 3, 4]`**
- Lo veremos en detalle en la siguiente sección

**🔍 ¿Qué significa "lazy" (evaluación perezosa)?**

- **Lazy evaluation**: Los valores se calculan **solo cuando se necesitan**
- **NO calcula** toda la lista inmediatamente
- **Calcula** elementos bajo demanda
- **Permite** listas infinitas sin problemas

```haskell
-- Esta lista infinita no cuelga el programa:
naturales = [1..]  -- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, ...]

-- Solo calcula lo que realmente usas:
primeros_cinco = take 5 naturales  -- [1, 2, 3, 4, 5]
-- Solo calculó 5 números, no infinitos
```

**🔍 ¿Qué es `take` en el ejemplo?**

- **`take`**: Función que toma los primeros n elementos de una lista
- **`take :: Int -> [a] -> [a]`**: Su tipo
- **`take 3 [1, 2, 3, 4, 5] = [1, 2, 3]`**
- **Esencial** para trabajar con listas infinitas

**🔍 ¿Qué significa "recursivas por naturaleza"?**

- **Recursivo**: Definido en términos de sí mismo
- **Una lista** es o bien **vacía** o bien **un elemento seguido de otra lista**
- Esta **estructura recursiva** hace natural usar recursión para procesarlas

#### **🏗️ Anatomía de una Lista**

```haskell
-- Una lista en Haskell tiene solo DOS posibilidades:
-- 1. Está vacía: []
-- 2. Tiene un head (primer elemento) y un tail (resto de la lista)

-- Ejemplos visuales:
[]                    -- Lista vacía
[5]                   -- head=5, tail=[]
[3, 5]               -- head=3, tail=[5]
[1, 3, 5]            -- head=1, tail=[3,5]
[1, 3, 5, 7, 9]      -- head=1, tail=[3,5,7,9]
```

**🔍 ¿Qué es "head" (cabeza)?**

- **Head**: El **primer elemento** de una lista no vacía
- **`head [1, 2, 3] = 1`**
- **`head []`**: ❌ ERROR - lista vacía no tiene head
- **No es** un operador, es un **concepto** y también una **función**

**🔍 ¿Qué es "tail" (cola)?**

- **Tail**: **Todos los elementos excepto el primero**
- **`tail [1, 2, 3] = [2, 3]`**
- **`tail [5] = []`** (lista con solo el tail vacío)
- **`tail []`**: ❌ ERROR - lista vacía no tiene tail

**🔍 ¿Por qué solo DOS posibilidades?**

- **Simplicidad**: Estructura mínima pero completa
- **Recursión natural**: Fácil de procesar recursivamente
- **Elegancia matemática**: Basado en teoría de tipos algebraicos

**🔍 ¿Qué significa que son "recursivas por naturaleza"?**

- Una lista **se define en términos de sí misma**
- **Lista** = **Vacía** OR **(Elemento + Lista más pequeña)**
- Esta definición recursiva hace que la **recursión sea natural** para procesarlas

```haskell
-- Definición conceptual de una lista:
data List a = Empty | Element a (List a)

-- En notación de Haskell:
-- []     representa Empty
-- (x:xs) representa Element x xs
```

**🔍 ¿Qué significa `data List a = Empty | Element a (List a)`?**

- **`data`**: Palabra clave para definir **nuevos tipos de datos**
- **`List a`**: Nombre del tipo, donde `a` es **variable de tipo**
- **`=`**: "Se define como"
- **`Empty`**: **Constructor** para lista vacía
- **`|`**: "OR" - indica alternativas
- **`Element a (List a)`**: **Constructor** que toma un elemento tipo `a` y otra lista
- **Recursivo**: `List a` se define usando `List a`

**🔍 ¿Qué son los "constructores"?**

- **Constructor**: Función especial que **crea** valores de un tipo
- **`Empty`**: Crea una lista vacía
- **`Element`**: Crea una lista con un elemento y otra lista
- **Similar** a constructores en programación orientada a objetos

**🔍 ¿Qué es una "variable de tipo"?**

- **Variable de tipo**: Placeholder para cualquier tipo específico
- **`a`** puede ser `Int`, `String`, `Bool`, etc.
- **`List Int`**: Lista de enteros
- **`List String`**: Lista de strings
- **Polimorfismo**: El mismo tipo funciona para diferentes tipos concretos

#### **📊 Diferentes Tipos de Listas**

```haskell
-- Listas de números enteros
enteros :: [Int]
enteros = [1, 2, 3, 4, 5]

-- Listas de números decimales
decimales :: [Double]
decimales = [1.5, 2.7, 3.14159]

-- Listas de caracteres (esto ES un String!)
caracteres :: [Char]
caracteres = ['H', 'o', 'l', 'a']

-- String es solo azúcar sintáctico para [Char]
mensaje :: String
mensaje = "Hola"  -- Exactamente igual que ['H','o','l','a']

-- Listas de strings
palabras :: [String]
palabras = ["Hola", "mundo", "funcional"]

-- Listas de valores booleanos
booleanos :: [Bool]
booleanos = [True, False, True, True]

-- Listas anidadas (listas de listas)
matriz :: [[Int]]
matriz = [[1, 2], [3, 4], [5, 6]]
```

**🔍 ¿Qué es `Int` vs `Double`?**

- **`Int`**: Números **enteros** con rango limitado (-2³¹ a 2³¹-1 típicamente)
- **`Double`**: Números **decimales** de doble precisión (como float64)
- **Diferentes tipos**: No puedes mezclarlos en la misma lista sin conversión

**🔍 ¿Qué es `Char`?**

- **`Char`**: Un **único carácter** Unicode
- **`'a'`**: Literal de carácter (comillas simples)
- **`"a"`**: String de un carácter (comillas dobles) = `['a']`

**🔍 ¿Qué es "azúcar sintáctico"?**

- **Syntactic sugar**: Notación **más fácil de escribir/leer**
- **Mismo significado**: `"Hola"` y `['H','o','l','a']` son **idénticos**
- **Conveniencia**: El compilador traduce automáticamente

**🔍 ¿Por qué `[Char]` es lo mismo que `String`?**

```haskell
-- En Haskell, String es solo un type alias:
type String = [Char]

-- Estas dos declaraciones son idénticas:
saludo1 :: String
saludo1 = "Hola"

saludo2 :: [Char]
saludo2 = ['H', 'o', 'l', 'a']

-- Puedes verificarlo:
-- *Main> saludo1 == saludo2
-- True
```

**🔍 ¿Qué es `type String = [Char]`?**

- **`type`**: Palabra clave para crear **alias de tipos**
- **`String`**: Nuevo nombre
- **`= [Char]`**: Es exactamente lo mismo que `[Char]`
- **Alias**: Solo un nombre diferente, **no un tipo nuevo**

**🔍 ¿Qué significa `[[Int]]`?**

- **`[Int]`**: Lista de enteros
- **`[[Int]]`**: Lista de **listas de enteros**
- **Matriz**: Estructura bidimensional
- **`[[1, 2], [3, 4]]`**: 2 filas, 2 columnas cada una

#### **⚡ Características Fundamentales**

**🔒 Inmutabilidad**

```haskell
lista_original = [1, 2, 3]

-- En lenguajes imperativos harías:
-- lista_original[0] = 99;  // Modifica la lista original

-- En Haskell, no puedes "modificar" - solo crear nuevas:
lista_nueva = 99 : [2, 3]  -- [99, 2, 3]
-- lista_original sigue siendo [1, 2, 3]

-- ¡No hay forma de cambiar lista_original!
```

**🔍 ¿Qué es `99 : [2, 3]` en el ejemplo?**

- **`:`**: Operador **cons** (constructor de lista)
- **`99`**: Elemento que se agrega al frente
- **`[2, 3]`**: Lista existente
- **Resultado**: Nueva lista `[99, 2, 3]`
- **Eficiente**: O(1) - tiempo constante

**🔄 Evaluación Perezosa (Lazy Evaluation)**

```haskell
-- Puedes crear listas infinitas sin problema:
naturales = [1..]  -- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, ...]

-- ¿Por qué no se cuelga el programa?
-- ¡Porque solo calcula lo que necesitas!
primeros_cinco = take 5 naturales  -- [1, 2, 3, 4, 5]
```

**🔍 ¿Qué es `[1..]`?**

- **Range syntax**: Notación para **rangos**
- **`[1..]`**: Desde 1 hasta infinito
- **`[1..10]`**: Desde 1 hasta 10
- **`[1,3..10]`**: 1, 3, 5, 7, 9 (incremento de 2)

**🔍 ¿Cómo funciona la evaluación perezosa?**

- **Thunk**: Haskell crea una **"promesa"** de calcular el valor
- **Demand-driven**: Solo calcula cuando **realmente necesitas** el valor
- **Memoria eficiente**: No almacena infinitos valores
- **Tiempo eficiente**: No calcula valores que nunca usas

```haskell
-- Esto no calcula 1 millón de números inmediatamente:
grandes = [1..1000000]

-- Solo calcula los primeros 3 cuando los necesitas:
tres_primeros = take 3 grandes  -- Calcula solo [1, 2, 3]
```

**🎯 Homogeneidad de Tipos**

```haskell
-- ✅ Todos del mismo tipo - OK:
numeros_enteros = [1, 2, 3, 4]           -- [Int]
numeros_decimales = [1.1, 2.2, 3.3]     -- [Double]
letras = ['a', 'b', 'c']                 -- [Char]

-- ❌ Tipos mixtos - ERROR:
-- mixto = [1, 'a', True]  -- ¡No compila!
-- No puedes mezclar Int, Char y Bool en la misma lista
```

**🔍 ¿Por qué esta restricción?**

- **Seguridad de tipos**: El compilador puede **verificar** que tu código es correcto
- **Optimización**: El compilador sabe exactamente cómo almacenar los datos
- **Claridad**: Sabes qué tipo de datos estás procesando
- **Previene errores**: No puedes sumar un número con un carácter por accidente

#### **🌍 Listas vs Otras Estructuras**

**📋 Listas vs Arrays (otros lenguajes)**

```haskell
-- Arrays tradicionales:
-- - Tamaño fijo
-- - Acceso aleatorio O(1)
-- - Mutables
-- - Basados en índices

-- Listas de Haskell:
-- - Tamaño dinámico
-- - Acceso secuencial O(n)
-- - Inmutables
-- - Basadas en estructura recursiva

-- Ejemplo de acceso:
-- Array: array[3]     // Directo, O(1)
-- Lista: lista !! 3   // Secuencial, O(n)
```

**🔍 ¿Qué significa "O(1)" y "O(n)"?**

- **Big O notation**: Describe **eficiencia** de algoritmos
- **O(1)**: **Tiempo constante** - siempre toma el mismo tiempo
- **O(n)**: **Tiempo lineal** - tiempo crece proporcionalmente al tamaño
- **Array[3]**: Salto directo a posición 3
- **Lista!!3**: Debe recorrer elementos 0, 1, 2 para llegar a 3

**🔍 ¿Qué es `!!` en `lista !! 3`?**

- **`!!`**: Operador de **indexado** en listas
- **`lista !! n`**: Obtiene el elemento en la posición n (comenzando desde 0)
- **`[10, 20, 30] !! 1 = 20`**
- **Ineficiente**: O(n) porque debe recorrer la lista

**🔗 Listas vs Tuplas**

```haskell
-- Lista: elementos del mismo tipo, tamaño variable
numeros = [1, 2, 3, 4, 5]     -- Puede crecer
mas_numeros = 6 : numeros     -- [6, 1, 2, 3, 4, 5]

-- Tupla: elementos de cualquier tipo, tamaño fijo
persona = ("Juan", 25, True)  -- (String, Int, Bool)
-- No puedes agregar más elementos a una tupla
```

**🔍 ¿Qué son las tuplas?**

- **Tupla**: Contenedor de **elementos de tipos potencialmente diferentes**
- **Tamaño fijo**: No puede cambiar después de crearla
- **`(a, b, c)`**: Tupla de 3 elementos (triple)
- **`(String, Int, Bool)`**: Tipos específicos de cada posición

**🔍 ¿Cuándo usar Lista vs Tupla?**

- **Lista**: Cuando tienes **colección homogénea** de tamaño variable
- **Tupla**: Cuando tienes **datos heterogéneos** de estructura fija
- **Ejemplo lista**: Calificaciones de estudiantes `[85, 92, 78]`
- **Ejemplo tupla**: Datos de estudiante `("Ana", 20, True)`

#### **🎯 ¿Cuándo Usar Listas?**

**✅ Perfectas para:**

- **Secuencias** de datos del mismo tipo
- **Procesamiento secuencial** (uno tras otro)
- **Colecciones** que pueden **crecer/decrecer**
- **Datos** que procesas con **recursión**
- **Streams** de información

```haskell
-- Ejemplos ideales:
calificaciones = [85, 92, 78, 96, 88]
palabras_archivo = ["Hola", "mundo", "de", "Haskell"]
coordenadas = [(0, 0), (1, 2), (3, 4), (5, 6)]
```

**🔍 ¿Qué es un "stream"?**

- **Stream**: Flujo continuo de datos
- **Ejemplo**: Datos llegando de una red, archivo, sensor
- **Listas lazy**: Perfectas para representar streams infinitos
- **Procesamiento**: Un elemento a la vez conforme llegan

**❌ No ideales para:**

- **Acceso aleatorio** frecuente por índice
- **Modificaciones** en posiciones específicas
- **Búsquedas rápidas** (mejor usar Map o Set)
- **Datos** con **estructura fija** conocida

**🔍 ¿Qué son Map y Set?**

- **Map**: Estructura clave-valor para búsquedas rápidas
- **Set**: Colección sin duplicados para membresía rápida
- **Estructuras especializadas**: Optimizadas para casos específicos
- **Las veremos** en módulos más avanzados

#### **🧪 Primeros Experimentos**

```haskell
-- Carga GHCi y prueba estos comandos:

-- Crear listas simples:
-- *Main> [1, 2, 3, 4, 5]
-- [1,2,3,4,5]

-- Lista vacía:
-- *Main> []
-- []

-- Lista de caracteres:
-- *Main> ['a', 'b', 'c']
-- "abc"

-- ¿Son iguales String y [Char]?
-- *Main> "hola" == ['h', 'o', 'l', 'a']
-- True

-- Lista infinita (no te preocupes, no se cuelga):
-- *Main> take 10 [1..]
-- [1,2,3,4,5,6,7,8,9,10]

-- Tipos de las listas:
-- *Main> :type [1, 2, 3]
-- [1, 2, 3] :: Num a => [a]

-- *Main> :type ["hola", "mundo"]
-- ["hola", "mundo"] :: [String]
```

**🔍 ¿Qué es `*Main>`?**

- **Prompt de GHCi**: Indica que estás en el **intérprete interactivo**
- **`*Main`**: Módulo actual (Main es el módulo por defecto)
- **`>`**: Espera tu comando
- **REPL**: Read-Eval-Print Loop

**🔍 ¿Qué es `:type`?**

- **Comando de GHCi**: Muestra el **tipo** de una expresión
- **`:type expresion`**: No evalúa, solo muestra el tipo
- **Abreviación**: `:t` hace lo mismo
- **Útil**: Para entender qué tipo tiene algo

**🔍 ¿Qué significa `Num a => [a]`?**

- **Constraint de tipo**: Restricción sobre el tipo `a`
- **`Num a`**: `a` debe ser un tipo numérico (Int, Double, etc.)
- **`=>`**: "implica" o "dado que"
- **`[a]`**: Lista de elementos del tipo `a`
- **Se lee**: "Para cualquier tipo `a` que sea numérico, esto es una lista de `a`"

### **🎯 Conceptos Clave para Recordar**

1. **Las listas son LA estructura fundamental** de Haskell
2. **Son inmutables** - no se modifican, se crean nuevas
3. **Son homogéneas** - todos los elementos del mismo tipo
4. **Son dinámicas** - pueden crecer sin límite
5. **Son lazy** - se evalúan solo cuando se necesitan
6. **Son recursivas** - definidas en términos de sí mismas
7. **String = [Char]** - los strings son listas de caracteres
8. **Dos formas**: vacía `[]` o elemento seguido de lista `(x:xs)`

### **🚀 Lo Que Viene**

Ahora que entiendes **qué** son las listas y **por qué** son importantes, en la siguiente sección aprenderás **cómo construirlas** usando los operadores fundamentales `:` y `[]`.

¡Las listas van a cambiar completamente tu forma de programar! 🌟

## Construcción con `:` y `[]`

### **🔨 Las Dos Piezas Fundamentales**

En Haskell, **todas** las listas se construyen con solo dos ingredientes:

```haskell
-- 1. [] - La lista vacía
listaVacia = []

-- 2. (:) - El operador "cons" (de constructor)
-- Agrega un elemento al FRENTE de una lista
listaNueva = 1 : []  -- [1]
```

¡Con estas dos piezas puedes construir cualquier lista en Haskell!

### **⚡ El Operador `:` (Cons)**

```haskell
-- Firma de tipo del operador (:)
-- (:) :: a -> [a] -> [a]

-- Ejemplos básicos
ejemplo1 = 5 : []                -- [5]
ejemplo2 = 1 : [2, 3]            -- [1,2,3]
ejemplo3 = 'H' : "ola"           -- "Hola"

-- Construcción paso a paso
lista1 = 1 : []                  -- [1]
lista2 = 2 : lista1              -- [2,1]
lista3 = 3 : lista2              -- [3,2,1]

-- Equivalente a:
lista123 = 3 : (2 : (1 : []))    -- [3,2,1]
```

**🔍 ¿Qué significa `(:) :: a -> [a] -> [a]`?**

```haskell
-- (:) :: a -> [a] -> [a]
--        │     │      │
--        │     │      └── Retorna: lista del mismo tipo
--        │     └────────── Segundo parámetro: una lista
--        └─────────────── Primer parámetro: un elemento
```

**🔍 ¿Solo agrega elementos al frente?**

```haskell
-- ✅ Agregar al FRENTE (O(1) - instantáneo)
alFrente = 1 : [2, 3, 4]        -- [1,2,3,4]

-- ❌ NO hay operador para agregar al final directamente
-- Para agregar al final necesitas concatenación:
alFinal = [1, 2, 3] ++ [4]       -- [1,2,3,4]
```

### **🧱 Construyendo Listas desde Cero**

```haskell
-- TODAS estas listas son exactamente iguales:

-- 1. Notación de lista literal (azúcar sintáctico)
lista1 = [1, 2, 3, 4, 5]

-- 2. Cons explícito
lista2 = 1 : 2 : 3 : 4 : 5 : []

-- 3. Cons con paréntesis explícitos
lista3 = 1 : (2 : (3 : (4 : (5 : []))))

-- Prueba en GHCi:
-- *Main> lista1 == lista2 && lista2 == lista3
-- True
```

**🔍 ¿Qué ocurre internamente?**

```haskell
-- El compilador traduce [1, 2, 3] a:
-- 1 : (2 : (3 : []))

-- Podemos visualizarlo como:
--    (:)
--   /   \
--  1    (:)
--      /   \
--     2    (:)
--         /   \
--        3     []
```

### **🔄 Asociatividad Derecha del Operador `:`**

```haskell
-- El operador : asocia por la derecha
x : y : z : [] = x : (y : (z : []))

-- Por eso no necesitamos paréntesis
lista = 1 : 2 : 3 : 4 : []       -- [1,2,3,4]

-- Pero este SÍ necesita paréntesis
listaMala = (1 : 2) : 3 : 4 : [] -- ¡ERROR! (1:2) no es una lista

-- Para construir lista de listas necesitas paréntesis
listasDeListas = [1,2] : [3,4] : [5,6] : []  -- [[1,2],[3,4],[5,6]]
```

**🔍 ¿Por qué es importante la asociatividad?**

```haskell
-- Sin asociatividad derecha:
-- 1 : 2 : [] podría interpretarse como (1 : 2) : []
-- ¡Pero (1 : 2) no es válido! El 2do argumento debe ser lista

-- Con asociatividad derecha:
-- 1 : 2 : [] se interpreta como 1 : (2 : [])
-- 2 : [] = [2]
-- 1 : [2] = [1,2]
```

### **🎮 Ejemplos Prácticos**

```haskell
-- 1. Insertar al inicio (push) - O(1)
push :: a -> [a] -> [a]
push x xs = x : xs

ghci> push 5 [1,2,3]
[5,1,2,3]

-- 2. Construir lista de números en reversa
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = [0]
cuentaRegresiva n = n : cuentaRegresiva (n-1)

ghci> cuentaRegresiva 5
[5,4,3,2,1,0]

-- 3. Agregar a lista solo si no existe
agregarUnico :: Eq a => a -> [a] -> [a]
agregarUnico x xs
  | x `elem` xs = xs            -- Ya existe, devolver igual
  | otherwise   = x : xs        -- No existe, agregar al frente

ghci> agregarUnico 3 [1,2,3,4]
[1,2,3,4]
ghci> agregarUnico 5 [1,2,3,4]
[5,1,2,3,4]
```

### **⚙️ Patrones Comunes con `:`**

```haskell
-- 1. Crear lista al revés (eficiente)
reversa :: [a] -> [a]
reversa xs = aux xs []
  where
    aux [] acc = acc
    aux (x:xs) acc = aux xs (x:acc)

ghci> reversa [1,2,3,4,5]
[5,4,3,2,1]

-- 2. Agregar al final (ineficiente - solo para ilustrar)
agregarAlFinal :: a -> [a] -> [a]
agregarAlFinal x [] = [x]
agregarAlFinal x (y:ys) = y : agregarAlFinal x ys

ghci> agregarAlFinal 9 [1,2,3]
[1,2,3,9]

-- 3. Duplicar cada elemento con cons
duplicar :: [a] -> [a]
duplicar [] = []
duplicar (x:xs) = x : x : duplicar xs

ghci> duplicar [1,2,3]
[1,1,2,2,3,3]
```

### **🏆 El Poder de `[]` - Lista Vacía**

```haskell
-- [] es el caso base de todas las listas
-- Usos comunes:

-- 1. Verificar si una lista está vacía
estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _  = False

-- 2. Como valor inicial
listaDesdeVacia = 1 : 2 : 3 : []

-- 3. Como caso base en recursión
longitud :: [a] -> Int
longitud [] = 0                 -- Caso base: lista vacía = 0
longitud (_:xs) = 1 + longitud xs

-- 4. Como acumulador inicial
sumaAcumulativa :: [Int] -> [Int]
sumaAcumulativa xs = go xs 0 []
  where
    go [] _ acc = reverse acc
    go (x:xs) suma acc = go xs (suma + x) ((suma + x) : acc)

ghci> sumaAcumulativa [1,2,3,4]
[1,3,6,10]
```

**🔍 ¿Por qué `[]` es tan importante?**

```haskell
-- Sin [], no podríamos:
-- 1. Terminar la recursión
-- 2. Representar listas vacías
-- 3. Construir listas desde cero

-- La lista vacía es como el 0 en la aritmética
-- TODO se construye a partir de ella
```

### **🔄 Comparación con `++` (Concatenación)**

```haskell
-- (:)  - Agrega UN elemento al FRENTE - O(1)
-- (++) - Une DOS listas - O(n)

lista1 = 1 : [2,3,4]            -- [1,2,3,4]
lista2 = [1] ++ [2,3,4]         -- [1,2,3,4]

-- PERO hay una diferencia ENORME en eficiencia:

-- (:) es O(1) - instantáneo sin importar tamaño
listaGrande = 0 : [1..1000000]  -- Instantáneo

-- (++) es O(n) - tiempo proporcional al primer argumento
-- [1..1000000] ++ [0]          -- ¡Muy lento!

-- Concatenando múltiples listas:
rapida = 1 : 2 : 3 : 4 : 5 : [] -- Cada : es O(1)
lenta = [1] ++ [2] ++ [3] ++ [4] ++ [5] -- Cada ++ es O(n)
```

**🔍 ¿Por qué `:` es más eficiente que `++`?**

```haskell
-- Para 1 : [2,3,4]:
-- Solo necesita crear UN nuevo nodo
-- ┌───┐    ┌───┐    ┌───┐    ┌───┐
-- │ 1 │───>│ 2 │───>│ 3 │───>│ 4 │
-- └───┘    └───┘    └───┘    └───┘

-- Para [1] ++ [2,3,4]:
-- Debe recorrer [1] completo (1 elemento)
-- ┌───┐    ┌───┐    ┌───┐    ┌───┐
-- │ 1 │───>│ 2 │───>│ 3 │───>│ 4 │
-- └───┘    └───┘    └───┘    └───┘

-- Para [1,2,3] ++ [4]:
-- Debe recorrer [1,2,3] completo (3 elementos)
-- ┌───┐    ┌───┐    ┌───┐    ┌───┐
-- │ 1 │───>│ 2 │───>│ 3 │───>│ 4 │
-- └───┘    └───┘    └───┘    └───┘
```

### **🧪 Técnicas Avanzadas**

```haskell
-- 1. Preprocesar y luego construir lista
palabrasCapitalizadas :: String -> [String]
palabrasCapitalizadas texto =
  let palabras = words texto  -- Divide en palabras
      capitalizar (c:cs) = toUpper c : cs
      capitalizar [] = []
  in map capitalizar palabras

ghci> palabrasCapitalizadas "hola mundo funcional"
["Hola","Mundo","Funcional"]

-- 2. Acumulador de lista eficiente (técnica común)
revertirPalabras :: String -> String
revertirPalabras = go [] . words
  where
    go acc [] = unwords (reverse acc)
    go acc (x:xs) = go (x:acc) xs

ghci> revertirPalabras "hola mundo funcional"
"funcional mundo hola"

-- 3. Construir lista infinita con (:)
repetir :: a -> [a]
repetir x = x : repetir x  -- ¡Lista infinita!

-- Usando take para obtener solo algunos
ghci> take 5 (repetir 7)
[7,7,7,7,7]
```

### **💡 Patrones con Pattern Matching**

```haskell
-- La construcción con : y pattern matching son inseparables

-- 1. Extraer head y tail
primeroYResto :: [a] -> (Maybe a, [a])
primeroYResto [] = (Nothing, [])
primeroYResto (x:xs) = (Just x, xs)

ghci> primeroYResto [1,2,3]
(Just 1,[2,3])
ghci> primeroYResto []
(Nothing,[])

-- 2. Extraer los primeros dos elementos
dosPrimeros :: [a] -> [a]
dosPrimeros (x:y:_) = [x,y]  -- Match primeros dos
dosPrimeros [x] = [x]        -- Match solo uno
dosPrimeros [] = []          -- Lista vacía

ghci> dosPrimeros [1,2,3,4,5]
[1,2]
ghci> dosPrimeros [1]
[1]

-- 3. Patrón común: extraer mientras queden elementos
extraerPares :: [(a,b)] -> ([a], [b])
extraerPares [] = ([], [])
extraerPares ((a,b):resto) =
  let (as, bs) = extraerPares resto
  in (a:as, b:bs)

ghci> extraerPares [(1,"a"), (2,"b"), (3,"c")]
([1,2,3],["a","b","c"])
```

### **🛠️ Construcción Eficiente de Listas Grandes**

```haskell
-- REGLAS PARA CONSTRUCCIÓN EFICIENTE:

-- ✅ BIEN: Construir listas PREPEND (al frente con :)
construirBien :: Int -> [Int]
construirBien n = go n []
  where
    go 0 acc = acc
    go n acc = go (n-1) (n:acc)  -- Agregar al frente es O(1)

-- ❌ MAL: Construir listas APPEND (al final con ++)
construirMal :: Int -> [Int]
construirMal 0 = []
construirMal n = construirMal (n-1) ++ [n]  -- Append es O(n)

-- Comparación de rendimiento:
-- construirBien 10000 - Instantáneo
-- construirMal 10000  - Varios segundos (O(n²) total)

-- Demostración:
ghci> :set +s  -- Muestra tiempo de ejecución
ghci> length $ construirBien 50000
50000
(0.02 secs, 14,056,984 bytes)
ghci> length $ construirMal 5000  -- ¡10 veces menos elementos!
5000
(8.52 secs, 2,787,685,512 bytes)  -- ¡400 veces más lento!
```

### **🎯 Truco Estándar: Acumulador Invertido**

```haskell
-- Patrón MUY COMÚN en Haskell:
-- 1. Construir lista al revés con (:) - O(n)
-- 2. Revertirla al final una sola vez - O(n)
-- Total: O(n) - mucho mejor que O(n²)

-- Ejemplo: generar números desde 1 hasta n
hastaN :: Int -> [Int]
hastaN n = go n []
  where
    -- Construimos [n,n-1,...,2,1]
    go 0 acc = acc
    go m acc = go (m-1) (m:acc)

-- Ejemplo: procesar entrada preservando orden
procesarEntrada :: [String] -> [String]
procesarEntrada lineas = reverse (go lineas [])
  where
    go [] acc = acc
    go (x:xs) acc = go xs (procesada:acc)
      where procesada = map toUpper x

ghci> procesarEntrada ["hola", "mundo"]
["HOLA","MUNDO"]
```

### **🏗️ Funciones Útiles Basadas en `:` y `[]`**

```haskell
-- 1. Implementar 'init' (todos menos el último)
miInit :: [a] -> [a]
miInit [] = error "Lista vacía"
miInit [_] = []
miInit (x:xs) = x : miInit xs

ghci> miInit [1,2,3,4]
[1,2,3]

-- 2. Intercalar elemento entre todos los elementos
intercalar :: a -> [a] -> [a]
intercalar _ [] = []
intercalar _ [x] = [x]
intercalar sep (x:xs) = x : sep : intercalar sep xs

ghci> intercalar 0 [1,2,3,4]
[1,0,2,0,3,0,4]

-- 3. Comprimir runs (secuencias del mismo elemento)
comprimir :: Eq a => [a] -> [(a, Int)]
comprimir [] = []
comprimir (x:xs) =
  let (iguales, diferentes) = span (== x) xs
      longitud = 1 + length iguales
  in (x, longitud) : comprimir diferentes

ghci> comprimir "aaabbbcccaaa"
[('a',3),('b',3),('c',3),('a',3)]
```

### **🔄 Implementaciones Manuales**

```haskell
-- Implementar reverse con (:)
miReverse :: [a] -> [a]
miReverse = go []
  where
    go acc [] = acc
    go acc (x:xs) = go (x:acc) xs

-- Implementar map con (:)
miMap :: (a -> b) -> [a] -> [b]
miMap _ [] = []
miMap f (x:xs) = f x : miMap f xs

-- Implementar filter con (:)
miFilter :: (a -> Bool) -> [a] -> [a]
miFilter _ [] = []
miFilter p (x:xs)
  | p x       = x : miFilter p xs
  | otherwise = miFilter p xs

-- Todas usan (:) y [] para construir las nuevas listas
```

### **🎯 Consejos Clave para Recordar**

```haskell
-- 1. SIEMPRE construye listas con (:) al frente
-- 2. EVITA (++) en loops o recursión
-- 3. Si necesitas mantener orden, usa acumulador y reverse
-- 4. [] es el caso base para TODA recursión en listas
-- 5. (x:xs) es EL patrón principal para procesamiento de listas
```

**🔍 ¿Cuándo NO usar `:` y `[]`?**

```haskell
-- Evita construir listas cuando:

-- 1. Necesitas acceso aleatorio frecuente
--    Usa Vector/Array en su lugar

-- 2. Necesitas estructura clave-valor
--    Usa Map en su lugar

-- 3. Necesitas conjuntos sin duplicados
--    Usa Set en su lugar

-- 4. Procesamientos de texto masivos
--    Usa Text en lugar de [Char]
```

### **🚀 Lo Que Viene Después**

Ahora que dominas la construcción de listas, en la siguiente sección veremos cómo **descomponerlas** usando **pattern matching** para realizar operaciones poderosas sobre sus elementos.

¡Las listas construidas con `:` y `[]` son los LEGO fundamentales de Haskell! 🌟
