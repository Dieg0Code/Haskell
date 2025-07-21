import Data.Char (isAsciiLower, isAsciiUpper, isDigit)

-- VALIDADOR DE DATOS BÁSICO
-- Solo usando tipos básicos y operadores

-- =====================-- VALIDACIÓN DE EDAD
-- =====================

-- Verificar si la edad está en rango valido
validarEdad :: Int -> Bool
validarEdad edad = edad >= 0 && edad <= 150

-- Verificar si es mayor de edad
esMayorEdad :: Int -> Bool
esMayorEdad edad = edad >= 18

-- =====================
-- VALIDACIÓN DE EMAIL
-- =====================

-- Verificar si contiene un '@'
tieneArroba :: String -> Bool
tieneArroba email = '@' `elem` email

-- Verificar si contiene un punto después del '@'
tienePuntoPostArroba :: String -> Bool
tienePuntoPostArroba email =
  let partePostArroba = dropWhile (/= '@') email -- dropWhile elimina todo antes del '@'
   in '.' `elem` partePostArroba

-- Ejemplo: asd@asd.com
-- dropWhile elimina 'asd@' y queda 'asd.com'
-- Luego la linea 'in '.' `elem` partePostArroba' verifica si hay un punto en 'asd.com', lo cual es True

-- Verificar longitud mínima del email
longitudMinimaEmail :: String -> Bool
longitudMinimaEmail email = length email >= 5

-- Verificar que no empiece o termine con un @ o un punto
noEmpiezaTerminaEspecial :: String -> Bool
noEmpiezaTerminaEspecial email =
  not (null email)
    && head email /= '@'
    && head email /= '.'
    && last email /= '@'
    && last email /= '.'

-- Primero verifica que no sea nulo, luego verifica que el primer y último carácter no sean '@' o '.'

-- Validación completa del email (básica)
validarEmail :: String -> Bool
validarEmail email =
  longitudMinimaEmail email
    && tieneArroba email
    && tienePuntoPostArroba email
    && noEmpiezaTerminaEspecial email

-- =============================
-- VALIDACIÓN DE CONTRASEÑA
-- =============================

-- Verificar la longitud mínima
minPasswordLength :: String -> Bool
minPasswordLength password = length password >= 8

-- Verificar que contiene al menos una mayúscula
hasCapitalizeLetter :: String -> Bool
hasCapitalizeLetter = any isAsciiUpper

-- Verificar que contiene al menos una minúscula
hasLowercaseLetter :: String -> Bool
hasLowercaseLetter = any isAsciiLower

-- Verificar que contiene al menos un número
hasNumber :: String -> Bool
hasNumber = any isDigit

-- Verificar que contiene al menos un carácter especial
hasSpecialChar :: String -> Bool
hasSpecialChar = any (`elem` "!@#$%^&*()_+-=[]{}|;:,.<>?")

-- Validación completa de la contraseña
validatePassword :: String -> Bool
validatePassword password =
  minPasswordLength password
    && hasCapitalizeLetter password
    && hasLowercaseLetter password
    && hasNumber password
    && hasSpecialChar password

-- =============================
-- FUNCIONES DE ANÁLISIS
-- =============================

-- Evaluar fortaleza de la contraseña
passwordStrength :: String -> String
passwordStrength password
  | length password < 6 = "Muy débil"
  | length password < 8 = "Débil"
  | not (hasCapitalizeLetter password) = "Regular"
  | not (hasLowercaseLetter password) = "Regular"
  | not (hasNumber password) = "Regular"
  | not (hasSpecialChar password) = "Buena"
  | length password >= 12 = "Muy fuerte"
  | otherwise = "Fuerte"

-- Clasificar edad en categorías
ageClassification :: Int -> String
ageClassification edad
  | edad < 0 = "Edad no válida"
  | edad <= 13 = "Niño"
  | edad <= 18 = "Adolescente"
  | edad <= 60 = "Adulto"
  | edad <= 150 = "Adulto mayor"
  | otherwise = "Edad no válida"

-- ============================
-- Validadores combinados
-- ============================

-- Validar todo los datos de un usuario
validarUsuario :: Int -> String -> String -> (Bool, [String])
validarUsuario edad email password =
  let errores = []
      -- Validar edad
      erroresEdad =
        if validarEdad edad
          then errores
          else "Edad inválida (0-150)" : errores
      -- Validar email
      erroresEmail =
        if validarEmail email
          then erroresEdad
          else "Email inválido" : erroresEdad
      -- Validar password
      erroresPassword =
        if validatePassword password
          then erroresEmail
          else "Contraseña inválida" : erroresEmail
      -- Resultado final
      esValido = null erroresPassword
   in (esValido, reverse erroresPassword)

-- Generar reporte de validación
reporteValidacion :: Int -> String -> String -> String
reporteValidacion edad email password =
  let edadValida = validarEdad edad
      emailValido = validarEmail email
      passwordValida = validatePassword password

      reporteEdad =
        if edadValida
          then "✓ Edad válida (" ++ show edad ++ " años - " ++ ageClassification edad ++ ")"
          else "✗ Edad inválida"

      reporteEmail =
        if emailValido
          then "✓ Email válido"
          else "✗ Email inválido"

      reportePassword =
        if passwordValida
          then "✓ Contraseña válida (" ++ passwordStrength password ++ ")"
          else "✗ Contraseña inválida (" ++ passwordStrength password ++ ")"

      separador = "\n"
   in reporteEdad ++ separador ++ reporteEmail ++ separador ++ reportePassword

-- =====================
-- FUNCIONES DE UTILIDAD
-- =====================

-- Contar tipos de caracteres en una contraseña
estadisticasPassword :: String -> (Int, Int, Int, Int)
estadisticasPassword password =
  let mayusculas = length (filter (\c -> c >= 'A' && c <= 'Z') password)
      minusculas = length (filter (\c -> c >= 'a' && c <= 'z') password)
      numeros = length (filter (\c -> c >= '0' && c <= '9') password)
      especiales = length (filter (`elem` "!@#$%^&*()_+-=[]{}|;:,.<>?") password)
   in (mayusculas, minusculas, numeros, especiales)

-- Verificar si un email tiene dominio específico
tieneDominio :: String -> String -> Bool
tieneDominio email dominio = dominio `elem` words (map (\c -> if c == '.' then ' ' else c) email)

-- Ejemplos de uso:
-- validarEdad 25              -- True
-- validarEmail "test@mail.com" -- True
-- validarPassword "MiPass123!" -- True
-- reporteValidacion 25 "test@mail.com" "MiPass123!"