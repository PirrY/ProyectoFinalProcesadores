# ProyectoFinalProcesadores
# Lenguaje Firme (FRM)
## Documentación del Mini-Lenguaje Basado en OCaml

### Descripción
Firme (FRM) es un lenguaje de programación diseñado como una adaptación en español de OCaml,
enfocado en demostrar el análisis léxico y sintáctico de lenguajes de programación.
El intérprete traduce código FRM a OCaml internamente para su ejecución.

---

## Tabla Léxica de Tokens y Patrones

### Palabras Reservadas

| Token | Patrón | Significado | Equivalente OCaml |
|-------|--------|-------------|-------------------|
| `KW_DECLARAR` | `declarar\b` | Declaración de variable | `let` |
| `KW_FUNCTION` | `funcion\b` | Declaración de función | `let` / `let rec` |
| `KW_SI` | `si\b` | Inicio de condicional | `if` |
| `KW_ENTONCES` | `entonces\b` | Parte "then" | `then` |
| `KW_SINO` | `sino\b` | Parte "else" | `else` |
| `KW_MIENTRAS` | `mientras\b` | Bucle while | `while` |
| `KW_PARA` | `para\b` | Bucle for | `for` |
| `KW_DESDE` | `desde\b` | Límite inferior del for | `=` dentro del for |
| `KW_HASTA` | `hasta\b` | Límite superior del for | `to` / `downto` |
| `KW_HACER` | `hacer\b` | Cuerpo de ciclo | `do` |
| `KW_FIN` | `fin\b` | Fin de bloque | `done` / cierre |
| `KW_IMPRIMIR` | `imprimir\b` | Imprimir en pantalla | `print_endline` / `print_int` |
| `KW_RETORNAR` | `retornar\b` | Retorno de función | última expresión |
| `KW_ENTERO` | `entero\b` | Tipo entero | `int` |
| `KW_REAL` | `real\b` | Tipo real | `float` |
| `KW_LOGICO` | `logico\b` | Tipo booleano | `bool` |
| `KW_VERDADERO` | `verdadero\b` | Booleano true | `true` |
| `KW_FALSO` | `falso\b` | Booleano false | `false` |

### Operadores

| Token | Patrón | Significado | Equivalente OCaml |
|-------|--------|-------------|-------------------|
| `OP_ASIG` | `<-` o `->` | Asignación / Inicialización | `=` |
| `OP_MAS` | `\+` | Suma | `+` |
| `OP_MENOS` | `-` | Resta | `-` |
| `OP_POR` | `\*` | Multiplicación | `*` |
| `OP_DIV` | `/` | División | `/` |
| `OP_MAYOR` | `>` | Mayor que | `>` |
| `OP_MENOR` | `<` | Menor que | `<` |
| `OP_MAYORIG` | `>=` | Mayor o igual | `>=` |
| `OP_MENORIG` | `<=` | Menor o igual | `<=` |
| `OP_IGUAL` | `==` | Igualdad | `=` |
| `OP_DISTINTO` | `!=` | Distinto | `<>` |
| `OP_Y` | `y\b` | Conjunción lógica | `&&` |
| `OP_O` | `o\b` | Disyunción lógica | `\|\|` |

### Delimitadores

| Token | Patrón | Significado |
|-------|--------|-------------|
| `DOS_PUNTOS` | `:` | Separador |
| `PAREN_IZQ` | `\(` | Paréntesis izquierdo |
| `PAREN_DER` | `\)` | Paréntesis derecho |
| `LLAVE_IZQ` | `{` | Llave izquierda |
| `LLAVE_DER` | `}` | Llave derecha |
| `COMA` | `,` | Coma |
| `PUNTOYCOMA` | `;` | Fin de sentencia (opcional) |

### Literales

| Token | Patrón | Significado |
|-------|--------|-------------|
| `STRING` | `"[^"\\]*"` | Cadena de caracteres |
| `INT` | `[0-9]+` | Entero |
| `FLOAT` | `[0-9]+\.[0-9]+` | Real |
| `IDENT` | `[a-zA-Z_][a-zA-Z0-9_]*` | Identificador |

### Otros

| Token | Patrón | Significado |
|-------|--------|-------------|
| `COMENTARIO` | `#.*` | Comentario hasta fin de línea |
| `ESPACIO` | `[ \t\r]+` | Espacios (ignorados) |
| `NUEVA_LINEA` | `\n` | Salto de línea |

---

## Gramática del Lenguaje Firme (FRM)

### Especificación EBNF Simplificada

```
programa ::= {sentencia}

sentencia ::= decl_var
            | decl_funcion
            | asignacion
            | sentencia_if
            | sentencia_mientras
            | sentencia_para
            | llamada_imprimir
            | expresion [";"]

decl_var ::= "declarar" IDENT [":" tipo] "<-" expresion

tipo ::= "entero" | "real" | "logico"

asignacion ::= IDENT "<-" expresion

sentencia_if ::= "si" expresion ":"
                 bloque_then
                 ["sino" ":" bloque_else]
                 "fin"

sentencia_mientras ::= "mientras" expresion ":"
                       bloque
                       "fin"

sentencia_para ::= "para" IDENT "desde" expresion "hasta" expresion ":"
                   bloque
                   "fin"

llamada_imprimir ::= "imprimir" "(" expresion ")"

bloque ::= {sentencia}

expresion ::= expresion_logica

expresion_logica ::= expresion_relacional [("y" | "o") expresion_relacional]*

expresion_relacional ::= expresion_aritmetica
                        [(">" | "<" | ">=" | "<=" | "==" | "!=")
                        expresion_aritmetica]

expresion_aritmetica ::= termino [("+" | "-") termino]*

termino ::= factor [("*" | "/") factor]*

factor ::= INT
         | FLOAT
         | STRING
         | IDENT
         | "verdadero"
         | "falso"
         | "(" expresion ")"

IDENT ::= [a-zA-Z_][a-zA-Z0-9_]*
INT ::= [0-9]+
FLOAT ::= [0-9]+ "." [0-9]+
STRING ::= '"' [^"]* '"'
```

---

## Ejemplos de Programas en Firme (FRM)

### Ejemplo 1: Condicional Simple
```frm
declarar x : entero <- 20

si x > 5:
    imprimir("Mayor que 5")
sino:
    imprimir("Menor o igual a 5")
fin
```

**Traducción a OCaml:**
```ocaml
let x = 20 in
if x > 5 then
  print_endline "Mayor que 5"
else
  print_endline "Menor o igual a 5"
```

### Ejemplo 2: Bucle Mientras
```frm
declarar contador <- 0

mientras contador < 5:
    imprimir(contador)
    contador <- contador + 1
fin

imprimir("Bucle terminado")
```

**Traducción a OCaml:**
```ocaml
let contador = ref 0 in
while !contador < 5 do
  print_int !contador;
  contador := !contador + 1
done;
print_endline "Bucle terminado"
```

### Ejemplo 3: Bucle Para
```frm
para i desde 1 hasta 10:
    imprimir(i)
fin
```

**Traducción a OCaml:**
```ocaml
for i = 1 to 10 do
  print_int i
done
```

### Ejemplo 4: Operaciones Complejas
```frm
declarar a <- 10
declarar b <- 20
declarar suma <- a + b
declarar producto <- a * b

imprimir("La suma es:")
imprimir(suma)
imprimir("El producto es:")
imprimir(producto)

si suma > 25 y producto < 300:
    imprimir("Condicion cumplida")
sino:
    imprimir("Condicion no cumplida")
fin
```

**Traducción a OCaml:**
```ocaml
let a = 10 in
let b = 20 in
let suma = a + b in
let producto = a * b in
print_endline "La suma es:";
print_int suma;
print_endline "El producto es:";
print_int producto;
if suma > 25 && producto < 300 then
  print_endline "Condicion cumplida"
else
  print_endline "Condicion no cumplida"
```

### Ejemplo 5: Factorial
```frm
declarar numero <- 7
declarar factorial <- 1
declarar i <- 1

mientras i <= numero:
    factorial <- factorial * i
    i <- i + 1
fin

imprimir("El factorial de 7 es:")
imprimir(factorial)
```

---

## Componentes del Intérprete

### 1. Analizador Léxico (Lexer)
- Convierte el texto fuente en tokens
- Reconoce palabras reservadas, operadores, literales e identificadores
- Ignora espacios en blanco y comentarios
- Maneja números enteros y reales
- Procesa cadenas de texto con escape sequences

### 2. Analizador Sintáctico (Parser)
- Construye el árbol de sintaxis abstracta (AST)
- Implementa la gramática del lenguaje
- Maneja precedencia de operadores
- Valida la estructura del programa

### 3. Evaluador (Interpreter)
- Ejecuta el AST y produce resultados
- Mantiene un entorno de variables
- Realiza conversiones de tipos cuando es necesario
- Maneja operaciones aritméticas, lógicas y relacionales
- Implementa estructuras de control (if, while, for)

### 4. Main
- Lee archivos .txt con código FRM
- Coordina el proceso completo de compilación/interpretación
- Maneja errores y excepciones

---

## Uso del Intérprete

### Ejecución Directa con OCaml
```bash
ocaml proyecto.ml ejemplo.txt
```

### Compilación y Ejecución
```bash
ocamlopt -o firme proyecto.ml
./firme ejemplo.txt
```

### Formato de Archivos
- Extensión: `.txt`
- Codificación: UTF-8
- Comentarios: líneas que comienzan con `#`

---

## Diferencias con Aurora (versión anterior)

| Característica | Aurora | Firme (FRM) |
|----------------|--------|-------------|
| Condicional | `pregunta ... entonces ... sino` | `si ... entonces ... sino` |
| Comparadores | `mayor_que`, `menor_que`, etc. | `>`, `<`, `>=`, `<=`, `==`, `!=` |
| Asignación | `igual` o `→` | `<-` o `->` |
| Bucles | `repetir ... desde ... hasta` | `para ... desde ... hasta` |
| Salida | `mostrar()` | `imprimir()` |
| Tipos | No especificados | `entero`, `real`, `logico` |
| Comentarios | No definidos | `#` hasta fin de línea |

---

## Notas de Implementación

### Tipos de Datos
- **Enteros (int)**: Números sin punto decimal
- **Reales (float)**: Números con punto decimal
- **Cadenas (string)**: Texto entre comillas dobles
- **Booleanos (bool)**: `verdadero` o `falso`

### Conversiones Automáticas
- Operaciones entre enteros producen enteros
- Operaciones mixtas (entero + real) producen reales
- Comparaciones funcionan con cualquier tipo numérico

### Alcance de Variables
- Variables declaradas con `declarar` están disponibles en el resto del programa
- Variables de bucle `para` solo existen dentro del bucle
- No hay alcance de bloque (todas las variables son globales al programa)

### Limitaciones Actuales
- No hay funciones definidas por el usuario (pendiente)
- No hay arrays o listas
- No hay pattern matching
- No hay recursión
- No hay manejo explícito de errores en el lenguaje
