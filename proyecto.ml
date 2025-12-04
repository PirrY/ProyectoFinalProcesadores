type token =
  (* Palabras reservadas *)
  | KW_DECLARAR
  | KW_FUNCTION
  | KW_SI
  | KW_ENTONCES
  | KW_SINO
  | KW_MIENTRAS
  | KW_PARA
  | KW_DESDE
  | KW_HASTA
  | KW_HACER
  | KW_FIN
  | KW_IMPRIMIR
  | KW_RETORNAR
  | KW_ENTERO
  | KW_REAL
  | KW_LOGICO
  | KW_VERDADERO
  | KW_FALSO

  (* Operadores *)
  | OP_ASIG           (* <- o -> *)
  | OP_MAS            (* + *)
  | OP_MENOS          (* - *)
  | OP_POR            (* * *)
  | OP_DIV            (* / *)
  | OP_MAYOR          (* > *)
  | OP_MENOR          (* < *)
  | OP_MAYORIG        (* >= *)
  | OP_MENORIG        (* <= *)
  | OP_IGUAL          (* == *)
  | OP_DISTINTO       (* != *)
  | OP_Y              (* y *)
  | OP_O              (* o *)

  (* Delimitadores *)
  | DOS_PUNTOS        (* : *)
  | PAREN_IZQ         (* ( *)
  | PAREN_DER         (* ) *)
  | LLAVE_IZQ         (* { *)
  | LLAVE_DER         (* } *)
  | COMA              (* , *)
  | PUNTOYCOMA        (* ; *)

  (* Literales e identificadores *)
  | INT of int
  | FLOAT of float
  | STRING of string
  | IDENT of string

  (* Fin de archivo *)
  | EOF

(* Tipo para los valores en tiempo de ejecución *)
type valor =
  | VInt of int
  | VFloat of float
  | VString of string
  | VBool of bool
  | VNulo

(* Tipo para las expresiones (AST) *)
type expresion =
  | EInt of int
  | EFloat of float
  | EString of string
  | EBool of bool
  | EIdent of string
  | EBinaria of string * expresion * expresion
  | EUnaria of string * expresion

(* Tipo para anotaciones de tipo *)
type tipo_anotacion =
  | TipoEntero
  | TipoReal
  | TipoLogico
  | TipoNone

(* Tipo para las sentencias (AST) *)
type sentencia =
  | SDeclaracion of string * tipo_anotacion * expresion
  | SAsignacion of string * expresion
  | SImprimir of expresion
  | SCondicional of expresion * sentencia list * sentencia list
  | SMientras of expresion * sentencia list
  | SPara of string * expresion * expresion * sentencia list
  | SExpresion of expresion

(*   ANALIZADOR LÉXICO   *)

(* Función para verificar si un caracter es dígito *)
let es_digito c = c >= '0' && c <= '9'

(* Función para verificar si un caracter es letra o guion bajo *)
let es_letra c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

(* Función para verificar si un caracter es alfanumérico *)
let es_alfanumerico c = es_letra c || es_digito c

(* Lee un número caracter por caracter
   - acc: acumula el string del número
   - tiene_punto: controla si ya encontró un punto decimal *)
let rec leer_numero chars acc tiene_punto =
  match chars with
  | c :: rest when es_digito c ->
      leer_numero rest (acc ^ String.make 1 c) tiene_punto
  | '.' :: rest when not tiene_punto ->
      leer_numero rest (acc ^ ".") true
  | _ ->
      (* Ya no hay más dígitos, convertir a token *)
      if tiene_punto then
        (FLOAT (float_of_string acc), chars)
      else
        (INT (int_of_string acc), chars)

(* Función para leer un identificador o palabra reservada *)
let rec leer_identificador chars acc =
  match chars with
  | c :: rest when es_alfanumerico c ->
      leer_identificador rest (acc ^ (String.make 1 c))
  | _ -> (acc, chars)

(* Función para leer una cadena de texto *)
let rec leer_texto chars acc =
  match chars with
  | '"' :: rest -> (acc, rest)
  | '\\' :: '"' :: rest -> leer_texto rest (acc ^ "\"")
  | '\\' :: 'n' :: rest -> leer_texto rest (acc ^ "\n")
  | '\\' :: 't' :: rest -> leer_texto rest (acc ^ "\t")
  | c :: rest -> leer_texto rest (acc ^ (String.make 1 c))
  | [] -> failwith "Error: Texto sin cerrar"

(* Función para convertir palabra a token *)
let palabra_a_token palabra =
  match palabra with
  | "declarar" -> KW_DECLARAR
  | "funcion" -> KW_FUNCTION
  | "si" -> KW_SI
  | "entonces" -> KW_ENTONCES
  | "sino" -> KW_SINO
  | "mientras" -> KW_MIENTRAS
  | "para" -> KW_PARA
  | "desde" -> KW_DESDE
  | "hasta" -> KW_HASTA
  | "hacer" -> KW_HACER
  | "fin" -> KW_FIN
  | "imprimir" -> KW_IMPRIMIR
  | "retornar" -> KW_RETORNAR
  | "entero" -> KW_ENTERO
  | "real" -> KW_REAL
  | "logico" -> KW_LOGICO
  | "verdadero" -> KW_VERDADERO
  | "falso" -> KW_FALSO
  | "y" -> OP_Y
  | "o" -> OP_O
  | _ -> IDENT palabra

(* Función para saltar comentarios *)
let rec saltar_comentario chars =
  match chars with
  | '\n' :: rest -> rest
  | _ :: rest -> saltar_comentario rest
  | [] -> []

(* Función principal de tokenización *)
let rec tokenizar chars =
  match chars with
  | [] -> [EOF]
  | ' ' :: rest | '\t' :: rest | '\r' :: rest ->
      tokenizar rest
  | '\n' :: rest -> tokenizar rest
  | '#' :: rest -> tokenizar (saltar_comentario rest)
  | '<' :: '-' :: rest -> OP_ASIG :: tokenizar rest
  | '-' :: '>' :: rest -> OP_ASIG :: tokenizar rest
  | '=' :: '=' :: rest -> OP_IGUAL :: tokenizar rest
  | '!' :: '=' :: rest -> OP_DISTINTO :: tokenizar rest
  | '>' :: '=' :: rest -> OP_MAYORIG :: tokenizar rest
  | '<' :: '=' :: rest -> OP_MENORIG :: tokenizar rest
  | '+' :: rest -> OP_MAS :: tokenizar rest
  | '-' :: rest -> OP_MENOS :: tokenizar rest
  | '*' :: rest -> OP_POR :: tokenizar rest
  | '/' :: rest -> OP_DIV :: tokenizar rest
  | '>' :: rest -> OP_MAYOR :: tokenizar rest
  | '<' :: rest -> OP_MENOR :: tokenizar rest
  | '(' :: rest -> PAREN_IZQ :: tokenizar rest
  | ')' :: rest -> PAREN_DER :: tokenizar rest
  | '{' :: rest -> LLAVE_IZQ :: tokenizar rest
  | '}' :: rest -> LLAVE_DER :: tokenizar rest
  | ':' :: rest -> DOS_PUNTOS :: tokenizar rest
  | ',' :: rest -> COMA :: tokenizar rest
  | ';' :: rest -> PUNTOYCOMA :: tokenizar rest
  | '"' :: rest ->
      let (texto, chars_restantes) = leer_texto rest "" in
      STRING texto :: tokenizar chars_restantes
  | c :: rest when es_digito c ->
      let (num_token, chars_restantes) = leer_numero rest (String.make 1 c) false in
      num_token :: tokenizar chars_restantes
  | c :: rest when es_letra c ->
      let (palabra, chars_restantes) = leer_identificador rest (String.make 1 c) in
      palabra_a_token palabra :: tokenizar chars_restantes
  | c :: _ -> failwith ("Error léxico: Caracter inesperado '" ^ (String.make 1 c) ^ "'")

(* Convierte un string a lista de caracteres
   Ejemplo: "hola" -> ['h'; 'o'; 'l'; 'a'] *)
let string_a_chars s =
  List.init (String.length s) (String.get s)

(*   ANALIZADOR SINTÁCTICO   *)

(* Excepción para errores de parseo *)
exception Error_parseo of string

(* Función para consumir un token esperado *)
let consumir tokens token_esperado =
  match tokens with
  | t :: rest when t = token_esperado -> rest
  | t :: _ ->
      let nombre_esperado = match token_esperado with
        | DOS_PUNTOS -> "':'"
        | KW_FIN -> "'fin'"
        | KW_HACER -> "'hacer'"
        | PAREN_DER -> "')'"
        | _ -> "un token específico"
      in
      raise (Error_parseo ("Se esperaba " ^ nombre_esperado))
  | [] -> raise (Error_parseo "Fin de archivo inesperado")

(* Parsear expresión primaria *)
let rec parsear_primaria tokens =
  match tokens with
  | INT n :: rest -> (EInt n, rest)
  | FLOAT f :: rest -> (EFloat f, rest)
  | STRING s :: rest -> (EString s, rest)
  | KW_VERDADERO :: rest -> (EBool true, rest)
  | KW_FALSO :: rest -> (EBool false, rest)
  | IDENT id :: rest -> (EIdent id, rest)
  | PAREN_IZQ :: rest ->
      let (expr, rest2) = parsear_expresion rest in
      let rest3 = consumir rest2 PAREN_DER in
      (expr, rest3)
  | _ -> raise (Error_parseo "Expresión primaria esperada")

(* Parsear término (multiplicación, división) *)
and parsear_termino tokens =
  let (izq, rest) = parsear_primaria tokens in
  let rec aux expr tokens =
    match tokens with
    | OP_POR :: rest ->
        let (der, rest2) = parsear_primaria rest in
        aux (EBinaria ("*", expr, der)) rest2
    | OP_DIV :: rest ->
        let (der, rest2) = parsear_primaria rest in
        aux (EBinaria ("/", expr, der)) rest2
    | _ -> (expr, tokens)
  in
  aux izq rest

(* Parsear expresión aritmética (suma, resta) *)
and parsear_aritmetica tokens =
  let (izq, rest) = parsear_termino tokens in
  let rec aux expr tokens =
    match tokens with
    | OP_MAS :: rest ->
        let (der, rest2) = parsear_termino rest in
        aux (EBinaria ("+", expr, der)) rest2
    | OP_MENOS :: rest ->
        let (der, rest2) = parsear_termino rest in
        aux (EBinaria ("-", expr, der)) rest2
    | _ -> (expr, tokens)
  in
  aux izq rest

(* Parsear expresión relacional *)
and parsear_relacional tokens =
  let (izq, rest) = parsear_aritmetica tokens in
  match rest with
  | OP_MAYOR :: rest2 ->
      let (der, rest3) = parsear_aritmetica rest2 in
      (EBinaria (">", izq, der), rest3)
  | OP_MENOR :: rest2 ->
      let (der, rest3) = parsear_aritmetica rest2 in
      (EBinaria ("<", izq, der), rest3)
  | OP_MAYORIG :: rest2 ->
      let (der, rest3) = parsear_aritmetica rest2 in
      (EBinaria (">=", izq, der), rest3)
  | OP_MENORIG :: rest2 ->
      let (der, rest3) = parsear_aritmetica rest2 in
      (EBinaria ("<=", izq, der), rest3)
  | OP_IGUAL :: rest2 ->
      let (der, rest3) = parsear_aritmetica rest2 in
      (EBinaria ("==", izq, der), rest3)
  | OP_DISTINTO :: rest2 ->
      let (der, rest3) = parsear_aritmetica rest2 in
      (EBinaria ("!=", izq, der), rest3)
  | _ -> (izq, rest)

(* Parsear expresión lógica *)
and parsear_logica tokens =
  let (izq, rest) = parsear_relacional tokens in
  let rec aux expr tokens =
    match tokens with
    | OP_Y :: rest ->
        let (der, rest2) = parsear_relacional rest in
        aux (EBinaria ("&&", expr, der)) rest2
    | OP_O :: rest ->
        let (der, rest2) = parsear_relacional rest in
        aux (EBinaria ("||", expr, der)) rest2
    | _ -> (expr, tokens)
  in
  aux izq rest

(* Parsear expresión general
   Esta es la entrada principal que delega a parsear_logica
   La jerarquía de precedencia es: logica > relacional > aritmetica > termino > primaria *)
and parsear_expresion tokens = parsear_logica tokens

(* Parsear anotación de tipo *)
let parsear_tipo tokens =
  match tokens with
  | DOS_PUNTOS :: KW_ENTERO :: rest -> (TipoEntero, rest)
  | DOS_PUNTOS :: KW_REAL :: rest -> (TipoReal, rest)
  | DOS_PUNTOS :: KW_LOGICO :: rest -> (TipoLogico, rest)
  | _ -> (TipoNone, tokens)

(* Parsear sentencia *)
let rec parsear_sentencia tokens =
  match tokens with
  | KW_DECLARAR :: IDENT id :: rest ->
      let (tipo, rest2) = parsear_tipo rest in
      let rest3 = consumir rest2 OP_ASIG in
      let (expr, rest4) = parsear_expresion rest3 in
      (SDeclaracion (id, tipo, expr), rest4)

  | IDENT id :: OP_ASIG :: rest ->
      let (expr, rest2) = parsear_expresion rest in
      (SAsignacion (id, expr), rest2)

  | KW_IMPRIMIR :: PAREN_IZQ :: rest ->
      let (expr, rest2) = parsear_expresion rest in
      let rest3 = consumir rest2 PAREN_DER in
      (SImprimir expr, rest3)

  | KW_SI :: rest ->
      let (cond, rest2) = parsear_expresion rest in
      let rest3 = consumir rest2 DOS_PUNTOS in
      let (sentencias_si, rest4) = parsear_bloque rest3 in
      (match rest4 with
       | KW_SINO :: DOS_PUNTOS :: rest5 ->
           let (sentencias_no, rest6) = parsear_bloque rest5 in
           let rest7 = consumir rest6 KW_FIN in
           (SCondicional (cond, sentencias_si, sentencias_no), rest7)
       | KW_FIN :: rest5 ->
           (SCondicional (cond, sentencias_si, []), rest5)
       | _ -> raise (Error_parseo "Se esperaba 'sino' o 'fin'"))

  | KW_MIENTRAS :: rest ->
      let (cond, rest2) = parsear_expresion rest in
      let rest3 = consumir rest2 DOS_PUNTOS in
      let (sentencias, rest4) = parsear_bloque rest3 in
      let rest5 = consumir rest4 KW_FIN in
      (SMientras (cond, sentencias), rest5)

  | KW_PARA :: IDENT id :: KW_DESDE :: rest ->
      let (inicio, rest2) = parsear_expresion rest in
      let rest3 = consumir rest2 KW_HASTA in
      let (fin, rest4) = parsear_expresion rest3 in
      let rest5 = consumir rest4 DOS_PUNTOS in
      let (sentencias, rest6) = parsear_bloque rest5 in
      let rest7 = consumir rest6 KW_FIN in
      (SPara (id, inicio, fin, sentencias), rest7)

  | _ -> raise (Error_parseo "Sentencia no reconocida")

(* Parsear bloque de sentencias *)
and parsear_bloque tokens =
  let rec aux acc tokens =
    match tokens with
    | KW_FIN :: _ | KW_SINO :: _ | EOF :: _ -> (List.rev acc, tokens)
    | PUNTOYCOMA :: rest -> aux acc rest
    | _ ->
        let (sent, rest) = parsear_sentencia tokens in
        aux (sent :: acc) rest
  in
  aux [] tokens

(* Parsear programa completo *)
let rec parsear_programa tokens =
  match tokens with
  | EOF :: _ -> []
  | PUNTOYCOMA :: rest -> parsear_programa rest
  | _ ->
      let (sent, rest) = parsear_sentencia tokens in
      sent :: parsear_programa rest

(* EVALUADOR   *)

(* Tipo para el entorno de variables *)
type entorno = (string * valor) list

(* Busca una variable en el entorno (lista de pares nombre-valor)
   Lanza excepción si no existe *)
let rec buscar_variable entorno nombre =
  match entorno with
  | [] -> raise (Failure ("Variable no declarada: " ^ nombre))
  | (n, v) :: rest ->
      if n = nombre then v else buscar_variable rest nombre

(* Actualiza o agrega una variable en el entorno
   Si existe, reemplaza su valor. Si no existe, la agrega al inicio *)
let rec actualizar_entorno entorno nombre valor =
  match entorno with
  | [] -> [(nombre, valor)]
  | (n, v) :: rest ->
      if n = nombre then (n, valor) :: rest
      else (n, v) :: actualizar_entorno rest nombre valor

(* Conversión de valor a string *)
let valor_a_string v =
  match v with
  | VInt n -> string_of_int n
  | VFloat f -> string_of_float f
  | VString s -> s
  | VBool b -> if b then "verdadero" else "falso"
  | VNulo -> "nulo"

(* Conversión de valor a entero *)
let valor_a_int v =
  match v with
  | VInt n -> n
  | VFloat f -> int_of_float f
  | _ -> raise (Failure "Se esperaba un número")

(* Conversión de valor a float *)
let valor_a_float v =
  match v with
  | VInt n -> float_of_int n
  | VFloat f -> f
  | _ -> raise (Failure "Se esperaba un número")

(* Conversión de valor a booleano *)
let valor_a_bool v =
  match v with
  | VBool b -> b
  | _ -> raise (Failure "Se esperaba un valor lógico")

(* Evaluar expresión *)
let rec evaluar_expresion expr entorno =
  match expr with
  | EInt n -> VInt n
  | EFloat f -> VFloat f
  | EString s -> VString s
  | EBool b -> VBool b
  | EIdent id -> buscar_variable entorno id
  | EUnaria (op, e) ->
      let v = evaluar_expresion e entorno in
      (match op with
       | "no" -> VBool (not (valor_a_bool v))
       | _ -> raise (Failure ("Operador unario desconocido: " ^ op)))
  | EBinaria (op, izq, der) ->
      let v_izq = evaluar_expresion izq entorno in
      let v_der = evaluar_expresion der entorno in
      (match op with
       (* Operaciones aritméticas: si ambos son enteros, resultado entero.
          Si alguno es float, convierte ambos a float *)
       | "+" ->
           (match (v_izq, v_der) with
            | (VInt a, VInt b) -> VInt (a + b)
            | _ -> VFloat (valor_a_float v_izq +. valor_a_float v_der))
       | "-" ->
           (match (v_izq, v_der) with
            | (VInt a, VInt b) -> VInt (a - b)
            | _ -> VFloat (valor_a_float v_izq -. valor_a_float v_der))
       | "*" ->
           (match (v_izq, v_der) with
            | (VInt a, VInt b) -> VInt (a * b)
            | _ -> VFloat (valor_a_float v_izq *. valor_a_float v_der))
       | "/" ->
           (match (v_izq, v_der) with
            | (VInt a, VInt b) ->
                if b = 0 then raise (Failure "División por cero")
                else VInt (a / b)
            | _ ->
                let divisor = valor_a_float v_der in
                if divisor = 0.0 then raise (Failure "División por cero")
                else VFloat (valor_a_float v_izq /. divisor))
       | ">" -> VBool (valor_a_float v_izq > valor_a_float v_der)
       | "<" -> VBool (valor_a_float v_izq < valor_a_float v_der)
       | ">=" -> VBool (valor_a_float v_izq >= valor_a_float v_der)
       | "<=" -> VBool (valor_a_float v_izq <= valor_a_float v_der)
       | "==" -> VBool (v_izq = v_der)
       | "!=" -> VBool (v_izq <> v_der)
       | "&&" -> VBool (valor_a_bool v_izq && valor_a_bool v_der)
       | "||" -> VBool (valor_a_bool v_izq || valor_a_bool v_der)
       | _ -> raise (Failure ("Operador desconocido: " ^ op)))

(* Evaluar sentencia *)
let rec evaluar_sentencia sent entorno =
  match sent with
  | SDeclaracion (id, tipo, expr) ->
      let valor = evaluar_expresion expr entorno in
      actualizar_entorno entorno id valor

  | SAsignacion (id, expr) ->
      (* Primero verifica que la variable exista *)
      let _ = buscar_variable entorno id in
      let valor = evaluar_expresion expr entorno in
      actualizar_entorno entorno id valor

  | SImprimir expr ->
      let valor = evaluar_expresion expr entorno in
      print_endline (valor_a_string valor);
      entorno

  | SCondicional (cond, sent_si, sent_no) ->
      let cond_val = evaluar_expresion cond entorno in
      if valor_a_bool cond_val then
        evaluar_programa sent_si entorno
      else
        evaluar_programa sent_no entorno

  | SMientras (cond, sentencias) ->
      (* Loop recursivo: evalúa condición, ejecuta bloque, repite *)
      let rec loop env =
        let cond_val = evaluar_expresion cond env in
        if valor_a_bool cond_val then
          let nuevo_env = evaluar_programa sentencias env in
          loop nuevo_env
        else
          env
      in
      loop entorno

  | SPara (id, inicio, fin, sentencias) ->
      (* Evalúa inicio y fin una sola vez, luego itera *)
      let v_inicio = valor_a_int (evaluar_expresion inicio entorno) in
      let v_fin = valor_a_int (evaluar_expresion fin entorno) in
      let rec loop i env =
        if i <= v_fin then
          (* Actualiza la variable de iteración y ejecuta el bloque *)
          let env_con_i = actualizar_entorno env id (VInt i) in
          let nuevo_env = evaluar_programa sentencias env_con_i in
          loop (i + 1) nuevo_env
        else
          env
      in
      loop v_inicio entorno

  | SExpresion expr ->
      let _ = evaluar_expresion expr entorno in
      entorno

(* Evalúa una lista de sentencias secuencialmente
   Usa fold_left para pasar el entorno actualizado de una sentencia a la siguiente *)
and evaluar_programa sentencias entorno =
  List.fold_left (fun env sent -> evaluar_sentencia sent env) entorno sentencias

(* Pipeline completo: código -> chars -> tokens -> AST -> ejecución *)
let ejecutar_codigo codigo =
  try
    let chars = string_a_chars codigo in
    let tokens = tokenizar chars in
    let programa = parsear_programa tokens in
    let _ = evaluar_programa programa [] in
    ()
  with
  | Error_parseo msg -> print_endline ("Error de parseo: " ^ msg)
  | Failure msg -> print_endline ("Error de ejecución: " ^ msg)
  | _ -> print_endline "Error desconocido"

(* Leer archivo y ejecutar *)
let ejecutar_archivo nombre_archivo =
  try
    let canal = open_in nombre_archivo in
    let longitud = in_channel_length canal in
    let contenido = really_input_string canal longitud in
    close_in canal;
    ejecutar_codigo contenido
  with
  | Sys_error msg -> print_endline ("Error al abrir archivo: " ^ msg)

(* Punto de entrada principal *)
let () =
  if Array.length Sys.argv < 2 then
    print_endline "Uso: ocaml proyecto.ml <archivo.txt>"
  else
    ejecutar_archivo Sys.argv.(1)
