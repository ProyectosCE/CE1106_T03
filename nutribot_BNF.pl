/** NutriBot BNF */

:-style_check(-singleton).
:- set_prolog_flag(double_quotes, chars).

:- discontiguous oracion/2.
:- discontiguous pregunta/2.

/** start (Interjecciones y saludos) */
start([hola]).
start([gracias]).
start([buenas]).
start([adios]).
start([chao]).
start([hola,nutribot]).  % Acepta "Hola Nutribot"
start([saludos]).

/** final */
final([gracias]).
final([muchas,gracias]).
final([chao]).
final([adios]).

/** negaciones */
negativo([no|S],S).
negativo([nunca|S],S).
negativo([jamas|S],S).
negativo([nada|S],S).

/** afirmaciones */
afirmativo([si|S],S).
afirmativo([claro|S],S).
afirmativo([por,supuesto|S],S).
afirmativo([definitivamente|S],S).

/** determinantes */
determinante([yo|S],S).
determinante([tu|S],S).
determinante([el|S],S).
determinante([ella|S],S).
determinante([ellos|S],S).
determinante([nosotros|S],S).
determinante([un|S],S).
determinante([una|S],S).
determinante([mi|S],S).
determinante([las|S],S).
determinante([los|S],S).

/** sustantivos generales */
sustantivo_g([sobrepeso|S],S).
sustantivo_g([enfermedades|S],S).
sustantivo_g([salud|S],S).
sustantivo_g([dieta|S],S).
sustantivo_g([ayuda|S],S).
sustantivo_g([calorias|S],S).
sustantivo_g([sano|S],S).
sustantivo_g([sana|S],S).
sustantivo_g([peso|S],S).
sustantivo_g([nutribot|S],S).  % Reconoce "nutribot"
sustantivo_g([_,_|S],S).  % Omite sustantivos no reconocidos

/** verbos */
verbo([tengo|S],S).
verbo([puede|S],S).
verbo([ayudar|S],S).
verbo([ayudas|S],S).
verbo([estoy|S],S).
verbo([es|S],S).
verbo([deberia|S],S).
verbo([quiero|S],S).
verbo([consumir|S],S).
verbo([he|S],S).
verbo([pensado|S],S).
verbo([necesito|S],S).
verbo([perder|S],S).  % Añadido para reconocer "perder peso"
verbo([hacer|S],S).  % Añadido para reconocer "hacer una dieta"

/** adverbios */
adverbio([mucho|S],S).
adverbio([poco|S],S).
adverbio(S,S).

/** frases preposicionales y modificadores */
modificador([al,menos|S],S).
modificador([mas,de|S],S).
modificador([menos,de|S],S).
modificador(S,S).

/** frases preposicionales */
preposicional([en|S],S).
preposicional([por|S],S).
preposicional([a|S],S).
preposicional([de|S],S).  % Maneja "de 100 calorías"
preposicional([con|S],S).

/** números y frecuencias */
numero([N|S],S) :- number(N), !.  % Acepta números directamente
numero([N|S],S) :- atom_number(N, _), !.  % Acepta números en formato atom y los convierte
frecuencia([veces|S],S).
frecuencia([dias|S],S).
frecuencia([semanas|S],S).

/** manejo de oraciones */
oracion(A,B):- sintagma_nominal(A,C), sintagma_verbal(C,B), !.
oracion(A,B):- sintagma_verbal(A,B), !.
oracion(A,B):- start(A), B = [].  % Maneja saludos como "hola" o "adios"

/** saludos seguidos de oraciones */
oracion(A,B):- start(Saludo), append(Saludo, Resto, A), oracion(Resto,B), !.  % Maneja "Hola, quiero perder peso"
oracion(A,B):- start(Saludo), append(Saludo, Resto, A), pregunta(Resto,B), !.  % Maneja "Hola, me puedes ayudar con mi dieta"

/** negaciones */
oracion(A,B):- negativo(A,C), sintagma_verbal(C,B), !.
oracion(A,B):- negativo(A,C), verbo(C,D), preposicional(D,E), sustantivo_g(E,B), !.

/** preguntas */
pregunta(A,B):- pronombre_objeto(A,C), verbo(C,D), preposicional(D,E), sintagma_nominal(E,B), !.
pregunta(A,B):- pronombre_objeto(A,C), verbo(C,D), sustantivo_g(D,B), !.
pregunta(A,B):- verbo_invertido(A,C), sintagma_nominal(C,B), !.
pregunta(A,B):- verbo_invertido(A,C), sintagma_verbal(C,B), !.
pregunta(A,B):- verbo_invertido(A,B), !.  % Maneja preguntas sin complemento como "Me puedes ayudar"

/** oraciones compuestas */
oracion_compuesta(A,B):- oracion(A,C), [','|C], oracion(C,B), !.
oracion_compuesta(A,B):- oracion_simple(A,B), !.

/** oraciones simples */
oracion_simple([], []).
oracion_simple([Word|S],S) :- sustantivo_g([Word|S],S), !.
oracion_simple([Word|S],S) :- verbo([Word|S],S), !.
oracion_simple([Word|S],S) :- preposicional([Word|S],S), !.
oracion_simple([Word|S],S) :- numero([Word|S],S), !.  % Maneja números como enteros o átomos

/** sintagmas nominales */
sintagma_nominal(A,B):- determinante(A,C), sustantivo_g(C,B), !.
sintagma_nominal(A,B):- sustantivo_g(A,B), !.
sintagma_nominal(A,B):- numero(A,C), sustantivo_g(C,B), !.  % Maneja frases como "100 calorías"
sintagma_nominal(A,B):- numero(A,C), preposicional(C,D), sustantivo_g(D,B), !.  % Maneja "100 calorías de algo"

/** sintagmas verbales */
sintagma_verbal(A,B):- verbo(A,C), sintagma_nominal(C,B), !.
sintagma_verbal(A,B):- verbo(A,C), preposicional(C,D), sintagma_nominal(D,B), !.  % Maneja "quiero una dieta de 100 calorías"
sintagma_verbal(A,B):- verbo(A,C), numero(C,D), sustantivo_g(D,B), !.  % Maneja "consumir 100 calorías"
sintagma_verbal(A,B):- numero(A,C), sustantivo_g(C,B), !.  % Maneja "100 calorías"

/** preguntas y órdenes */

/** preguntas */
pregunta(A,B):- pronombre_objeto(A,C), verbo_invertido(C,D), sintagma_verbal(D,B), !.
pregunta(A,B):- verbo_invertido(A,C), sintagma_nominal(C,B), !.
pregunta(A,B):- verbo_invertido(A,C), preposicional(C,D), sustantivo_g(D,B), !.
pregunta(A,B):- verbo_invertido(A,C), verbo(C,D), sustantivo_g(D,B), !.

/** pronombres para preguntas */
pronombre_objeto([me|S],S).
pronombre_objeto([te|S],S).

/** órdenes */
imperativo([come|S],S).
imperativo([evita|S],S).

orden(A,B):- imperativo(A,C), sintagma_nominal(C,B), !.

/** verbos invertidos para preguntas */
verbo_invertido([puedes|S],S).
verbo_invertido([deberia|S],S).
verbo_invertido([es|S],S).

/** validación gramatical */

/** validacion para solo saludo */
validacion_gramatical(Oracion) :- start(Oracion), writeln('Solo saludo'), !.

/** validacion para saludo con oracion */
validacion_gramatical(Oracion) :- start(Saludo), append(Saludo, Resto, Oracion), oracion(Resto,[]), writeln('Saludo con oración'), !.

/** validacion para saludo con pregunta */
validacion_gramatical(Oracion) :- start(Saludo), append(Saludo, Resto, Oracion), pregunta(Resto,[]), writeln('Saludo con pregunta'), !.

/** validacion para pregunta (con o sin complemento) */
validacion_gramatical(Oracion) :- pregunta(Oracion,[]), writeln('Pregunta'), !.

/** validacion para oraciones afirmativas o negativas */
validacion_gramatical(Oracion) :- oracion(Oracion,[]), writeln('Oración afirmativa o negativa'), !.

/** validacion generica para el resto de las oraciones */
validacion_gramatical(Oracion):- nl, writeln('Oración gramaticalmente incorrecta'), writeln('Escriba de nuevo su oración'), nl, fail.
