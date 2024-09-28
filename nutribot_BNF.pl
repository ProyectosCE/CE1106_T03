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
start([hola,nutribot]).

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
sustantivo_g([nutribot|S],S).
sustantivo_g([ejercicio|S],S).
sustantivo_g([_,_|S],S).

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
verbo([perder|S],S).
verbo([hago|S],S).
verbo([bajar|S],S).  /** Added "bajar" **/
verbo([gustaria|S],S).  /** Added "bajar" **/
verbo([quiero,bajar|S],S).  /** Added "bajar" **/
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
preposicional([de|S],S).
preposicional([de,peso|S],S).  /** Added "de peso" **/

/** números y frecuencias */
numero([N|S],S) :- number(N), !.
numero([N|S],S) :- atom_number(N, _), !. % Allow for atomic representation of numbers.
frecuencia([veces|S],S).
frecuencia([dias|S],S).
frecuencia([semanas|S],S).
frec([_,_|S],S).

/** manejo de oraciones */
oracion(A,B):- sintagma_nominal(A,C), sintagma_verbal(C,B), !.
oracion(A,B):- sintagma_verbal(A,B), !.
oracion(A,B):- start(A), B = [].

/** negaciones */
oracion(A,B):- negativo(A,C), sintagma_verbal(C,B), !.
oracion(A,B):- negativo(A,C), verbo(C,D), preposicional(D,E), sustantivo_g(E,B), !.

/** preguntas */
pregunta(A,B):- pronombre_objeto(A,C), verbo(C,D), preposicional(D,E), sintagma_nominal(E,B), !.
pregunta(A,B):- pronombre_objeto(A,C), verbo(C,D), sustantivo_g(D,B), !.
pregunta(A,B):- verbo_invertido(A,C), sintagma_nominal(C,B), !.
pregunta(A,B):- verbo_invertido(A,C), sintagma_verbal(C,B), !.

/** oraciones compuestas */
oracion_compuesta(A,B):- oracion(A,C), [','|C], oracion(C,B), !.
oracion_compuesta(A,B):- oracion_simple(A,B), !.

/** oraciones simples */
oracion_simple([], []).
oracion_simple([Word|S],S) :- sustantivo_g([Word|S],S), !.
oracion_simple([Word|S],S) :- verbo([Word|S],S), !.
oracion_simple([Word|S],S) :- preposicional([Word|S],S), !.
oracion_simple([Word|S],S) :- numero([Word|S],S), !.

/** sintagmas nominales */
sintagma_nominal(A,B):- determinante(A,C), sustantivo_g(C,B), !.
sintagma_nominal(A,B):- sustantivo_g(A,B), !.
sintagma_nominal(A,B):- numero(A,C), sustantivo_g(C,B), !.
sintagma_nominal(A,B):- numero(A,C), preposicional(C,D), sustantivo_g(D,B), !.

/** sintagmas verbales */
sintagma_verbal(A,B):- verbo(A,C), sintagma_nominal(C,B), !.
sintagma_verbal(A,B):- verbo(A,C), preposicional(C,D), sintagma_nominal(D,B), !.
sintagma_verbal(A,B):- verbo(A,C), preposicional(C,D), sustantivo_g(D,B), !.  /** Added this rule for "bajar de peso" **/
sintagma_verbal(A,B):- verbo(A,C), numero(C,D), sustantivo_g(D,B), !.  /** Support for numbers after verbs **/
sintagma_verbal(A,B):- verbo(A,C), numero(C,D), sustantivo_g(D,B), !.
sintagma_verbal(A,B):- numero(A,C), sustantivo_g(C,B), !.
sintagma_verbal(A,B):- verbo(A,C), sustantivo_g(C,D), frecuencia(D,B), !.
sintagma_verbal(A,B):- verbo(A,C), sustantivo_g(C,D), numero(D,E), frecuencia(E,F), preposicional(F,G), sustantivo_g(G,B), !.

/** preguntas y órdenes */
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
/** validación gramatical con parámetro de salida */
validacion_gramatical(Oracion, 'valido') :- oracion(Oracion, []), !.
validacion_gramatical(Oracion, 'valido') :- pregunta(Oracion, []), !.
validacion_gramatical(Oracion, 'valido') :- orden(Oracion, []), !.
validacion_gramatical(_, 'Oración gramaticalmente incorrecta') :- !.
