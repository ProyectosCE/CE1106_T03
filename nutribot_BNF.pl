/** NutriBot BNF */

:-style_check(-singleton).
:- set_prolog_flag(double_quotes, chars).

/** inicio */
inicio([hola]).
inicio([iniciar]).
inicio([buenas]).
inicio([buenos]).
inicio([nutribot]).
inicio([nutritec]).

/** final */
final([gracias]).
final([muchas,gracias]).
final([chao]).
final([adios]).

/** negacion */
negacion([no|S],S).
negacion([nunca|S],S).
negacion([jamas|S],S).
negacion([nada|S],S).

/** afirmacion */
afirmacion([si|S],S).
afirmacion([claro|S],S).
afirmacion([por,supuesto|S],S).
afirmacion([definitivamente|S],S).

/** determinante */
determinante([yo|S],S).
determinante([el|S],S).
determinante([la|S],S).
determinante([los|S],S).
determinante([las|S],S).
determinante([mi|S],S).
determinante([tu|S],S).
determinante([su|S],S).
determinante([un|S],S).
determinante([una|S],S).

/** sustantivo_g */
sustantivo_g([colesterol|S],S).
sustantivo_g([sangre|S],S).
sustantivo_g([ejercicio|S],S).
sustantivo_g([problemas|S],S).
sustantivo_g([persona|S],S).
sustantivo_g([dieta|S],S).
sustantivo_g([salud|S],S).
sustantivo_g([peso|S],S).
sustantivo_g([alimentacion|S],S).
sustantivo_g([vida|S],S).
sustantivo_g([mariscos|S],S).
sustantivo_g([vegano|S],S).
sustantivo_g([queto|S],S).
sustantivo_g([rutina|S],S).
sustantivo_g([casa|S],S).
sustantivo_g([gimnasio|S],S).
sustantivo_g([_,_|S],S).  % Salta sobre sustantivos no reconocidos

/** verbo */
verbo([deseo|S],S).
verbo([tengo|S],S).
verbo([gustaria|S],S).
verbo([perder|S],S).
verbo([bajar|S],S).  % Maneja 'bajar de peso'
verbo([controlando|S],S).
verbo([controlo|S],S).
verbo([hago|S],S).
verbo([realizo|S],S).
verbo([practico|S],S).
verbo([estas|S],S).  % Maneja "como estas"
verbo([prefiero|S],S).
verbo([recomendarme|S],S).
verbo([seleccionar|S],S).
verbo([escuchado|S],S).
verbo([puedes|S],S).
verbo([corro|S],S).
verbo([voy|S],S).
verbo([_,_|S],S).

/** adverbios */
adverbio([bastante|S],S).  % Maneja "bastante"
adverbio([mucho|S],S).     % Maneja "mucho"
adverbio([poco|S],S).      % Maneja "poco"
adverbio(S,S).             % Permite flexibilidad si no hay adverbio

/** modificadores */
modificador([al,menos|S],S).  % Maneja "al menos"
modificador([mas,de|S],S).     % Maneja "más de"
modificador([menos,de|S],S).   % Maneja "menos de"
modificador(S,S).

/** frase_preposicional */
preposicion([en|S],S).
preposicion([por|S],S).
preposicion([a|S],S).
preposicion([de|S],S).  % Maneja "bajar de peso"

frase_preposicional([por,semana|S],S).
frase_preposicional([a,la,semana|S],S).  % Maneja "a la semana"
frase_preposicional([en,sangre|S],S).
frase_preposicional([de,peso|S],S).

/** numeros y frecuencia */
numero([N|S],S) :- atom_number(N,_).  % Reconoce números como "5", "3"
frecuencia([veces|S],S).
frecuencia([dias|S],S).
frecuencia([semanas|S],S).

/** pronombres de objeto */
pronombre_objeto([me|S],S).
pronombre_objeto([te|S],S).
pronombre_objeto([lo|S],S).
pronombre_objeto([la|S],S).
pronombre_objeto([nos|S],S).
pronombre_objeto([les|S],S).

/** preguntas */
verbo_invertido([es|S],S).
verbo_invertido([debo|S],S).
verbo_invertido([puedo|S],S).
verbo_invertido([estoy|S],S).
verbo_invertido([puedes|S],S).

pregunta(A,B):- verbo_invertido(A,C), sintagma_nominal(C,B), !.
pregunta(A,B):- verbo_invertido(A,C), preposicion(C,D), sustantivo_g(D,B), !.
pregunta(A,B):- verbo_invertido(A,C), pronombre_objeto(C,D), verbo(D,E), sintagma_nominal(E,B), !.  % Maneja preguntas como "¿me puedes ayudar?"

/** ordenes */
imperativo([come|S],S).
imperativo([evita|S],S).
imperativo([añade|S],S).
imperativo([reduce|S],S).
imperativo([consulta|S],S).
imperativo([selecciona|S],S).

orden(A,B):- imperativo(A,C), sintagma_nominal(C,B), !.
orden(A,B):- imperativo(A,C), preposicion(C,D), sustantivo_g(D,B), !.

/** inicio con oracion */
inicio_oracion(A,B):- inicio(A,C), [','|C], oracion(C,B).
inicio_oracion(A,B):- inicio(A,C), [','|C], pregunta(C,B).
inicio_oracion(A,B):- inicio(A,C), [','|C], orden(C,B).

/** oracion */
oracion(A,B):- append(A, ['.'], C), oracion(C,B), !.  % Maneja el punto final
oracion(A,B):- inicio(A,B), !.  % Reconoce saludos simples
oracion(A,B):- sintagma_nominal(A,C), sintagma_verbal(C,B), !.
oracion(A,B):- sintagma_verbal(A,B), !.
oracion(A,B):- oracion_compuesta(A,B), !.

/** oracion_compuesta */
oracion_compuesta(A,B):- oracion(A,C), [','|C], oracion(C,B).
oracion_compuesta(A,B):- oracion(A,C), ['pero'|C], oracion(C,B).
oracion_compuesta(A,B):- oracion(A,C), ['y'|C], oracion(C,B).
oracion_compuesta(A,B):- oracion(A,C), ['o'|C], oracion(C,B).

/** sintagma_nominal */
sintagma_nominal(A,B):- determinante(A,C), sustantivo_g(C,B).
sintagma_nominal(A,B):- sustantivo_g(A,B).

/** sintagma_verbal */
sintagma_verbal(A,B):- verbo(A,C), adverbio(C,D), sintagma_nominal(D,B).
sintagma_verbal(A,B):- verbo(A,C), modificador(C,D), numero(D,E), frecuencia(E,F), frase_preposicional(F,B).
sintagma_verbal(A,B):- verbo(A,C), preposicion(C,D), sustantivo_g(D,B).  % Maneja "bajar de peso"

/** sintagma_verbal con negacion */
sintagma_verbal(A,B):- negacion(A,C), verbo(C,D), sintagma_nominal(D,B), !.
sintagma_verbal(A,B):- negacion(A,C), verbo(C,D), preposicion(C,E), sustantivo_g(E,B), !.

/** validacion_gramatical */
validacion_gramatical(Oracion):- oracion(Oracion,[]), !.
validacion_gramatical(Oracion):- pregunta(Oracion,[]), !.
validacion_gramatical(Oracion):- orden(Oracion,[]), !.
validacion_gramatical(Oracion):- inicio_oracion(Oracion,[]), !.
validacion_gramatical(Oracion):- oracion_compuesta(Oracion,[]), !.
validacion_gramatical(Oracion):- nl, writeln('Oracion gramaticalmente incorrecta'), writeln('Escriba de nuevo su oracion'), nl, validacion_gramatical(_).
