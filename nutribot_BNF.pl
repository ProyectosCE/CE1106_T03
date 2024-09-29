/** NutriBot BNF Modificado */

:-style_check(-singleton).
:- set_prolog_flag(double_quotes, chars).

:- discontiguous oracion/2.
:- discontiguous pregunta/2.

% Interjecciones y saludos
start([hola]).
start([gracias]).
start([buenas]).
start([adios]).
start([chao]).
start([hola, nutribot]).

% Finalizaciones
final([gracias]).
final([muchas, gracias]).
final([chao]).
final([adios]).

% Negaciones
negativo([no|S], S).
negativo([nunca|S], S).
negativo([jamas|S], S).
negativo([nada|S], S).

% Afirmaciones
afirmativo([si|S], S).
afirmativo([claro|S], S).
afirmativo([por, supuesto|S], S).
afirmativo([definitivamente|S], S).

% Determinantes y pronombres
determinante([yo|S], S).
determinante([tu|S], S).
determinante([el|S], S).
determinante([ella|S], S).
determinante([nosotros|S], S).
determinante([un|S], S).
determinante([una|S], S).
determinante([unas|S], S).
determinante([unos|S], S).
determinante([mi|S], S).
determinante([las|S], S).
determinante([los|S], S).


% Subject determiners that shouldnt be followed by a noun
subject_determiner([yo|S], S).
subject_determiner([tu|S], S).
subject_determiner([ella|S], S).
subject_determiner([nosotros|S], S).
subject_determiner([ellas|S], S).
subject_determiner([ellos|S], S).
subject_determiner([usted|S], S).
subject_determiner([ustedes|S], S).




% Sustantivos relacionados con dietas y salud
sustantivo_g([arroz|S], S).
sustantivo_g([sobrepeso|S], S).
sustantivo_g([semana|S], S).
sustantivo_g([salud|S], S).
sustantivo_g([dieta|S], S).
sustantivo_g([ayuda|S], S).
sustantivo_g([calorias|S], S).
sustantivo_g([peso|S], S).
sustantivo_g([nutribot|S], S).
sustantivo_g([ejercicio|S], S).
sustantivo_g([colesterol|S], S).
sustantivo_g([problemas|S], S).
sustantivo_g([dislipidemia|S], S).
sustantivo_g([sangre|S], S).
sustantivo_g([control|S], S).
sustantivo_g([_,_|S], S).  % Any other sustantive.

% Verbos comunes
verbo([tengo|S], S).
verbo([puedo|S], S).
verbo([puede|S], S).
verbo([ayudar|S], S).
verbo([quiero|S], S).
verbo([necesito|S], S).
verbo([bajar|S], S).
verbo([comer|S], S).
verbo([evitar|S], S).
verbo([perder|S], S).
verbo([controlar|S], S).
verbo([hago|S], S).
verbo([gustaria|S], S).  % Added to handle "me gustaria".
verbo([_|S], S).  % Added to handle "me gustaria".

% Verbos de control con gerundio
verbo_gerundio([controlando|S], S).  % Added to handle "controlando el colesterol".

% Adverbios
adverbio([mucho|S], S).
adverbio([poco|S], S).
adverbio(S, S).  % Acepta cualquier otra secuencia como adverbio por defecto

% Preposicionales
preposicional([en|S], S).
preposicional([por|S], S).
preposicional([a|S], S).
preposicional([a,la|S], S).
preposicional([de|S], S).
preposicional([con|S], S).
preposicional([la|S], S).
preposicional([el|S], S).

% Números y frecuencias
numero([N|S], S) :- number(N), !.  % Already a number
numero([N|S], S) :- atom_number(N, _), !.  % Convert from atom to number
numero([5|S], S).
numero([3|S], S).
numero([_|S], S).

frecuencia([veces|S], S).
frecuencia([dias|S], S).
frecuencia([semanas|S], S).
frecuencia([a,la,semana|S], S).
frecuencia([_|S], S).

% Oraciones simples (verbo + sustantivo o verbo + verbo)
oracion_simple(A, B) :- verbo(A, C), sustantivo_g(C, B), !.
oracion_simple(A, B) :- verbo(A, C), preposicional(C, D), sustantivo_g(D, B), !.
oracion_simple(A, B) :- verbo_gerundio(A, C), sustantivo_g(C, B), !.
oracion_simple(A, B) :- verbo(A, C), verbo_gerundio(C, B), !.

% Sintagmas nominales (Noun phrases)
sintagma_nominal(A, B) :- determinante(A, C), sustantivo_g(C, B), !.
sintagma_nominal(A, B) :- sustantivo_g(A, B), !.
sintagma_nominal(A, B) :- numero(A, C), sustantivo_g(C, B), !.

% Sintagmas verbales (Verb phrases)
sintagma_verbal(A, B) :- verbo(A, C), sintagma_nominal(C, B), !.
sintagma_verbal(A, B) :- verbo(A, C), preposicional(C, D), sintagma_nominal(D, B), !.
sintagma_verbal(A, B) :- verbo(A, C), numero(C, D), frecuencia(D, B), !.  % Adjusted to handle "5 veces"
sintagma_verbal(A, B) :- verbo(A, C), sustantivo_g(C,D),numero(D, E), frecuencia(E, B), !.
sintagma_verbal(A, B) :- verbo(A, C), sustantivo_g(C, D), frecuencia(D, B), !.
sintagma_verbal(A, B) :- verbo_gerundio(A, C), preposicional(C, D), sustantivo_g(D, B), !.
sintagma_verbal(A, B) :- verbo(A, C), sustantivo_g(C, D), numero(D, E), frecuencia(E, F), preposicional(F, B), !.

% Manejo de preguntasz
pregunta(A, B) :- pronombre_objeto(A, C), verbo(C, D), sustantivo_g(D, B), !.
pregunta(A, B) :- verbo_invertido(A, C), sintagma_nominal(C, B), !.
pregunta(A, B) :- verbo_invertido(A, C), sintagma_verbal(C, B), !.

% Pronombres para preguntas
pronombre_objeto([me|S], S).
pronombre_objeto([te|S], S).

% Órdenes
imperativo([come|S], S).
imperativo([evita|S], S).

% Órdenes simples (imperativo + sustantivo)
orden(A, B) :- imperativo(A, C), sintagma_nominal(C, B), !.

% Verbos invertidos para preguntas
verbo_invertido([puedes|S], S).
verbo_invertido([deberia|S], S).
verbo_invertido([es|S], S).

% Restrict invalid noun-noun combinations (like "yo ejercicio no")
invalid_combination([Determinante|S]) :-
    determinante([Determinante|Rest], []),  % Ensure the first element is a valid determinante.
    sustantivo_g(S, _).  % Check if the rest of the phrase starts with a noun, which would be invalid.

invalid_combination([Determinante|S]) :-
    subject_determiner([Determinante|Rest], []),  % Check if its a subject determiner.
    sustantivo_g(S, _).  % Check if its followed by a noun.

% Main sentence structure (Subject + Verb + Object)
oracion(A, B) :- sintagma_nominal(A, C), sintagma_verbal(C, B), !.

% Prevent invalid combinations from being accepted
oracion(A, _) :- invalid_combination(A), !, fail.

% Flexible sentence matching
oracion_flexible(A, B) :- verbo(A, C), !, oracion_flexible(C, B).
oracion_flexible(A, B) :- preposicional(A, C), !, oracion_flexible(C, B).
oracion_flexible(A, B) :- sustantivo_g(A, B), !.

% Negation followed by a valid verb phrase
oracion(A, B) :- negativo(A, C), sintagma_verbal(C, B), !.
oracion(A, B) :- oracion_simple(A, B), !.
oracion(A, B) :- oracion_flexible(A, B), !.

% Validate grammar
validacion_gramatical(Oracion, 'valido') :- oracion(Oracion, []), !.
validacion_gramatical(Oracion, 'valido') :- pregunta(Oracion, []), !.
validacion_gramatical(Oracion, 'valido') :- orden(Oracion, []), !.
validacion_gramatical(_, 'Oración gramaticalmente incorrecta') :- !.
