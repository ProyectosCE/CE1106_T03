/* 
================================== LICENCIA ================================================== 
MIT License
Copyright (c) 2024 José Bernardo Barquero Bonilla,
                   Jose Eduardo Campos Salazar,
                   Jimmy Feng Feng,
                   Alexander Montero Vargas
Consulta el archivo LICENSE para más detalles.
==============================================================================================
*/

/* 
==================================REFERENCIAS================================================
Para este archivo se tomaron como referencia general las siguientes fuentes:

 * Documentación propia de Prolog: https://www.swi-prolog.org/
* Interfaz basada en la interfaz del proyecto "NutriTEC" del usuario de GitHub @johnnyzaet08.
  Puede acceder a su archivo en la dirección:
  https://github.com/johnnyzaet08/NutriTEC

 ===========================================================================================
*/





/**
 * NutriBot BNF Modificado: Handles grammatical rules and sentence structure for processing user inputs.
 */

:-style_check(-singleton).
:- set_prolog_flag(double_quotes, chars).

:- discontiguous oracion/2.
:- discontiguous pregunta/2.

/**
 * @param Words List of words used in greetings.
 */
start([hola]).
start([gracias]).
start([buenas]).
start([adios]).
start([chao]).
start([hola, nutribot]).

/**
 * @param Words List of words used in farewells.
 */
final([gracias]).
final([muchas, gracias]).
final([chao]).
final([adios]).

/**
 * Handles negation in user input.
 * @param Words List of words representing negation.
 * @param Rest Remaining words after processing negation.
 */
negativo([no|S], S).
negativo([nunca|S], S).
negativo([jamas|S], S).
negativo([nada|S], S).

/**
 * Handles affirmation in user input.
 * @param Words List of words representing affirmation.
 * @param Rest Remaining words after processing affirmation.
 */
afirmativo([si|S], S).
afirmativo([claro|S], S).
afirmativo([por, supuesto|S], S).
afirmativo([definitivamente|S], S).

/**
 * Handles determiners and pronouns in user input.
 * @param Words List of words representing determiners.
 * @param Rest Remaining words after processing determiners.
 */
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

/**
 * Handles subject determiners that should not be followed by a noun.
 * @param Words List of words representing subject determiners.
 * @param Rest Remaining words after processing.
 */
subject_determiner([yo|S], S).
subject_determiner([tu|S], S).
subject_determiner([ella|S], S).
subject_determiner([nosotros|S], S).
subject_determiner([ellas|S], S).
subject_determiner([ellos|S], S).
subject_determiner([usted|S], S).
subject_determiner([ustedes|S], S).

/**
 * Handles common nouns related to health and diet.
 * @param Words List of words representing nouns.
 * @param Rest Remaining words after processing.
 */
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

/**
 * Handles common verbs.
 * @param Words List of words representing verbs.
 * @param Rest Remaining words after processing.
 */
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

% Verbos con gerundio
verbo_gerundio([controlando|S], S).

/**
 * Handles adverbs.
 * @param Words List of words representing adverbs.
 * @param Rest Remaining words after processing.
 */
adverbio([mucho|S], S).
adverbio([poco|S], S).
adverbio(S, S).  % Accepts any other sequence as an adverb by default.

/**
 * Handles prepositions.
 * @param Words List of words representing prepositions.
 * @param Rest Remaining words after processing.
 */
preposicional([en|S], S).
preposicional([por|S], S).
preposicional([a|S], S).
preposicional([a,la|S], S).
preposicional([de|S], S).
preposicional([con|S], S).
preposicional([la|S], S).
preposicional([el|S], S).

/**
 * Handles numbers and frequency-related words.
 * @param Words List of words representing numbers or frequencies.
 * @param Rest Remaining words after processing.
 */
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

/**
 * Handles simple sentences (verb + noun or verb + verb).
 * @param A List of words starting with a verb.
 * @param B Remaining words after processing.
 */
oracion_simple(A, B) :- verbo(A, C), sustantivo_g(C, B), !.
oracion_simple(A, B) :- verbo(A, C), preposicional(C, D), sustantivo_g(D, B), !.
oracion_simple(A, B) :- verbo_gerundio(A, C), sustantivo_g(C, B), !.
oracion_simple(A, B) :- verbo(A, C), verbo_gerundio(C, B), !.

/**
 * Handles noun phrases (determiner + noun).
 * @param A List of words starting with a determiner.
 * @param B Remaining words after processing.
 */
sintagma_nominal(A, B) :- determinante(A, C), sustantivo_g(C, B), !.
sintagma_nominal(A, B) :- sustantivo_g(A, B), !.
sintagma_nominal(A, B) :- numero(A, C), sustantivo_g(C, B), !.

/**
 * Handles verb phrases (verb + noun phrase).
 * @param A List of words starting with a verb.
 * @param B Remaining words after processing.
 */
sintagma_verbal(A, B) :- verbo(A, C), sintagma_nominal(C, B), !.
sintagma_verbal(A, B) :- verbo(A, C), preposicional(C, D), sintagma_nominal(D, B), !.
sintagma_verbal(A, B) :- verbo(A, C), numero(C, D), frecuencia(D, B), !.  % Adjusted to handle "5 veces"
sintagma_verbal(A, B) :- verbo(A, C), sustantivo_g(C,D),numero(D, E), frecuencia(E, B), !.
sintagma_verbal(A, B) :- verbo(A, C), sustantivo_g(C, D), frecuencia(D, B), !.
sintagma_verbal(A, B) :- verbo_gerundio(A, C), preposicional(C, D), sustantivo_g(D, B), !.
sintagma_verbal(A, B) :- verbo(A, C), sustantivo_g(C, D), numero(D, E), frecuencia(E, F), preposicional(F, B), !.


/**
 * Handles user queries in the form of a question.
 * @param A List of words starting with a pronoun or verb.
 * @param B Remaining words after processing.
 */
pregunta(A, B) :- pronombre_objeto(A, C), verbo(C, D), sustantivo_g(D, B), !.
pregunta(A, B) :- verbo_invertido(A, C), sintagma_nominal(C, B), !.
pregunta(A, B) :- verbo_invertido(A, C), sintagma_verbal(C, B), !.

% Pronombres para preguntas
% @param [me|S] Representa el pronombre objeto "me" y el resto de la oración S.
% @param [te|S] Representa el pronombre objeto "te" y el resto de la oración S.
pronombre_objeto([me|S], S).
pronombre_objeto([te|S], S).

% Órdenes
% @param [come|S] Representa la orden "come" (imperativo de comer) y el resto de la oración S.
% @param [evita|S] Representa la orden "evita" (imperativo de evitar) y el resto de la oración S.
imperativo([come|S], S).
imperativo([evita|S], S).

% Órdenes simples (imperativo + sustantivo)
% @param A La lista que contiene la orden.
% @param B La lista resultante tras procesar el imperativo seguido de un sustantivo.
orden(A, B) :- imperativo(A, C), sintagma_nominal(C, B), !.

% Verbos invertidos para preguntas
% @param [puedes|S] Representa el verbo invertido "puedes" en una pregunta y el resto de la oración S.
% @param [deberia|S] Representa el verbo invertido "debería" en una pregunta y el resto de la oración S.
% @param [es|S] Representa el verbo invertido "es" en una pregunta y el resto de la oración S.
verbo_invertido([puedes|S], S).
verbo_invertido([deberia|S], S).
verbo_invertido([es|S], S).

% Restricción de combinaciones de sustantivos inválidos (ej. "yo ejercicio no")
% @param [Determinante|S] Verifica si la combinación de un determinante seguido de un sustantivo es inválida.
invalid_combination([Determinante|S]) :-
    determinante([Determinante|Rest], []),  % Verifica que el primer elemento sea un determinante válido.
    sustantivo_g(S, _).  % Verifica si el resto de la frase comienza con un sustantivo, lo cual sería inválido.

invalid_combination([Determinante|S]) :-
    subject_determiner([Determinante|Rest], []),  % Verifica si es un determinante de sujeto.
    sustantivo_g(S, _).  % Verifica si está seguido por un sustantivo.

invalid_combination([Sustantivo|Rest]) :-
    sustantivo_g([Sustantivo|_], []),  % Verifica que el primer elemento sea un sustantivo válido.
    subject_determiner(Rest, _).  % Verifica si está seguido por un determinante de sujeto.

% Estructura principal de oración (Sujeto + Verbo + Objeto)
% @param A La lista de palabras que forman la oración.
% @param B El resto de la oración tras procesar el sujeto, verbo y objeto.
oracion(A, B) :- sintagma_nominal(A, C), sintagma_verbal(C, B), !.

% Prevención de combinaciones inválidas
% @param A La lista de palabras a evaluar.
oracion(A, _) :- invalid_combination(A), !, fail.

% Emparejamiento flexible de oraciones
% @param A La lista de palabras de la oración.
% @param B El resto de la oración tras procesar de manera flexible sustantivos, verbos y preposiciones.
oracion_flexible(A, B) :- verbo(A, C), !, oracion_flexible(C, B).
oracion_flexible(A, B) :- preposicional(A, C), !, oracion_flexible(C, B).
oracion_flexible(A, B) :- sustantivo_g(A, B), !.

% Negación seguida de una frase verbal válida
% @param A La lista de palabras con una negación al inicio.
% @param B El resto de la oración tras procesar la negación y la frase verbal.
oracion(A, B) :- negativo(A, C), sintagma_verbal(C, B), !.
oracion(A, B) :- oracion_simple(A, B), !.
oracion(A, B) :- oracion_flexible(A, B), !.

/**
 * Validates user input and categorizes it into sentence structures.
 * @param Oracion The sentence input.
 * @param Validation The validation result.
 */
validacion_gramatical(Oracion, 'valido') :- oracion(Oracion, []), !.
validacion_gramatical(Oracion, 'valido') :- pregunta(Oracion, []), !.
validacion_gramatical(Oracion, 'valido') :- orden(Oracion, []), !.
validacion_gramatical(_, 'Oración gramaticalmente incorrecta') :- !.
