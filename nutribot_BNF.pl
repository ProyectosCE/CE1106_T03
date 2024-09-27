/** NutriBot BNF */

:-style_check(-singleton).
:- set_prolog_flag(double_quotes, chars).

/** start */
start([hola]).
start([iniciar]).
start([buenas]).
start([buenos]).
start([nutritec]).

/** final */
final([gracias]).
final([muchas,gracias]).
final([chao]).
final([adios]).

/** negative */
negative([no|S],S).
negative([nunca|S],S).
negative([jamas|S],S).
negative([nada|S],S).

/** positive */
positive([si|S],S).
positive([claro|S],S).
positive([por,supuesto|S],S).
positive([definitivamente|S],S).

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
sustantivo_g([_,_|S],S).  % Skips over unrecognized nouns

/** verb */
verb([deseo|S],S).
verb([tengo|S],S).
verb([gustaria|S],S).
verb([perder|S],S).
verb([bajar|S],S).  % Handles 'bajar de peso'
verb([controlando|S],S).
verb([controlo|S],S).
verb([hago|S],S).
verb([realizo|S],S).
verb([practico|S],S).
verb([estas|S],S).  % Handles "como estas"
verb([_,_|S],S).

/** adverbs */
adverb([bastante|S],S).  % Handles "bastante" (quite)
adverb([mucho|S],S).     % Handles "mucho" (a lot)
adverb([poco|S],S).      % Handles "poco" (a little)
adverb(S,S).             % Allows flexibility if no adverb is present

/** prepositional phrases and modifiers */
modifier([al,menos|S],S).  % Handles "al menos"
modifier([mas,de|S],S).     % Handles "más de"
modifier([menos,de|S],S).   % Handles "menos de"
modifier(S,S).

/** prepositional phrase */
prepositional([en|S],S).
prepositional([por|S],S).
prepositional([a|S],S).
prepositional([de|S],S).  % Handles "bajar de peso" (lose weight)

prepositional_phrase([por,semana|S],S).
prepositional_phrase([a,la,semana|S],S).  % Handles "a la semana"
prepositional_phrase([en,sangre|S],S).
prepositional_phrase([de,peso|S],S).

/** number and frequency */
number([N|S],S) :- atom_number(N,_).  % Recognizes numbers like "5", "3"
frequency([veces|S],S).
frequency([dias|S],S).
frequency([semanas|S],S).

/** oracion */
oracion(A,B):- sintagma_nominal(A,C), sintagma_verbal(C,B), !.
oracion(A,B):- sintagma_verbal(A,B), !.
oracion(A,B):- oracion_compuesta(A,B), !.  % Handles compound sentences

/** compound sentence */
oracion_compuesta(A,B):- oracion(A,C), [','|C], oracion(C,B).  % Handle sentences with commas (e.g., "hago bastante, al menos 5 veces a la semana")

/** sintagma_nominal */
sintagma_nominal(A,B):- determinante(A,C), sustantivo_g(C,B).
sintagma_nominal(A,B):- sustantivo_g(A,B).

/** sintagma_verbal */
sintagma_verbal(A,B):- verb(A,C), adverb(C,D), sintagma_nominal(D,B).
sintagma_verbal(A,B):- verb(A,C), modifier(C,D), number(D,E), frequency(E,F), prepositional_phrase(F,B).
sintagma_verbal(A,B):- verb(A,C), prepositional(C,D), sustantivo_g(D,B).  % Handles "bajar de peso"

/** validacion_gramatical */
validacion_gramatical(Oracion):- oracion(Oracion,[]), !.
validacion_gramatical(Oracion):- nl, writeln('Oracion gramaticalmente incorrecta'), writeln('Escriba de nuevo su oracion'), nl, validacion_gramatical(_).