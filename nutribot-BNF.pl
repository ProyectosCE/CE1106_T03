%Palabras clave de usuario

:-style_check(-singleton).

% inicio de la conversación
start([hola]).
start([iniciar]).
start([buenas]).
start([buenos]).
start([nutritec]).

%Finalizar la conversación
final([gracias]).
final([muchas,gracias]).
final([chao]).
final([adios]).

%Respuestas negativas
negative([no|S],S).
negative(['No'|S],S).
negative([nunca|S],S).
negative(['Nunca'|S],S).
negative([jamas|S],S).
negative(['Jamas'|S],S).
negative([nada|S],S).
negative(['Nada'|S],S).


%Respuestas positivas
positive([si|S],S).
positive([claro|S],S).

% Determinantes
determinante([yo|S],S).
determinante(['Yo'|S],S).

% Sustantivo
sustantivo_g([_|S],S).

% Verbos conjugados posibles
verb([deseo|S],S).
verb([tengo|S],S).
verb([gustaria|S],S).
verb([pensado|S],S).
verb([llevar|S],S).
verb([estoy|S],S).
verb([diagnosticado|S],S).
verb([habia|S],S).
verb([realizar|S],S).
verb([quiero|S],S).
verb([hago|S],S).
verb([realizo|S],S).
verb([me,gustaria|S],S).
verb([me,diagnosticaron|S],S).
verb([deseo,llevar|S],S).

verb(['Deseo'|S],S).
verb(['Tengo'|S],S).
verb(['Gustaria'|S],S).
verb(['Pensado'|S],S).
verb(['Llevar'|S],S).
verb(['Estoy'|S],S).
verb(['Diagnosticado'|S],S).
verb(['Habia'|S],S).
verb(['Realizar'|S],S).
verb(['Quiero'|S],S).
verb(['Hago'|S],S).
verb(['Realizo'|S],S).

verb(['Me','gustan'|S],S).
verb(['Me','gustaria'|S],S).
verb(['Me','diagnosticaron'|S],S).
verb(['Deseo','llevar'|S],S).


% Verifica si es una oración gramaticalmente 
oracion(A,B):-
    sintagma_nominal(A,C).


% Recibe una lista de palabras y una lista vacía; elimina el primer sintagma nominal encontrado
sintagma_nominal(A,B):-
    determinante(A,C),
    sintagma_verbal(C,Z),
	sustantivo_g(Z,B).

sintagma_nominal(A,B):-
	sintagma_verbal(A,C),
    sustantivo_g(C,B).

sintagma_nominal(A,B):-
    sintagma_verbal(A,B).


% Recibe una lista de palabras y una lista vacía; elimina el primer sintagma verbal encontrado
sintagma_verbal(A,B):-
	verb(A,B).

% Valida si la oración digitada por el usuario está gramaticalmente correcta según el BNF establecido

validacion_gramatical(Oracion):-
    oracion(Oracion,[]),
	!.

validacion_gramatical(Oracion):-
	nl, writeln('Oracion gramaticalmente incorrecta'),
	writeln('Escriba de nuevo su oracion'),nl,
	
	validacion_gramatical(Oracion2).