% Define las respuestas a diversas entradas
respond('hola', 'Hola, como puedo ayudarte hoy?').
respond('como estas', 'Estoy bien, gracias. Y tu?').
respond('cual es tu nombre', 'Soy nutrichat sin nombre. Como te llamas tu?').
respond('adios', '¡Hasta luego!').

% Respuesta por defecto si la entrada no es reconocida
respond(_, 'Lo siento, no entiendo tu pregunta.').

% Eliminar puntos y espacios adicionales al principio y al final de la entrada
normalize_input(Input, Normalized) :-
    % hace toda la entrada minuscula
    string_lower(Input, Lowered),
    % crea una lista separada por espacios
    split_string(Lowered, " ", "", Parts),
    % quita puntos y signos de pergunda
    remove_trailing_period(Parts, TrimmedParts),
    % vuelve a juntar todo pero clean
    atomic_list_concat(TrimmedParts, ' ', Normalized).

remove_trailing_period([Last], []) :-  
    sub_string(Last, _, 1, 0, '.').
remove_trailing_period(List, List).

% Bucle principal de interaccion
chat :-
    write('Tu: '),
    % tira lo que tiene en cola
    flush_output, 
    % esto lee la entrada y la convierte en un string
    read_line_to_string(user_input, InputRaw),
    % esto hace varias cosas, vamos a la funcion
    normalize_input(InputRaw, Input),
    (   Input == 'adios'
    ->  write('Chatbot: ¡Hasta luego!'), nl
    ;   respond(Input, Response),
        write('Chatbot: '), write(Response), nl,
        chat
    ).

% Punto de entrada
comienzo :-
    write('Bienvenido al chatbot. Escribe "adios" para terminar.'), nl,
    chat.
