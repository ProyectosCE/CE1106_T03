:- use_module(iomodule).  % Importar el módulo intermedio
:- dynamic user/2.

% Comienzo del chatbot
comienzo :-
    write('Bienvenido al chatbot. Escribe "adios" para terminar.'), nl,
    chat.

% Llamada principal para iniciar el chat
chat :- 
    write('Tu: '), 
    flush_output, 
    read_line_to_string(user_input, InputRaw),  % Leer la entrada del usuario
    call_iomodule(InputRaw, Response),  % Llamar al módulo I/O para procesar la entrada
    write('Chatbot: '), write(Response), nl,  % Imprimir la respuesta del chatbot
    (InputRaw == "adios" -> write('Chatbot: ¡Hasta luego!'), nl ; chat).  % Terminar el chat si el usuario dice 'adios'

% Llamar a la función de iomodule para procesar la entrada y devolver la respuesta
call_iomodule(InputRaw, Response) :-
    iomodule:process_user_input(InputRaw, Response).  % Delegar el procesamiento a iomodule
