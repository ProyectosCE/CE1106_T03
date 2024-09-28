:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- set_prolog_flag(encoding, utf8).

% Consultar el archivo nutribot.pl
:- consult('nutribot.pl').

% Iniciar el servidor
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% Definir un manejador HTTP para las solicitudes en /nutribot
:- http_handler(root(nutribot), handle_nutribot_request, []).

% Manejar solicitudes POST
handle_nutribot_request(Request) :-
    catch(
        (
            % Leer el cuerpo JSON de la solicitud
            http_read_json_dict(Request, JSONIn),
            % Asegurarse de que el campo "query" está presente y es válido
            (   _{query: Input} :< JSONIn
            ->  atom_string(Input, InputText),
                % Procesar la entrada del chatbot
                process_chatbot_input(InputText, Response),
                % Proteger la escritura de la respuesta en el stream
                reply_safe_json(Response)
            ;   reply_safe_json("Lo siento, no entiendo tu pregunta.")
            )
        ),
        Error,
        handle_error(Error)
    ).

% Procesar la entrada del chatbot
process_chatbot_input(Input, Response) :-
    % Normalizar la entrada
    normalize_input(Input, Words),
    % Verificar si la entrada pasa la validación gramatical
    (   validacion_gramatical(Words)
    ->  % Si la validación es correcta, buscar el tema coincidente
        (   find_best_matching_theme(Words, Theme)
        ->  theme_response(Theme, Response)
        ;   Response = "Lo siento, no entiendo tu pregunta."  % Si no hay tema coincidente
        )
    ;   Response = "Lo siento, tu gramática no es correcta. Por favor intenta de nuevo."  % Si la gramática falla
    ).

% Devolver respuesta segura en JSON
reply_safe_json(Response) :-
    catch(
        (
            % Intentar enviar la respuesta como JSON
            reply_json_dict(_{response: Response}),
            flush_output  % Asegurar que el stream se vacíe correctamente
        ),
        _Error,
        % Si hay un error, manejarlo y devolver una respuesta simple
        reply_json_dict(_{code: 500, message: "Error interno del servidor."})
    ).

% Manejar errores
handle_error(Error) :-
    format(user_error, 'Error: ~w~n', [Error]),  % Imprimir el error en la consola
    % Devolver un error JSON al cliente
    reply_json_dict(_{code: 500, message: "Error en el procesamiento de la solicitud."}),
    flush_output.  % Asegurar que el stream se vacíe correctamente
