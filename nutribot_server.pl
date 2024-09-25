:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- set_prolog_flag(encoding, utf8).  % Asegura que todo el sistema use UTF-8

% Importar el chatbot desde el archivo nutribot.pl
:- consult('nutribot.pl').

% Iniciar el servidor
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% Ruta para manejar las consultas (POST y OPTIONS)
:- http_handler(root(chat), handle_chat_request, []).

% Manejo de la solicitud HTTP con soporte UTF-8 y CORS
handle_chat_request(Request) :-
    memberchk(method(Options), Request),     % Verificar el método de la solicitud
    handle_options(Options, Request).

% Manejar solicitudes POST y OPTIONS
handle_options(post, Request) :-
    !,  % Manejar la solicitud POST
    cors_enable(Request),
    http_read_json_dict(Request, QueryDict),  % Leer JSON de la solicitud
    Query = QueryDict.get(query),             % Obtener la consulta desde el JSON
    process_query(Query, Response),           % Procesar la consulta con el chatbot
    format('Content-type: application/json; charset=UTF-8~n~n'),  % Asegurar que la respuesta esté en UTF-8
    reply_json_dict(_{response: Response}, [json_object(dict)]).   % Enviar la respuesta correctamente en UTF-8

handle_options(options, Request) :-
    !,  % Manejar la solicitud OPTIONS
    cors_enable(Request),
    format('Content-type: text/plain~n~n'),
    format('OK').

% Habilitar CORS para las respuestas
cors_enable(_Request) :-
    format('Access-Control-Allow-Origin: *~n'),  % Permitir solicitudes desde cualquier origen
    format('Access-Control-Allow-Methods: POST, OPTIONS~n'),  % Permitir métodos POST y OPTIONS
    format('Access-Control-Allow-Headers: Content-Type~n').  % Permitir el encabezado Content-Type

% Procesar la consulta con el chatbot (nutribot.pl)
process_query(Input, Response) :-
    normalize_input(Input, Words),
    (   find_matching_theme(Words, Theme)
    ->  theme_response(Theme, Response)
    ;   atomic_list_concat(Words, ' ', InputStr),
        respond(InputStr, Response)
    ).

% Para iniciar el servidor, llama a server(8080).
