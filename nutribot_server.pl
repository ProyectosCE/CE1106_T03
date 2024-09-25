:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

% Importar el chatbot desde el archivo nutribot.pl
:- consult('nutribot.pl').

% Iniciar el servidor
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% Ruta para manejar las consultas (solo POST)
:- http_handler(root(chat), handle_chat_request, [method(post)]).

% Manejo de la solicitud HTTP con soporte UTF-8
handle_chat_request(Request) :-
    http_read_json_dict(Request, QueryDict),  % Leer JSON de la solicitud
    Query = QueryDict.get(query),             % Obtener la consulta desde el JSON
    process_query(Query, Response),           % Procesar la consulta con el chatbot
    format('Content-type: application/json; charset=UTF-8~n~n'),  % Asegurar que la respuesta esté en UTF-8
    reply_json_dict(_{response: Response}).   % Devolver respuesta en formato JSON UTF-8

% Procesar la consulta con el chatbot (nutribot.pl)
process_query(Input, Response) :-
    normalize_input(Input, Words),
    (   find_matching_theme(Words, Theme)
    ->  theme_response(Theme, Response)
    ;   atomic_list_concat(Words, ' ', InputStr),
        respond(InputStr, Response)
    ).

% Para iniciar el servidor, llama a server(8080).
