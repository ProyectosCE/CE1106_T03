/** servidor_http
 *  
 *  Modulo Prolog que implementa un servidor HTTP para interactuar con el chatbot NutriBot.
 *  @author Alexander Montero Vargas
 * 
 *  Este modulo define un servidor HTTP utilizando las bibliotecas de SWI-Prolog que permite la interacción
 *  con el chatbot definido en el archivo `nutribot.pl`. El servidor responde a solicitudes HTTP POST y OPTIONS,
 *  procesando consultas en formato JSON y respondiendo con las respuestas generadas por el chatbot.
 *  
 */

:- use_module(library(http/thread_httpd)).   
:- use_module(library(http/http_dispatch)).  
:- use_module(library(http/http_json)).      
:- set_prolog_flag(encoding, utf8).          

/** consult
 *  
 *  Importa el modulo del chatbot desde el archivo `nutribot.pl`.
 *  @param Archivo El archivo que contiene la implementacion del chatbot.
 */
:- consult('nutribot.pl').  /** Importa el chatbot desde 'nutribot.pl' */

/** server
 *  
 *  Inicia el servidor HTTP en el puerto especificado.
 *  @param Port El puerto en el que se iniciara el servidor.
 *  
 *  Esta predicado inicia un servidor HTTP en el puerto proporcionado, utilizando `http_server/2`
 *  y el manejador `http_dispatch`.
 */
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

/** http_handler(root(chat), handle_chat_request, [])
 *  
 *  Define una ruta HTTP para manejar las consultas al chatbot.
 *  
 *  Esta linea define un manejador HTTP para la ruta `/chat`, que invoca el predicado `handle_chat_request`.
 *  Maneja tanto solicitudes POST como OPTIONS.
 */
:- http_handler(root(chat), handle_chat_request, []). 

/** handle_chat_request
 *  
 *  Maneja las solicitudes HTTP y decide si procesarlas como POST o OPTIONS.
 *  @param Request La solicitud HTTP.
 *  
 *  Este predicado actua como enrutador para manejar solicitudes HTTP POST y OPTIONS. Para cada
 *  tipo de metodo HTTP, delega a `handle_options/2` para su respectivo manejo.
 */
handle_chat_request(Request) :-
    memberchk(method(Options), Request), 
    handle_options(Options, Request).     

/** handle_options
 *  
 *  Maneja solicitudes POST y OPTIONS.
 *  @param Method El metodo HTTP (post u options).
 *  @param Request La solicitud HTTP.
 *  
 *  Este predicado procesa las solicitudes HTTP POST y OPTIONS:
 *  - Para POST, lee el JSON del cuerpo de la solicitud, extrae la consulta y la procesa con el chatbot.
 *  - Para OPTIONS, habilita CORS y responde con "OK".
 */
handle_options(post, Request) :-
    !, 
    cors_enable,  
    http_read_json_dict(Request, QueryDict),  
    Query = QueryDict.get(query),  
    process_query(Query, Response),  
    reply_json_dict(_{response: Response}, [json_object(dict)]).  

handle_options(options, _Request) :-
    !,  
    cors_enable,  
    format('Content-type: text/plain~n~n'),  
    format('OK').

/** cors_enable
 *  
 *  Habilita CORS en la respuesta HTTP.
 *  
 *  Este predicado escribe los encabezados necesarios para habilitar CORS
 *  en la respuesta, permitiendo que la API sea consumida desde cualquier origen.
 */
cors_enable :-
    format('Access-Control-Allow-Origin: *~n'),  /** Permite solicitudes desde cualquier origen */
    format('Access-Control-Allow-Methods: POST, OPTIONS~n'),  /** Permite los métodos POST y OPTIONS */
    format('Access-Control-Allow-Headers: Content-Type~n').  /** Permite el encabezado Content-Type */

/** process_query
 *  
 *  Procesa una consulta enviada al chatbot.
 *  @param Input La consulta del usuario en formato de texto.
 *  @param Response La respuesta generada por el chatbot.
 *  
 *  Este predicado toma la consulta del usuario, la normaliza dividiendola en palabras,
 *  e intenta encontrar un tema relacionado. Si se encuentra un tema, genera una respuesta
 *  basada en ese tema. De lo contrario, responde con una cadena concatenada de las palabras.
 */
process_query(Input, Response) :-
    normalize_input(Input, Words),  
    (   find_matching_theme(Words, Theme)  
    ->  theme_response(Theme, Response)  
    ;   atomic_list_concat(Words, ' ', InputStr),  
        respond(InputStr, Response)  
    ).
    