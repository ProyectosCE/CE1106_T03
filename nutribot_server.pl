/** servidor_http
 *  
 *  Módulo Prolog que implementa un servidor HTTP para interactuar con el chatbot NutriBot.
 *  @author Alexander Montero Vargas
 */

:- use_module(library(http/thread_httpd)).   % Módulo para manejar servidores HTTP en Prolog
:- use_module(library(http/http_dispatch)).  % Módulo para manejar rutas HTTP
:- use_module(library(http/http_json)).      % Módulo para manejar JSON en las solicitudes/respuestas HTTP
:- use_module(library(http/http_header)).    % Módulo para manejar encabezados HTTP
:- set_prolog_flag(encoding, utf8).          % Asegurar que Prolog trabaje con UTF-8

/** consult
 *  
 *  Importa los módulos del chatbot desde los archivos `nutribot.pl` y `nutribot_BNF.pl`.
 */
:- consult('nutribot.pl').    % Importa el chatbot principal desde 'nutribot.pl'
:- consult('nutribot_BNF.pl').% Importa las reglas de gramática del chatbot desde 'nutribot_BNF.pl'

/** server
 *  
 *  Inicia el servidor HTTP en el puerto especificado.
 *  @param Port El puerto en el que se iniciará el servidor.
 */
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

/** http_handler(root(chat), handle_chat_request, [])
 *  
 *  Define una ruta HTTP para manejar las consultas al chatbot.
 *  La ruta raíz "/chat" será manejada por el predicado `handle_chat_request`.
 */
:- http_handler(root(chat), handle_chat_request, []). 

/** handle_chat_request
 *  
 *  Maneja las solicitudes HTTP POST y OPTIONS.
 *  @param Request La solicitud HTTP entrante.
 */
handle_chat_request(Request) :-
    memberchk(method(Options), Request),  % Verifica si el Método de la solicitud es POST o OPTIONS
    handle_options(Options, Request).     % Llama al predicado correspondiente para manejar la solicitud

/** handle_options
 *  
 *  Maneja solicitudes POST y OPTIONS según el método especificado.
 *  @param Method El Método HTTP (POST u OPTIONS).
 *  @param Request La solicitud HTTP.
 */
handle_options(post, Request) :-
    !,  % Evitar retroceso tras éxito
    cors_enable,  % Habilitar CORS
    http_read_json_dict(Request, QueryDict),  % Leer el cuerpo de la solicitud en formato JSON
    Query = QueryDict.get(query),  % Extraer la consulta del campo "query"
    process_query(Query, Response),  % Procesar la consulta usando el chatbot
    format('Content-type: application/json; charset=UTF-8~n'),  % Asegurar que el contenido sea UTF-8
    reply_json_dict(_{response: Response}, [json_object(dict)]).  % Devolver la respuesta en formato JSON

handle_options(options, _Request) :-
    !,  % Evitar retroceso tras éxito
    cors_enable,  % Habilitar CORS para solicitudes OPTIONS
    format('Content-type: text/plain; charset=UTF-8~n~n'),  
    format('OK').  % Responder con "OK" para confirmar la solicitud OPTIONS

/** cors_enable
 *  
 *  Habilita CORS en la respuesta HTTP, permitiendo que el servidor sea accesible desde cualquier origen.
 */
cors_enable :-
    format('Access-Control-Allow-Origin: *~n'),  % Permitir solicitudes desde cualquier origen
    format('Access-Control-Allow-Methods: POST, OPTIONS~n'),  % Permitir los Métodos POST y OPTIONS
    format('Access-Control-Allow-Headers: Content-Type~n').  % Permitir el encabezado Content-Type

/** process_query
 *  
 *  Procesa una consulta enviada al chatbot y genera una respuesta adecuada.
 *  @param Input La consulta del usuario en formato de texto.
 *  @param Response La respuesta generada por el chatbot.
 */
process_query(Input, Response) :-
    atom_string(InputAtom, Input),  % Asegurar que la entrada se trate como átomo
    (   InputAtom == 'adios'
    ->  Response = 'hasta la proxima',  % Responder con mensaje de despedida
        reset_user  % Restablecer la sesión del usuario
    ;   normalize_input(Input, Words),  % Normalizar la consulta
        validacion_gramatical(Words, Resultado),  % Validar la gramática de la entrada
        (   Resultado == 'valido'  % Si la gramática es válida
        ->  find_best_matching_theme(Words, Theme),  % Encontrar el tema que mejor coincida
            store_user_theme(Words),  % Almacenar el tema detectado en el perfil del usuario
            (   Theme == 'calorias'
            ->  store_calories(Words)  % Almacenar la información calórica si el tema es "calorias"
            ;   true  % No se necesita acción específica para otros temas
            ),
            theme_response(Theme, ThemeResponse),  % Obtener la respuesta basada en el tema
            check_diet_compatibility(MatchedMenus),  % Verificar las dietas compatibles
            (   MatchedMenus \= []  % Si hay dietas compatibles
            ->  extract_diet_body(MatchedMenus, ResponseBody),  % Extraer los detalles del plan de dieta
                atomic_list_concat(ResponseBody, ', ', Response)  % Combinar la respuesta en una cadena
            ;   Response = ThemeResponse  % Si no hay dietas compatibles, usar la respuesta del tema
            )
        ;   % Si la gramática no es válida, devolver un mensaje de error
            Response = Resultado
        )
    ).

/** extract_diet_body
 *  
 *  Extrae solo el cuerpo del plan de dieta de una lista de menús coincidentes.
 *  @param MatchedMenus Lista de dietas coincidentes.
 *  @param Meals El menú extraído de la dieta.
 */
extract_diet_body([_-Meals | _], Meals).  % Extraer la lista de comidas de la dieta
