/** servidor_http
 *  
 *  Modulo Prolog que implementa un servidor HTTP para interactuar con el chatbot NutriBot.
 *  @author Alexander Montero Vargas
 */

:- use_module(library(http/thread_httpd)).   
:- use_module(library(http/http_dispatch)).  
:- use_module(library(http/http_json)).      
:- use_module(library(http/http_header)).    % Para el manejo de encabezados HTTP
:- set_prolog_flag(encoding, utf8).          % Asegurar que Prolog trabaje con UTF-8

/** consult
 *  
 *  Importa los módulos del chatbot desde el archivo principal `nutribot.pl`.
 */
:- consult('nutribot.pl').  % Importa el chatbot principal desde 'nutribot.pl'
:- consult('nutribot_BNF.pl').  % Importa el chatbot principal desde 'nutribot.pl'
/** server
 *  
 *  Inicia el servidor HTTP en el puerto especificado.
 *  @param Port El puerto en el que se iniciara el servidor.
 */
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

/** http_handler(root(chat), handle_chat_request, [])
 *  
 *  Define una ruta HTTP para manejar las consultas al chatbot.
 */
:- http_handler(root(chat), handle_chat_request, []). 

/** handle_chat_request
 *  
 *  Maneja las solicitudes HTTP POST y OPTIONS.
 *  @param Request La solicitud HTTP.
 */
handle_chat_request(Request) :-
    memberchk(method(Options), Request), 
    handle_options(Options, Request).

/** handle_options
 *  
 *  Maneja solicitudes POST y OPTIONS.
 *  @param Method El metodo HTTP (post u options).
 *  @param Request La solicitud HTTP.
 */
handle_options(post, Request) :-
    !, 
    cors_enable,  % Habilitar CORS
    http_read_json_dict(Request, QueryDict),  % Leer el cuerpo de la solicitud en formato JSON
    Query = QueryDict.get(query),  % Extraer la consulta del campo "query"
    process_query(Query, Response),  % Procesar la consulta usando el chatbot
    format('Content-type: application/json; charset=UTF-8~n'),  % Asegurar que el contenido sea UTF-8
    reply_json_dict(_{response: Response}, [json_object(dict)]).  % Devolver la respuesta en formato JSON

handle_options(options, _Request) :-
    !,  
    cors_enable,  % Habilitar CORS para solicitudes OPTIONS
    format('Content-type: text/plain; charset=UTF-8~n~n'),  
    format('OK').

/** cors_enable
 *  
 *  Habilita CORS en la respuesta HTTP.
 */
cors_enable :-
    format('Access-Control-Allow-Origin: *~n'),  % Permitir solicitudes desde cualquier origen
    format('Access-Control-Allow-Methods: POST, OPTIONS~n'),  % Permitir los étodos POST y OPTIONS
    format('Access-Control-Allow-Headers: Content-Type~n').  % Permitir el encabezado Content-Type

/** process_query
 *  
 *  Procesa una consulta enviada al chatbot.
 *  @param Input La consulta del usuario en formato de texto.
 *  @param Response La respuesta generada por el chatbot.
 */
process_query(Input, Response) :-
    atom_string(InputAtom, Input),  % Asegurarse de que la entrada se trate como un átomo
    (   InputAtom == 'adios'
    ->  Response = 'hasta la proxima', reset_user  % Llamar a reset_user para reiniciar la sesión
    ;   normalize_input(Input, Words),  % Normalizar la consulta
        validacion_gramatical(Words, Resultado),  % Validar la gramática
        (   Resultado == 'valido'  % Si la gramática es válida
        ->  store_user_theme(Words),  % Almacenar el tema detectado en el perfil del usuario
            find_best_matching_theme(Words, Theme),  % Encontrar el mejor tema correspondiente
            (   Theme == 'calorias'
            ->  store_calories(Words)  % Almacenar información relacionada con las calorías para 'calorias'
            ;   true  % No se necesita acción específica para otros temas
            ),
            theme_response(Theme, ThemeResponse),  % Obtener la respuesta del tema
            check_diet_compatibility(MatchedMenus),  % Verificar dietas compatibles
            
            % Verificar si hay menús disponibles
            (   MatchedMenus \= []  % Si hay dietas coincidentes
            ->  extract_diet_body(MatchedMenus, ResponseBody),  % Extraer el cuerpo de la dieta coincidente
                atomic_list_concat(ResponseBody, ', ', Response)  % Combinar la respuesta en una cadena
            ;   (   member(Theme, ['proteica', 'alcalina', 'mediterranea', 'vegetariana', 'keto', 'detox', 'hipercalorica', 'hipocalorica'])
                ->  Response = 'No hay menu disponible'  % Mensaje específico para temas seleccionados
                ;   Response = ThemeResponse  % Si no hay menús y no es un tema específico, usar la respuesta del tema
                )
            )
        ;   % Si la gramática no es válida, devuelve un mensaje de error
            Response = Resultado
        )
    ).


% Helper predicate to extract only the body of the diet plan
extract_diet_body([_-Meals | _], Meals).  % Extract the list of meals from the diet
