:- module(iomodule, [
    process_user_input/2  % Predicado principal para procesar la entrada del usuario y devolver la respuesta
]).

:- dynamic user/2.
:- use_module('nutribot_themes').
:- consult('nutribot_DB').
:- consult('nutribot_BNF').

% Definir el perfil del usuario como dinámico
user("profile", []).

% Función principal que recibe la entrada del usuario, procesa la entrada y genera la respuesta
process_user_input(InputRaw, Response) :-
    normalize_input(InputRaw, Words),  % Normalizar la entrada
    (   Words == ['adios']
    ->  Response = "¡Hasta luego!"  % Respuesta si el usuario dice 'adios'
    ;   (   validacion_gramatical(Words)  % Verificar gramática
        ->  handle_theme(Words, Response)  % Manejar el tema y devolver la respuesta
        ;   Response = "Lo siento, tu gramática no es correcta. Por favor intenta de nuevo."  % Respuesta si la gramática no es correcta
        )
    ).

% Manejar los diferentes temas identificados
handle_theme(Words, Response) :-
    find_best_matching_theme(Words, Theme),  % Encontrar el tema coincidente
    (   Theme == 'calorias'  % Si el tema es 'calorias'
    ->  store_calories(Words, NewProfileText),  % Almacenar calorías en el perfil
        theme_response('calorias', TempResponse),  % Obtener la respuesta
        check_diet_compatibility(NewProfileText, MenuText),  % Verificar compatibilidad de dietas
        format_response(TempResponse, MenuText, Response)  % Formatear la respuesta final
    ;   store_user_theme(Words, NewProfileText),  % Almacenar el tema en el perfil
        theme_response(Theme, TempResponse),  % Obtener la respuesta basada en el tema
        check_diet_compatibility(NewProfileText, MenuText),  % Verificar compatibilidad de dietas
        format_response(TempResponse, MenuText, Response)  % Formatear la respuesta final
    ).

% Formatear la respuesta final con el menú de dietas sugerido (si aplica)
format_response(TempResponse, '', TempResponse).  % Si no hay menú sugerido, devolver solo la respuesta
format_response(TempResponse, MenuText, FullResponse) :-
    atomic_list_concat([TempResponse, ' Menú sugerido: ', MenuText], FullResponse).  % Combinar respuesta y menú

% Normalize input: convert to lowercase, remove punctuation, and convert words to atoms
normalize_input(Input, NormalizedWords) :-
    string_lower(Input, Lowered),  
    split_string(Lowered, " ", ".,!?", Parts),  
    maplist(atom_string, NormalizedWords, Parts).  

% Función para encontrar el tema con más coincidencias de palabras clave
find_best_matching_theme(Words, BestTheme) :-
    findall(Count-Theme, (
        theme(Theme, Keywords),
        count_matches(Words, Keywords, 0, Count)
    ), Results),
    max_member(_-BestTheme, Results).  

% Contar cuántas palabras coinciden con las palabras clave del tema
count_matches([], _, Count, Count).
count_matches([Word|Rest], Keywords, Acc, Count) :-
    (   member(Word, Keywords)
    ->  NewAcc is Acc + 1
    ;   NewAcc is Acc
    ),
    count_matches(Rest, Keywords, NewAcc, Count).

% Guardar solo los temas relevantes en el perfil del usuario
store_user_theme(Words, ProfileText) :-
    find_best_matching_theme(Words, Theme),
    relevant_theme(Theme),  
    retract(user("profile", Profile)),
    append([Theme], Profile, NewProfile),
    assert(user("profile", NewProfile)),
    atomic_list_concat(NewProfile, ', ', ProfileText).  
store_user_theme(_, _).

% Guardar la información de calorías en el perfil del usuario y devolver el perfil actualizado como cadena
store_calories(Words, ProfileText) :-
    extract_calories(Words, Calories),
    retract(user("profile", Profile)),
    append([Calories], Profile, NewProfile),
    assert(user("profile", NewProfile)),
    atomic_list_concat(NewProfile, ', ', ProfileText).  

% Extraer la cantidad de calorías del input
extract_calories(Words, Calories) :-
    append(_, [NumeroAtom, calorias], Words),  
    atom_number(NumeroAtom, Calories).

% Verificar compatibilidad de la dieta basado en el perfil del usuario y devolver los menús como texto si hay al menos 3 coincidencias
check_diet_compatibility(ProfileText, DietText) :-
    user("profile", Profile),  
    findall(Diet, dieta(Diet), Diets),  
    check_diets(Profile, Diets, DietText),
    atomic_list_concat(Profile, ', ', ProfileText).  

% Verificar cada tema/dieta basado en el perfil del usuario y devolver la dieta coincidente como texto si hay al menos 3 coincidencias
check_diets(_, [], '') :- !.  
check_diets(UserKeywords, [Diet|Rest], DietText) :-
    Diet = [Name | Keywords],  
    count_matches(UserKeywords, Keywords, 0, Count),  
    (Count >= 3 ->  
        get_diet_menu(Name, Menu),  % Obtener el menú si coincide
        atomic_list_concat([Name, ': ', Menu], DietText)  % Formatear el nombre y el menú
    ;   check_diets(UserKeywords, Rest, DietText)  
    ).

% Llamar a la función para obtener el menú adecuado para la dieta como texto
get_diet_menu(NombreDieta, MenuText) :-
    dieta([NombreDieta, _, _, _, _, _, _, MenuFunc]),
    with_output_to(string(MenuText), call(MenuFunc)).  % Llamar al menú adecuado y obtener el texto como cadena

% Reiniciar el perfil del usuario a un estado inicial
reset_user_profile :-
    retractall(user("profile", _)),
    assert(user("profile", [])).

% Obtener el perfil actual del usuario como cadena
get_user_profile(ProfileText) :-
    user("profile", Profile),
    atomic_list_concat(Profile, ', ', ProfileText).
