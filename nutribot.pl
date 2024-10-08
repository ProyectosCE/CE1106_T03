/* 
================================== LICENCIA ================================================== 
MIT License
Copyright (c) 2024 José Bernardo Barquero Bonilla,
                   Jose Eduardo Campos Salazar,
                   Jimmy Feng Feng,
                   Alexander Montero Vargas
Consulta el archivo LICENSE para más detalles.
==============================================================================================
*/

/* 
==================================REFERENCIAS================================================
Para este archivo se tomaron como referencia general las siguientes fuentes:

 * Documentación propia de Prolog: https://www.swi-prolog.org/
 ===========================================================================================
*/



/**
 * nutribot_BNF and nutribot_DB are consulted for loading additional rules or facts.
 */
:- consult('nutribot_BNF').
:- consult('nutribot_DB').

/**
 * Predicate to store dynamic user information.
 * @dynamic user/2
 */
:- dynamic user/2.

/** 
 * Default user profile initialized with an empty profile.
 */
user("profile", []).

/**
 * Themes associated with user input. Each theme is a keyword group associated with certain words.
 * @param Theme The theme category.
 * @param Keywords The list of words associated with the theme.
 */
theme('welcom', ['hola', 'como', 'estas', 'buenas', 'holi','holap','uwu']).
theme('goodbye', ['adios', 'hasta', 'luego','chao']).
theme('help_need', ['ayuda', 'sobre', 'peso', 'deseo', 'quiero', 'gustaria','perder','bajar','sobre','peso','sobrepeso','tengo']).
theme('Dislipidemia',['problema','control','colesterol','dislipidemia','tengo','controlando']).
theme('Hipercolesterolemia',['hipercolesterolemia','aumento','niveles','colesterol','sangre']).
theme('avanzado', ['mas', '5', 'veces', 'alta', 'frecuente', 'diariamente','semana','ejercicio']).
theme('intermedio', ['3', 'veces', 'media', 'moderada','mucho','ejercicio']).
theme('inicial', ['menos','veces', 'baja', 'poco', 'sedentario','no','ejercicio','casi','nada','hago']).
theme('saludable',['enfermo','saludable','ninguna','enfermedad','padezco']).
theme('proteica', ['proteica', 'alta en proteinas', 'proteínas', 'musculo', 'muscular','alta','dieta','quiero']).
theme('alcalina', ['alcalina', 'ph', 'equilibrio', 'basica', 'ácido', 'acida']).
theme('mediterranea', ['mediterranea', 'aceite de oliva', 'granos', 'pescado', 'frutas', 'verduras', 'saludable','quiero','dieta']).
theme('vegetariana', ['vegetariana', 'sin', 'carne', 'vegetal', 'proteínas', 'vegetales', 'frutas', 'verduras','dieta','quiero']).
theme('keto', ['keto', 'cetogénica', 'baja en carbohidratos', 'grasas', 'cetonas']).
theme('detox', ['detox', 'desintoxicante', 'limpieza', 'jugos', 'toxinas', 'limpiar']).
theme('hipercalorica', ['hipercalórica', 'alto',  'calorias', 'subir', 'peso', 'aumento', 'energía']).
theme('hipocalorica', ['hipocalórica', 'baja', 'calorías', 'perder', 'peso', 'dieta baja', 'deficit', 'calórico']).
theme('calorias',['calorias','cantidad','diarias','consumir','consumo','diario','calorías','quiero','unas']).

/**
 * Responses associated with each theme, used to generate chatbot responses based on detected themes.
 * @param Theme The theme category.
 * @param Response The response message.
 */
theme_response('welcom', 'Hola, como puedo ayudarte?').
theme_response('goodbye', 'hasta la proxima 👋').
theme_response('help_need', 'Soy tu nutricionista profesional para ayudarte, ¿padeces de alguna enfermedad?').
theme_response('Dislipidemia','Te recomendaría una dieta baja en grasas, ¿Tienes pensado una cantidad específica de calorías diarias por consumir?').
theme_response('Hipercolesterolemia','Te recomendaría una dieta vegana, ¿Tienes pensado una cantidad específica de calorías diarias por consumir? ').
theme_response('saludable','Me alegro, ¿Tienes pensado una cantidad específica de calorías diarias por consumir? ').
theme_response('calorias', '¿Eres activo físicamente?').
theme_response('no_calorias', '¿Eres activo físicamente?').
theme_response('avanzado', '¡Genial! Hacer actividad más de 5 veces por semana es excelente para tu salud, ¿Tienes un tipo de dieta te gustaría realizar?').
theme_response('intermedio', 'Hacer ejercicio 3 veces por semana es un buen inicio, sigue así. ¿Tienes un tipo de dieta te gustaría realizar?').
theme_response('inicial', 'Es importante aumentar tu actividad física para mejorar tu salud, intenta hacer ejercicio al menos 3 veces por semana. ¿Tienes un tipo de dieta te gustaría realizar?').
theme_response('proteica', 'Te recomiendo una dieta alta en proteínas para ganar masa muscular y mantener tu energía.').
theme_response('alcalina', 'Una dieta alcalina te ayudará a equilibrar el pH de tu cuerpo.').
theme_response('mediterranea', 'La dieta mediterránea es excelente para la salud cardiovascular, con un enfoque en alimentos saludables como aceite de oliva, pescado, y frutas.').
theme_response('vegetariana', 'Una dieta vegetariana es una excelente opción.').
theme_response('keto', 'La dieta keto es baja en carbohidratos y alta en grasas.').
theme_response('detox', 'Una dieta detox puede ayudar a limpiar tu cuerpo de toxinas.').
theme_response('hipercalorica', 'Una dieta hipercalórica puede ayudarte a ganar peso o energía. Asegúrate de consumir alimentos ricos en nutrientes.').
theme_response('hipocalorica', 'Una dieta hipocalórica es efectiva para perder peso.').
theme_response('invalid', 'Lo siento, no entiendo tu consulta. ¿Puedes reformularla?').

/**
 * Predicate to generate responses based on predefined user queries.
 * @param Input The user input.
 * @param Response The response generated by the chatbot.
 */
respond('hola', 'Hola, ¿cómo puedo ayudarte hoy?').
respond('como estas', 'Estoy bien, gracias. ¿Y tú?').
respond('cual es tu nombre', 'Soy Nutrichat sin nombre. ¿Cómo te llamas tú?').
respond('adios', '¡Hasta luego!').
respond(_, 'Lo siento, no entiendo tu pregunta.').

/**
 * Normalizes user input by converting it to lowercase and splitting it into individual words.
 * @param Input The raw input string.
 * @param NormalizedWords The list of normalized words.
 */
normalize_input(Input, NormalizedWords) :-
    string_lower(Input, Lowered),
    split_string(Lowered, " ", ".,!?", Parts),
    maplist(atom_string, NormalizedWords, Parts).

/**
 * Counts the number of matches between a list of words and a list of keywords.
 * @param Words The list of words to compare.
 * @param Keywords The list of keywords to compare against.
 * @param Acc The accumulator for the match count.
 * @param Count The final count of matches.
 */
count_matches([], _, Count, Count).
count_matches([Word|Rest], Keywords, Acc, Count) :-
    (   member(Word, Keywords)
    ->  NewAcc is Acc + 1
    ;   NewAcc is Acc
    ),
    count_matches(Rest, Keywords, NewAcc, Count).

/**
 * Finds the best matching theme for a given list of words by counting matches.
 * @param Words The list of words.
 * @param BestTheme The theme that has the most keyword matches.
 */
find_best_matching_theme(Words, BestTheme) :-
    findall(Count-Theme, (
        theme(Theme, Keywords),
        count_matches(Words, Keywords, 0, Count)
    ), Results),
    max_member(_-BestTheme, Results).

/**
 * Stores the detected theme in the user's profile.
 * @param Words The words from user input to analyze.
 */
store_user_theme(Words) :-
    find_best_matching_theme(Words, Theme),
    retract(user("profile", Profile)),
    append([Theme], Profile, NewProfile),
    assert(user("profile", NewProfile)).

/**
 * Extracts the number of calories from the user's input and stores it in their profile.
 * @param Words The list of words containing calorie information.
 * @param Calories The extracted calorie number.
 */
extract_calories(Words, Calories) :-
    append(_, [NumeroAtom, calorias], Words),
    atom_number(NumeroAtom, Calories).

/**
 * Stores the user's calorie intake in their profile.
 * @param Words The words containing the calorie information.
 */
store_calories(Words) :-
    extract_calories(Words, Calories),
    retract(user("profile", Profile)),
    append([Calories], Profile, NewProfile),
    assert(user("profile", NewProfile)).

/**
 * Checks compatibility of user's preferences with available diets.
 * @param MatchedDiets The list of diets that match the user's profile.
 */
check_diet_compatibility(MatchedDiets) :-
    user("profile", Profile),
    findall(Diet, dieta(Diet), Diets),
    check_diets(Profile, Diets, MatchedDiets).

/**
 * Recursively checks diets to see if they match the user's preferences.
 * @param UserKeywords The user's profile keywords.
 * @param Diets The list of available diets.
 * @param MatchedDiets The list of matched diets.
 */
check_diets(_, [], []).  
check_diets(UserKeywords, [Diet|Rest], MatchedDiets) :-
    Diet = [NombreDieta | Keywords],
    count_matches(UserKeywords, Keywords, 0, Count),
    (Count >= 3 ->  
        dieta([NombreDieta, _, _, _, _, _, _, _, MenuFunc]),
        call(MenuFunc, Menu),
        MatchedDiets = [NombreDieta-Menu|MoreMatchedDiets],  
        check_diets(UserKeywords, Rest, MoreMatchedDiets)
    ;   check_diets(UserKeywords, Rest, MatchedDiets)).  

/**
 * Main chat loop for the chatbot. Reads input, normalizes it, and generates responses.
 */
chat :- 
    write('Tu: '), 
    flush_output, 
    read_line_to_string(user_input, InputRaw), 
    normalize_input(InputRaw, Words),

    (   Words == ['adios'] 
    ->  write('Chatbot: ¡Hasta luego!'), nl, reset_user, comienzo
    ;   validacion_gramatical(Words, Resultado),
        (   (Resultado == 'valido')
        ->  store_user_theme(Words),  
            (   find_best_matching_theme(Words, Theme)
            ->  (   Theme == 'calorias' 
                ->  store_calories(Words),  % Almacenar las calorías proporcionadas por el usuario
                    theme_response('calorias', Response),  % Respuesta específica para calorías
                    write('Chatbot: '), write(Response), nl,
                    
                    % Verificación de compatibilidad de dietas
                    check_diet_compatibility(MatchedMenus),
                    print_menus(MatchedMenus)
                    
                ;   theme_response(Theme, Response),
                    write('Chatbot: '), write(Response), nl,
                    
                    % Verificación de compatibilidad de dietas
                    check_diet_compatibility(MatchedMenus),
                    print_menus(MatchedMenus),
                    
                    % Para temas específicos: si no encuentra menú, lo informa
                    (   member(Theme, ['proteica', 'alcalina', 'mediterranea', 'vegetariana', 'keto', 'detox', 'hipercalorica', 'hipocalorica'])
                    ->  (MatchedMenus == [] 
                        -> writeln('No hay menú disponible')
                        ; true)  % Ya imprimió el menú, no hace falta ás
                    ;   true  % No hace nada extra para otros temas
                    )
                )
            ;   atomic_list_concat(Words, ' ', Input),
                respond(Input, Response),
                write('Chatbot: '), write(Response), nl
            )
        ;   write('Chatbot: '), write(Resultado), nl
        ),
        chat  
    ).

/**
 * Prints matched diet menus to the console.
 * @param MatchedMenus The list of diets with their associated menus.
 */
print_menus([]).  
print_menus([NombreDieta-Menu|Rest]) :-
    write('Menú para la dieta: '), writeln(NombreDieta), nl,
    print_menu(Menu),
    print_menus(Rest).

/**
 * Prints each item in the menu.
 * @param Menu The list of menu items.
 */
print_menu([]).
print_menu([Head|Tail]) :-
    writeln(Head),
    print_menu(Tail).

/**
 * Begins the chatbot interaction with the user.
 */
comienzo :-
    write('Bienvenido al chatbot. Escribe "adios" para terminar.'), nl,
    chat.

/**
 * Resets the user's profile and clears stored information.
 */
reset_user :-
    retractall(user("profile", _)),  
    set_default_user.

/**
 * Initializes the default user profile.
 */
set_default_user :-
    assert(user("profile", [])).

/**
 * Prints the user's profile for debugging or monitoring purposes.
 */
print_user :-
    user("profile", Profile),
    write('Perfil del usuario: '), write(Profile), nl.

/**
 * Runs checks for diet compatibility and resets the user's profile.
 */
check :-
    reset_user,
    print_user,
    check_diet_compatibility(MatchedDiets),  % Llamada a check_diet_compatibility
    (   MatchedDiets == [] 
    ->  writeln('No hay menú disponible') 
    ;   print_menus(MatchedDiets)  % Imprime los menús si hay dietas compatibles
    ).
