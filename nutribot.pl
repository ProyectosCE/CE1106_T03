:- consult('nutribot_BNF').
:- consult('nutribot_DB').

:- dynamic user/2.

user("profile", []).

theme('welcom', ['hola', 'como', 'estas', 'buenas', 'holi','holap','uwu']).
theme('goodbye', ['adios', 'hasta', 'luego','chao']).
theme('help_need', ['ayuda', 'sobre', 'peso', 'deseo', 'quiero', 'me', 'gustaria']).
theme('Dislipidemia',['problema','control','colesterol','Dislipidemia']).
theme('Hipercolesterolemia',['Hipercolesterolemia','aumento','niveles','colesterol','sangre']).
theme('avanzado', ['mas', '5', 'veces', 'alta', 'frecuente', 'diariamente','semana','ejercicio']).
theme('intermedio', ['3', 'veces', 'media', 'moderada','mucho','ejercicio']).
theme('inicial', ['menos','veces', 'baja', 'poco', 'sedentario','no','ejercicio','casi','nada']).
theme('saludable',['enfermo','saludable','tengo','ninguna','enfermedad','padezco']).
theme('proteica', ['proteica', 'alta en proteinas', 'proteínas', 'musculo', 'muscular','alta','dieta']).
theme('alcalina', ['alcalina', 'ph', 'equilibrio', 'basica', 'ácido', 'acida']).
theme('mediterranea', ['mediterranea', 'aceite de oliva', 'granos', 'pescado', 'frutas', 'verduras', 'saludable','quiero','dieta']).
theme('vegetariana', ['vegetariana', 'sin carne', 'vegetal', 'proteínas vegetales', 'frutas', 'verduras']).
theme('keto', ['keto', 'cetogénica', 'baja en carbohidratos', 'grasas', 'cetonas']).
theme('detox', ['detox', 'desintoxicante', 'limpieza', 'jugos', 'toxinas', 'limpiar']).
theme('hipercalorica', ['hipercalórica', 'alto en calorías', 'subir de peso', 'aumento', 'energía']).
theme('hipocalorica', ['hipocalórica', 'baja en calorías', 'perder peso', 'dieta baja', 'deficit calórico']).
theme('calorias',['calorias','cantidad','diarias','consumir','consumo','diario']).

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
theme_response('alcalina', 'Una dieta alcalina te ayudará a equilibrar el pH de tu cuerpo. ¿Te gustaría recibir algunas recomendaciones?').
theme_response('mediterranea', 'La dieta mediterránea es excelente para la salud cardiovascular, con un enfoque en alimentos saludables como aceite de oliva, pescado, y frutas.').
theme_response('vegetariana', 'Una dieta vegetariana es una excelente opción. ¿Te gustaría conocer opciones ricas en proteínas vegetales?').
theme_response('keto', 'La dieta keto es baja en carbohidratos y alta en grasas. ¿Te gustaría aprender más sobre cómo entrar en cetosis?').
theme_response('detox', 'Una dieta detox puede ayudar a limpiar tu cuerpo de toxinas. ¿Estás pensando en hacer una desintoxicación con jugos o batidos?').
theme_response('hipercalorica', 'Una dieta hipercalórica puede ayudarte a ganar peso o energía. Asegúrate de consumir alimentos ricos en nutrientes.').
theme_response('hipocalorica', 'Una dieta hipocalórica es efectiva para perder peso. ¿Te gustaría recomendaciones para mantenerte en déficit calórico?').
theme_response('invalid', 'Lo siento, no entiendo tu consulta. ¿Puedes reformularla?').

respond('hola', 'Hola, ¿cómo puedo ayudarte hoy?').
respond('como estas', 'Estoy bien, gracias. ¿Y tú?').
respond('cual es tu nombre', 'Soy Nutrichat sin nombre. ¿Cómo te llamas tú?').
respond('adios', '¡Hasta luego!').

respond(_, 'Lo siento, no entiendo tu pregunta.').

normalize_input(Input, NormalizedWords) :-
    string_lower(Input, Lowered),
    split_string(Lowered, " ", ".,!?", Parts),
    maplist(atom_string, NormalizedWords, Parts).

count_matches([], _, Count, Count).
count_matches([Word|Rest], Keywords, Acc, Count) :-
    (   member(Word, Keywords)
    ->  NewAcc is Acc + 1
    ;   NewAcc is Acc
    ),
    count_matches(Rest, Keywords, NewAcc, Count).

find_best_matching_theme(Words, BestTheme) :-
    findall(Count-Theme, (
        theme(Theme, Keywords),
        count_matches(Words, Keywords, 0, Count)
    ), Results),
    max_member(_-BestTheme, Results).

store_user_theme(Words) :-
    find_best_matching_theme(Words, Theme),
    retract(user("profile", Profile)),
    append([Theme], Profile, NewProfile),
    assert(user("profile", NewProfile)).

store_calories(Words) :-
    extract_calories(Words, Calories),
    retract(user("profile", Profile)),
    append([Calories], Profile, NewProfile),
    assert(user("profile", NewProfile)).

extract_calories(Words, Calories) :-
    append(_, [NumeroAtom, calorias], Words),
    atom_number(NumeroAtom, Calories).

check_diet_compatibility(MatchedDiets) :-
    user("profile", Profile),
    findall(Diet, dieta(Diet), Diets),
    check_diets(Profile, Diets, MatchedDiets).

check_diets(_, [], []).  
check_diets(UserKeywords, [Diet|Rest], MatchedDiets) :-
    Diet = [NombreDieta | Keywords],
    count_matches(UserKeywords, Keywords, 0, Count),
    (Count >= 3 ->  
        dieta([NombreDieta, _, _, _, _, _, _, _, MenuFunc]),
        call(MenuFunc, Menu),
        MatchedDiets = [NombreDieta-Menu|MoreMatchedDiets],  % Collect the matched diet
        check_diets(UserKeywords, Rest, MoreMatchedDiets)
    ;   check_diets(UserKeywords, Rest, MatchedDiets)).  % Continue checking without adding unmatched diets.

chat :- 
    write('Tu: '), 
    flush_output, 
    read_line_to_string(user_input, InputRaw), 
    normalize_input(InputRaw, Words),

    (   Words == ['adios'] 
    ->  write('Chatbot: ¡Hasta luego!'), nl
    ;   validacion_gramatical(Words, Resultado),
        (   (Resultado == 'valido')
        ->  store_user_theme(Words),  
            (   find_best_matching_theme(Words, Theme)
            ->  (   Theme == 'calorias'
                ->  store_calories(Words),
                    theme_response('calorias', Response),
                    check_diet_compatibility(MatchedMenus),
                    write('Chatbot: '), write(Response), nl,
                    print_menus(MatchedMenus)
                ;   theme_response(Theme, Response),
                    write('Chatbot: '), write(Response), nl
                ),
                check_diet_compatibility(MatchedMenus),
                print_menus(MatchedMenus)
            ;   atomic_list_concat(Words, ' ', Input),
                respond(Input, Response),
                write('Chatbot: '), write(Response), nl
            )
        ;   write('Chatbot: '), write(Resultado), nl
        ),
        chat  
    ).

print_menus([]).  
print_menus([NombreDieta-Menu|Rest]) :-
    write('Menú para la dieta: '), writeln(NombreDieta), nl,
    print_menu(Menu),
    print_menus(Rest).

print_menu([]).
print_menu([Head|Tail]) :-
    writeln(Head),
    print_menu(Tail).

comienzo :-
    write('Bienvenido al chatbot. Escribe "adios" para terminar.'), nl,
    chat.

reset_user :-
    retractall(user("profile", _)).

set_default_user :-
    assert(user("profile", ['mediterranea','avanzado','3000','calorias','saludable','help_need'])).

print_user :-
    user("profile", Profile),
    write('Perfil del usuario: '), write(Profile), nl.

check :-
    reset_user,
    set_default_user,
    print_user,
    check_diet_compatibility.
