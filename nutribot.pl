:- consult('nutribot_BNF').
:- consult('nutribot_DB').

% Declare user/2 as dynamic so it can be modified
:- dynamic user/2.

% Define a dynamic user profile
user("profile", []).

% Define themes and their associated keywords
theme('welcom', ['hola', 'como', 'estas', 'buenas', 'holi','holap','uwu']).
theme('goodbye', ['adios', 'hasta', 'luego','chao']).
theme('help_need', ['ayuda', 'sobre', 'peso', 'deseo', 'quiero', 'me', 'gustaria']).
theme('Dislipidemia',['problema','control','colesterol','Dislipidemia']).
theme('Hipercolesterolemia',['Hipercolesterolemia','aumento','niveles','colesterol','sangre']).
theme('avanzado', ['mas', '5', 'veces', 'alta', 'frecuente', 'diariamente']).
theme('intermedio', ['3', 'veces', 'media', 'moderada','mucho']).
theme('inicial', ['menos', '3' ,'veces', 'baja', 'poco', 'sedentario','no','hago','ejercicio','casi','nada']).
theme('saludable',['no','enfermo','saludable','estoy','tengo','ninguna','enfermedad','padezco']).
theme('proteica', ['proteica', 'alta en proteinas', 'proteínas', 'musculo', 'muscular','alta','en']).
theme('alcalina', ['alcalina', 'ph', 'equilibrio', 'basica', 'ácido', 'acida']).
theme('mediterranea', ['mediterránea', 'aceite de oliva', 'granos', 'pescado', 'frutas', 'verduras', 'saludable']).
theme('vegetariana', ['vegetariana', 'sin carne', 'vegetal', 'proteínas vegetales', 'frutas', 'verduras']).
theme('keto', ['keto', 'cetogénica', 'baja en carbohidratos', 'grasas', 'cetonas']).
theme('detox', ['detox', 'desintoxicante', 'limpieza', 'jugos', 'toxinas', 'limpiar']).
theme('hipercalorica', ['hipercalórica', 'alto en calorías', 'subir de peso', 'aumento', 'energía']).
theme('hipocalorica', ['hipocalórica', 'baja en calorías', 'perder peso', 'dieta baja', 'deficit calórico']).

theme('calorias',['calorias','cantidad','diarias','consumir','consumo','diario']).
theme('no_calorias',['no','calorias','tengo', 'especifico', 'se', 'cuantos']).

% Define responses for themes
theme_response('welcom', 'Hola, como puedo ayudarte?').
theme_response('goodbye', 'hasta la proxima 👋').
theme_response('help_need', 'Soy tu nutricionista profesional para ayudarte, ¿padeces de alguna enfermedad?').

% Respuestas a enfermedades
theme_response('Dislipidemia','Te recomendaría una dieta baja en grasas, ¿Tienes pensado una cantidad específica de calorías diarias por consumir?').
theme_response('Hipercolesterolemia','Te recomendaría una dieta vegana, ¿Tienes pensado una cantidad específica de calorías diarias por consumir? ').
theme_response('saludable','Me alegro, ¿Tienes pensado una cantidad específica de calorías diarias por consumir? ').

% Respuestas a calorias
theme_response('calorias', '¿Eres activo físicamente?').
theme_response('no_calorias', '¿Eres activo físicamente?').

% Respuestas a actividad fisica
theme_response('avanzado', '¡Genial! Hacer actividad más de 5 veces por semana es excelente para tu salud, ¿Tienes un tipo de dieta te gustaría realizar?').
theme_response('intermedio', 'Hacer ejercicio 3 veces por semana es un buen inicio, sigue así. ¿Tienes un tipo de dieta te gustaría realizar?').
theme_response('inicial', 'Es importante aumentar tu actividad física para mejorar tu salud, intenta hacer ejercicio al menos 3 veces por semana. ¿Tienes un tipo de dieta te gustaría realizar?').

% Respuestas a dietas
theme_response('proteica', 'Te recomiendo una dieta alta en proteínas para ganar masa muscular y mantener tu energía.').
theme_response('alcalina', 'Una dieta alcalina te ayudará a equilibrar el pH de tu cuerpo. ¿Te gustaría recibir algunas recomendaciones?').
theme_response('mediterranea', 'La dieta mediterránea es excelente para la salud cardiovascular, con un enfoque en alimentos saludables como aceite de oliva, pescado, y frutas.').
theme_response('vegetariana', 'Una dieta vegetariana es una excelente opción. ¿Te gustaría conocer opciones ricas en proteínas vegetales?').
theme_response('keto', 'La dieta keto es baja en carbohidratos y alta en grasas. ¿Te gustaría aprender más sobre cómo entrar en cetosis?').
theme_response('detox', 'Una dieta detox puede ayudar a limpiar tu cuerpo de toxinas. ¿Estás pensando en hacer una desintoxicación con jugos o batidos?').
theme_response('hipercalorica', 'Una dieta hipercalórica puede ayudarte a ganar peso o energía. Asegúrate de consumir alimentos ricos en nutrientes.').
theme_response('hipocalorica', 'Una dieta hipocalórica es efectiva para perder peso. ¿Te gustaría recomendaciones para mantenerte en déficit calórico?').

% Fallback responses to individual inputs
respond('hola', 'Hola, ¿cómo puedo ayudarte hoy?').
respond('como estas', 'Estoy bien, gracias. ¿Y tú?').
respond('cual es tu nombre', 'Soy Nutrichat sin nombre. ¿Cómo te llamas tú?').
respond('adios', '¡Hasta luego!').

respond(_, 'Lo siento, no entiendo tu pregunta.').

% Match words with a theme
match_theme(Words, Theme, Count) :-
    theme(Theme, Keywords),
    count_matches(Words, Keywords, 0, Count).

% Count how many words match the theme keywords
count_matches([], _, Count, Count).
count_matches([Word|Rest], Keywords, Acc, Count) :-
    (   member(Word, Keywords)
    ->  NewAcc is Acc + 1
    ;   NewAcc is Acc
    ),
    count_matches(Rest, Keywords, NewAcc, Count).

% Check if a theme matches the input words
find_matching_theme(Words, Theme) :-
    match_theme(Words, Theme, Count),
    Count >= 2.

% Store the detected theme in the user profile and print the updated profile
store_user_theme(Words) :-
    find_matching_theme(Words, Theme),
    retract(user("profile", Profile)),
    append([Theme], Profile, NewProfile),
    assert(user("profile", NewProfile)),
    write('Perfil actualizado: '), write(NewProfile), nl.  % Print the updated profile


% Extraer el número de calorías de la oración
extract_calories(Words, Calories) :-
    append(_, [Numero, calorias], Words),  % Buscar el número antes de la palabra "calorias"
    (   number(Numero)
    ->  Calories = Numero  % Si es un número, lo usamos directamente
    ;   atom_number(Numero, Calories)  % Si es un átomo, lo convertimos a número
    ).

% Guardar calorías en el perfil del usuario
store_calories(Words) :-
    extract_calories(Words, Calories),
    retract(user("profile", Profile)),
    atom_number(CaloriesAtom, Calories),  % Convertir número en atom para almacenamiento
    append([CaloriesAtom], Profile, NewProfile),
    assert(user("profile", NewProfile)),
    write('Perfil actualizado con calorías: '), write(NewProfile), nl.


% Chequeo del perfil de usuario con las dietas

check_diet_compatibility :-
    user("profile", Profile),  % Accede al perfil del usuario
    findall(Diet, dieta(Diet), Diets),  % Recoge todas las dietas
    check_diets(Profile, Diets).  % Verifica las dietas con el perfil del usuario
    
check_diets(_, []) :- !.
check_diets(UserKeywords, [Diet|Rest]) :-
    Diet = [Name | Keywords],  % Separa el nombre de la dieta de sus parámetros
    count_matches(UserKeywords, Keywords, 0, Count),  % Asegúrate de que la firma sea correcta
    (Count >= 3 ->
        write('Menú para la dieta: '), write(Name), nl,
        % Aquí se llama a imprimir_dieta/2
        imprimir_dieta(Name, _)  % Llama a imprimir_dieta con el nombre de la dieta
    ;   true
    ),
    check_diets(UserKeywords, Rest).


imprimir_dieta(NombreDieta, MenuFunc) :-
    dieta([NombreDieta, _, _, _, _, _, _, _, MenuFunc]),
    call(MenuFunc).

% Main interaction loop with grammatical check (with trace for debugging)
chat :- 
    write('Tu: '), 
    flush_output, 
    read_line_to_string(user_input, InputRaw), 
    normalize_input(InputRaw, Words),
    write('Entrada normalizada: '), writeln(Words),  % Trace to see normalized input

    (   Words == ['adios'] 
    ->  write('Chatbot: ¡Hasta luego!'), nl
    ;   (   validacion_gramatical(Words)  % Verify grammar before proceeding
        ->  write('Entrada válida, procesando...'), nl,
            process_chat(Words)  % Separate function to process valid input
        ;   write('Chatbot: Lo siento, tu gramática no es correcta. Por favor intenta de nuevo.'), nl,
            chat
        )
    ).

% Separate processing logic for valid input
process_chat(Words) :-
    store_user_theme(Words),  % Store the detected theme
    write('Perfil actualizado: '), writeln(Words), nl,
    (   find_matching_theme(Words, Theme)
    ->  (   Theme == 'calorias'
        ->  store_calories(Words),  % Procesar calorías si el tema es 'calorias'
            theme_response('calorias', Response),
            write('Chatbot: '), write(Response), nl
        ;   theme_response(Theme, Response),
            write('Chatbot: '), write(Response), nl
        ),
        check_diet_compatibility  % Verificar compatibilidad de dietas
    ;   respond(Words, Response),
        write('Chatbot: '), write(Response), nl
    ),
    chat.

% Normalize input: convert to lowercase, remove punctuation, and convert words to atoms or numbers (with trace)
normalize_input(Input, NormalizedWords) :-
    string_lower(Input, Lowered),
    write('Texto en minúsculas: '), writeln(Lowered),  % Trace for lowercased text
    split_string(Lowered, " ", ".,!?", Parts),  % Split by spaces and remove punctuation
    write('Partes del texto: '), writeln(Parts),  % Trace for parts after split
    maplist(convert_to_atom_or_number, Parts, NormalizedWords).  % Convert to atoms or numbers

% Convert strings to atoms or numbers (with trace)
convert_to_atom_or_number(Part, Atom) :-
    (   number_string(Number, Part)
    ->  Atom = Number  % If the part is a number, keep it as a number
    ;   atom_string(Atom, Part),  % Otherwise, convert the string to an atom
        write('Convertido a átomo: '), writeln(Atom)  % Trace for each converted atom
    ).

% Entry point
comienzo :-
    write('Bienvenido al chatbot. Escribe "adios" para terminar.'), nl,
    chat.


% Reglas para pruebas

reset_user :-
    retractall(user("profile", _)), 
    assert(user("profile", ['proteica', '2000', 'intermedio', 'musculo', 'pérdida', 'grasa'])).  

print_user :-
    user("profile", Profile),
    write('Perfil del usuario: '), write(Profile), nl.

check :-
    reset_user,
    print_user,
    check_diet_compatibility.
