:- consult('nutribot_BNF').
:- consult('nutribot_DB').

% Verificación de temas y respuestas

dieta('alta en proteina',["no",'alta','proteica']).
dieta('vegana',['colesterol alto','media','vegana']).

user("profile",[]).

% Define themes and their associated keywords
theme('welcom', ['hola', 'como', 'estas', 'buenas', 'holi','holap','uwu']).
theme('goodbye', ['adios', 'hasta', 'luego','chao']).
theme('help_need', ['ayuda', 'sobre', 'peso', 'deseo', 'quiero', 'me', 'gustaria']).
theme('Dislipidemia',['problema','control','colesterol','Dislipidemia']).
theme('Hipercolesterolemia',['Hipercolesterolemia','aumento','niveles','colesterol','sangre']).
theme('actividad_alta', ['mas', '5', 'veces', 'alta', 'frecuente', 'diariamente']).
theme('actividad_media', ['3', 'veces', 'media', 'moderada','mucho']).
theme('actividad_baja', ['menos', '3' ,'veces', 'baja', 'poco', 'sedentario','no','hago','ejercicio','casi','nada']).
theme('saludable',['no','enfermo','saludable','estoy']).

% Define responses for themes
theme_response('welcom', 'Hola, como puedo ayudarte?').
theme_response('goodbye', 'hasta la proxima 👋').
theme_response('help_need', 'Soy tu nutricionista profesional para ayudarte, ¿padeces de alguna enfermedad?').
theme_response('Dislipidemia','Te recomendaría una dieta baja en grasas, ¿qué tanta actividad física haces?').
theme_response('Hipercolesterolemia','Te recomendaría una dieta vegana, ¿qué tanta actividad física haces?').

theme_response('actividad_alta', '¡Genial! Hacer actividad más de 5 veces por semana es excelente para tu salud.').
theme_response('actividad_media', 'Hacer ejercicio 3 veces por semana es un buen inicio, sigue así.').
theme_response('actividad_baja', 'Es importante aumentar tu actividad física para mejorar tu salud, intenta hacer ejercicio al menos 3 veces por semana.').

% Fallback responses to individual inputs
respond('hola', 'Hola, ¿cómo puedo ayudarte hoy?').
respond('como estas', 'Estoy bien, gracias. ¿Y tú?').
respond('cual es tu nombre', 'Soy Nutrichat sin nombre. ¿Cómo te llamas tú?').
respond('adios', '¡Hasta luego!').

respond(_, 'Lo siento, no entiendo tu pregunta.').

% Normalize input: convert to lowercase, remove punctuation, and convert words to atoms
normalize_input(Input, NormalizedWords) :-
    string_lower(Input, Lowered),
    split_string(Lowered, " ", ".,!?", Parts),  % Split by spaces and remove punctuation
    maplist(atom_string, NormalizedWords, Parts).  % Convert strings to atoms for comparison

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

% Main interaction loop with grammatical check
chat :-
    write('Tu: '),
    flush_output,
    read_line_to_string(user_input, InputRaw),
    normalize_input(InputRaw, Words),
    (   Words == ['adios']
    ->  write('Chatbot: ¡Hasta luego!'), nl
    ;   (   validacion_gramatical(Words)  % Valida la gramática antes de continuar
        ->  (   find_matching_theme(Words, Theme)
            ->  theme_response(Theme, Response),
                write('Chatbot: '), write(Response), nl
            ;   atomic_list_concat(Words, ' ', Input),
                respond(Input, Response),
                write('Chatbot: '), write(Response), nl
            )
        ;   true  % Si falla la validación gramatical, se pide nueva entrada
        ),
        chat
    ).

% Entry point
comienzo :-
    write('Bienvenido al chatbot. Escribe "adios" para terminar.'), nl,
    chat.
