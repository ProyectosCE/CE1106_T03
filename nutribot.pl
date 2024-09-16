% Define themes and their associated keywords
theme('welcom', ['hola', 'como', 'estas', 'buenas', 'holi','holap','uwu']).
theme('goodbye', ['adios', 'hasta', 'luego','chao']).
theme('help_need', ['ayuda', 'sobre', 'peso', 'deseo', 'quiero', 'me', 'gustaria']).





% Define responses for themes
theme_response('welcom', 'Hola, como puedo ayudarte?').
theme_response('goodbye', 'hasta la proxima 👋').
theme_response('help_need', 'Soy tu nutricionista profesional para ayudarte,padeces de alguna enfermedad ?').

% Fallback responses to individual inputs
respond('hola', 'Hola, ¿como puedo ayudarte hoy?').
respond('como estas', 'Estoy bien, gracias. ¿Y tu?').
respond('cual es tu nombre', 'Soy Nutrichat sin nombre. ¿Cómo te llamas tu?').
respond('adios', '¡Hasta luego!').

% Default response if no input is recognized
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

% Main interaction loop
chat :-
    write('Tu: '),
    flush_output,
    read_line_to_string(user_input, InputRaw),
    normalize_input(InputRaw, Words),
    (   Words == ['adios']
    ->  write('Chatbot: ¡Hasta luego!'), nl
    ;   (   find_matching_theme(Words, Theme)
        ->  theme_response(Theme, Response),
            write('Chatbot: '), write(Response), nl
        ;   atomic_list_concat(Words, ' ', Input),
            respond(Input, Response),
            write('Chatbot: '), write(Response), nl
        ),
        chat
    ).

% Entry point
comienzo :-
    write('Bienvenido al chatbot. Escribe "adios" para terminar.'), nl,
    chat.
