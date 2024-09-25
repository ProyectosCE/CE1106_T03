:- consult('nutribot-BNF').
:- consult('nutribot-diets').

% Definir dietas con recomendaciones
dieta('alta en proteina', ["no",'alta','proteica']).
dieta('vegana',['colesterol alto','media','vegana']).
dieta('keto', ['keto','baja en carbohidratos','grasas saludables']).
dieta('vegetariana', ['vegetariana','sin carne']).

% Base de datos de usuarios (inicialmente vacía)
user("profile", []).

% Definir temas y palabras clave asociadas
theme('welcome', ['hola', 'como', 'estas', 'buenas', 'holi','holap','uwu']).
theme('goodbye', ['adios', 'hasta', 'luego','chao']).
theme('help_need', ['ayuda', 'sobre', 'peso', 'deseo', 'quiero', 'me', 'gustaria', 'bajar']).
theme('Dislipidemia',['problema','control','colesterol','Dislipidemia']).
theme('Hipercolesterolemia',['Hipercolesterolemia','aumento','niveles','colesterol','sangre']).
theme('actividad_alta', ['mas', '5', 'veces', 'alta', 'frecuente', 'diariamente']).
theme('actividad_media', ['3', 'veces', 'media', 'moderada','mucho']).
theme('actividad_baja', ['menos', '3' ,'veces', 'baja', 'poco', 'sedentario','no','hago','ejercicio','casi','nada']).
theme('saludable',['no','enfermo','saludable','estoy']).
theme('calorias', ['calorias', '1800', '3000', '1500', 'cantidad', 'consumir']).
theme('diet_preference', ['dieta', 'keto', 'vegana', 'vegetariana', 'sin', 'mariscos', 'proteina', 'grasas', 'azucar']).

% Definir respuestas para los temas
theme_response('welcome', 'Hola, encantado de verte. ¿En qué te puedo ayudar hoy?').
theme_response('goodbye', '¡Hasta la próxima! Cuídate.').
theme_response('help_need', 'Soy tu nutricionista profesional. ¿Padeces alguna enfermedad?').
theme_response('Dislipidemia', 'Te recomendaría una dieta baja en grasas. ¿Cuántas calorías diarias te gustaría consumir?').
theme_response('Hipercolesterolemia', 'Te recomendaría una dieta vegana. ¿Qué tanta actividad física realizas?').
theme_response('actividad_alta', '¡Genial! Hacer actividad más de 5 veces por semana es excelente para tu salud.').
theme_response('actividad_media', 'Hacer ejercicio 3 veces por semana es un buen inicio. Sigue así.').
theme_response('actividad_baja', 'Es importante aumentar tu actividad física para mejorar tu salud. Intenta hacer ejercicio al menos 3 veces por semana.').
theme_response('calorias', 'Especifica cuántas calorías diarias deseas consumir.').
theme_response('diet_preference', '¿Tienes alguna preferencia alimenticia o algún tipo de dieta que te gustaría seguir?').

% Respuestas fallback para entradas individuales
respond('hola', 'Hola, ¿cómo puedo ayudarte hoy?').
respond('como estas', 'Estoy bien, gracias. ¿Y tú?').
respond('cual es tu nombre', 'Soy Nutrichat, tu nutricionista personal. ¿Cómo te llamas tú?').
respond('adios', '¡Hasta luego!').
respond(_, 'Lo siento, no entiendo tu pregunta. ¿Puedes reformularla?').

% Normalizar entrada: convierte a minúsculas, elimina puntuación, y convierte palabras a átomos
normalize_input(Input, NormalizedWords) :-
    string_lower(Input, Lowered),
    split_string(Lowered, " ", ".,!?", Parts),  % Dividir por espacios y eliminar puntuación
    maplist(atom_string, NormalizedWords, Parts).  % Convertir strings a átomos para comparación

% Coincidencia de palabras con un tema
match_theme(Words, Theme, Count) :-
    theme(Theme, Keywords),
    count_matches(Words, Keywords, 0, Count).

% Contar cuántas palabras coinciden con las palabras clave del tema
count_matches([], _, Count, Count).
count_matches([Word|Rest], Keywords, Acc, Count) :-
    (   member(Word, Keywords)
    ->  NewAcc is Acc + 1
    ;   NewAcc is Acc
    ),
    count_matches(Rest, Keywords, NewAcc, Count).

% Buscar si algún tema coincide con las palabras de entrada
find_matching_theme(Words, Theme) :-
    match_theme(Words, Theme, Count),
    Count >= 2.

% Bucle de interacción principal con validación gramatical
chat :- 
    write('Tu: '),
    flush_output,
    read_line_to_string(user_input, InputRaw),
    normalize_input(InputRaw, Words),
    (   Words == ['adios']
    ->  write('Chatbot: ¡Hasta luego!'), nl
    ;   (   validacion_gramatical(Words)  % Validar la gramática antes de continuar
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

% Punto de entrada
comienzo :-
    write('Bienvenido a NutriTec. Escribe "adios" para terminar.'), nl,
    chat.
