:- module(nutribot_themes, [
    theme/2,
    theme_response/2,
    relevant_theme/1  % Asegurarse de exportar los temas relevantes
]).

% Definición de temas y palabras clave asociadas
theme('welcom', ['hola', 'como', 'estas', 'buenas', 'holi', 'holap', 'uwu']).
theme('goodbye', ['adios', 'hasta', 'luego', 'chao']).
theme('help_need', ['ayuda', 'sobre', 'peso', 'deseo', 'quiero', 'me', 'gustaria']).
theme('Dislipidemia', ['problema', 'control', 'colesterol', 'Dislipidemia']).
theme('Hipercolesterolemia', ['Hipercolesterolemia', 'aumento', 'niveles', 'colesterol', 'sangre']).
theme('avanzado', ['mas', '5', 'veces', 'alta', 'frecuente', 'diariamente', 'semana', 'ejercicio']).
theme('intermedio', ['3', 'veces', 'media', 'moderada', 'mucho', 'ejercicio']).
theme('inicial', ['menos', 'veces', 'baja', 'poco', 'sedentario', 'no', 'ejercicio', 'casi', 'nada']).
theme('saludable', ['enfermo', 'saludable', 'tengo', 'ninguna', 'enfermedad', 'padezco']).
theme('proteica', ['proteica', 'alta en proteinas', 'proteínas', 'musculo', 'muscular', 'alta', 'dieta']).
theme('alcalina', ['alcalina', 'ph', 'equilibrio', 'basica', 'ácido', 'acida']).
theme('mediterranea', ['mediterranea', 'aceite de oliva', 'granos', 'pescado', 'frutas', 'verduras', 'saludable', 'quiero', 'dieta']).
theme('vegetariana', ['vegetariana', 'sin carne', 'vegetal', 'proteínas vegetales', 'frutas', 'verduras']).
theme('keto', ['keto', 'cetogénica', 'baja en carbohidratos', 'grasas', 'cetonas']).
theme('detox', ['detox', 'desintoxicante', 'limpieza', 'jugos', 'toxinas', 'limpiar']).
theme('hipercalorica', ['hipercalórica', 'alto en calorías', 'subir de peso', 'aumento', 'energía']).
theme('hipocalorica', ['hipocalórica', 'baja en calorías', 'perder peso', 'dieta baja', 'deficit calórico']).

% Temas relevantes para la dieta (calorías, actividad física, tipo de dieta, enfermedades)
relevant_theme('calorias').
relevant_theme('avanzado').
relevant_theme('intermedio').
relevant_theme('inicial').
relevant_theme('saludable').
relevant_theme('Dislipidemia').
relevant_theme('Hipercolesterolemia').
relevant_theme('proteica').
relevant_theme('alcalina').
relevant_theme('mediterranea').
relevant_theme('vegetariana').
relevant_theme('keto').
relevant_theme('detox').
relevant_theme('hipercalorica').
relevant_theme('hipocalorica').

% Definición de respuestas para cada tema
theme_response('welcom', 'Hola, como puedo ayudarte?').
theme_response('goodbye', 'hasta la proxima 👋').
theme_response('help_need', 'Soy tu nutricionista profesional para ayudarte, ¿padeces de alguna enfermedad?').
theme_response('Dislipidemia', 'Te recomendaría una dieta baja en grasas, ¿Tienes pensado una cantidad específica de calorías diarias por consumir?').
theme_response('Hipercolesterolemia', 'Te recomendaría una dieta vegana, ¿Tienes pensado una cantidad específica de calorías diarias por consumir?').
theme_response('saludable', 'Me alegro, ¿Tienes pensado una cantidad específica de calorías diarias por consumir?').
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
