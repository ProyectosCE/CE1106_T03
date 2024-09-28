/** 
 * Definicion de las dietas con sus atributos correspondientes.
 * La estructura de cada dieta es:
 * dieta([NombreDieta, TipoDieta, Calorias, [Padecimientos NO recomendados], [Padecimientos recomendados], [Actividades NO recomendadas], [Actividades recomendadas], [Detalle Dieta]])
 * 
 * Referencias de las posibles dietas presentadas en el código:
 * 
 * Proteicas: https://rutinasentrenamiento.com/fitness/ejercicios/dietas-altas-en-proteinas-beneficios-contraindicaciones-los-mejores-alimentos
 * Alcalina: https://www.dietdoctor.com
 * Mediterranea: https://www.dietdoctor.com
 * Vegetariana: https://www.webmd.com
 * Keto: https://www.healthline.com/nutrition/ketogenic-diet-101
 * Detox: https://www.webmd.com/diet/a-z/detox-diets
 * Hipercalorica: https://www.verywellfit.com
 * Hipocaloricas: https://www.mayoclinic.org/search/search-results?q=low%20calorie%20diet
 *
 */

/**
 * Dietas Proteicas:
 * - Dieta con un alto contenido en proteinas, recomendada para personas con niveles de actividad fisica intermedios.
 */
dieta(['proteica1', proteica, 2000, intermedio, [diabetes], [obesidad], ciclismo, pesas, menuProte1]).
dieta(['proteica2', proteica, 2500, intermedio, [diabetes], [obesidad], ciclismo, pesas, menuProte2]).

/**
 * Dietas Alcalinas:
 * - Dieta basada en alimentos que favorecen la alcalinidad del cuerpo, recomendada para personas con niveles de actividad inicial.
 */
dieta(['alcalina1', alcalina, 1800, inicial, [phalto], [phbajo], pesas, cardio, menuAlca1]).
dieta(['alcalina2', alcalina, 1500, inicial, [phalto], [phbajo], pesas, cardio, menuAlca2]).

/**
 * Dietas Mediterraneas:
 * - Dieta balanceada en grasas saludables, ideal para personas en niveles avanzados de actividad fisica.
 */
dieta(['mediterranea1', mediterranea, 3000, avanzado, [desnutricion], [enfermedad, cardiovascular], natacion, fondos, menuMedi1]).
dieta(['mediterranea2', mediterranea, 1500, inicial, [desnutricion], [enfermedad, cardiovascular], natacion, fondos, menuMedi2]).

/**
 * Dietas Vegetarianas:
 * - Dieta basada en alimentos de origen vegetal, con un contenido moderado de calorias.
 */
dieta(['vegetariana1', vegetariana, 1800, inicial, [fatiga], [colesterol], futbol, correr, menuVege1]).
dieta(['vegetariana2', vegetariana, 2500, intermedio, [fatiga], [colesterol], futbol, correr, menuVege2]).

/**
 * Dietas Keto (Cetogénicas):
 * - Dieta baja en carbohidratos y alta en grasas, recomendada para niveles iniciales de actividad fisica.
 */
dieta(['keto1', keto, '1800', inicial, [hiperlipidemia], [hipertension], hit, suiza, menuKeto1]).
dieta(['keto2', keto, '1400', inicial, [hiperlipidemia], [hipertension], hit, suiza, menuKeto2]).

/**
 * Dietas Detox:
 * - Dieta desintoxicante, recomendada para niveles iniciales de actividad fisica.
 */
dieta(['detox1', detox, '1400', inicial, [diabetes], [fibromialgia], crossfit, caminata, menuDetox1]).
dieta(['detox2', detox, '1600', inicial, [diabetes], [fibromialgia], crossfit, caminata, menuDetox2]).

/**
 * Dietas Hipercaloricas:
 * - Dieta rica en calorias, ideal para ganar masa muscular o tratar la desnutricion.
 */
dieta(['hipercalorica1', hipercalorica, '2500', intermedio, [obesidad], [desnutricion], powerlifting, triatlon, menuHiper1]).
dieta(['hipercalorica2', hipercalorica, '3500', avanzado, [obesidad], [desnutricion], powerlifting, triatlon, menuHiper2]).

/**
 * Dietas Hipocaloricas:
 * - Dieta baja en calorias, ideal para perder peso.
 */
dieta(['hipocalorica1', hipocalorica, '1200', inicial, [desnutricion], [sobrepeso], triatlon, yoga, menuHipo1]).
dieta(['hipocalorica2', hipocalorica, '1500', inicial, [desnutricion], [sobrepeso], triatlon, yoga, menuHipo2]).

/** 
 * Definicion de los niveles de frecuencia de actividad fisica.
 */
%Frecuencia
frecuencia([inicial]).
frecuencia([intermedio]).
frecuencia([avanzado]).

/** 
 * Definicion de las actividades fisicas disponibles.
 */
actividades([yoga]).
actividades([triatlon]).
actividades([powerlifting]).
actividades([caminata]).
actividades([crossfit]).
actividades([hit]).
actividades([suiza]).
actividades([futbol]).
actividades([correr]).
actividades([natacion]).
actividades([fondos]).
actividades([cardio]).
actividades([pesas]).
actividades([ciclismo]).

/** 
 * Definicion de las cantidades de calorias.
 * Las opciones de calorias disponibles en las dietas son:
 * - 1200: Dieta baja en calorias, recomendada para perdida de peso rapida.
 * - 1400: Dieta baja en calorias, adecuada para una perdida de peso moderada.
 * - 1500: Dieta moderada en calorias para mantener el peso o perdida leve.
 * - 1600: Dieta moderada en calorias para un equilibrio nutricional.
 * - 1800: Dieta moderada para personas con actividad fisica ligera.
 * - 2000: Dieta estándar para personas con actividad fisica moderada.
 * - 2500: Dieta alta en calorias, recomendada para personas con alta actividad fisica.
 * - 3000: Dieta rica en calorias, utilizada para aumentar masa muscular.
 * - 3500: Dieta muy alta en calorias, ideal para ganar peso de manera intensiva.
 */
calorias(['1200']).
calorias(['1400']).
calorias(['1500']).
calorias(['1600']).
calorias(['1800']).
calorias(['2000']).
calorias(['2500']).
calorias(['3000']).
calorias(['3500']).

/** 
 * Definición de los tipos de dieta disponibles.
 * Los tipos de dieta y sus aplicaciones son:
 * - proteica: Dieta alta en proteinas, recomendada para ganar masa muscular.
 * - hipocalorica: Dieta baja en calorias, indicada para personas con sobrepeso, obesidad, diabetes, colesterol alto o hipertension arterial.
 * - hipercalorica: Dieta alta en calorias, recomendada para personas con desnutrición o que buscan aumentar masa muscular.
 * - detox: Dieta de desintoxicacion, utilizada en casos de trastornos digestivos, enfermedades autoinmunes, fibromialgia, sindrome de fatiga cronica, o inflamacion.
 * - mediterranea: Dieta balanceada, indicada para prevenir o tratar enfermedades cardiovasculares y cerebrovasculares.
 * - keto: Dieta cetogenica, baja en carbohidratos, ideal para bajar de peso y no recomendada para diabeticos.
 * - vegetariana: Dieta basada en alimentos de origen vegetal, recomendada para personas que evitan el consumo de carne.
 * - alcalina: Dieta diseñada para bajar de peso y regular los niveles de pH en el cuerpo.
 */
tipodieta([proteica]).
tipodieta([hipocalorica]).      
tipodieta([hipercalorica]).     
tipodieta([detox]).             
tipodieta([mediterranea]).      
tipodieta([keto]).              
tipodieta([vegetariana]).       
tipodieta([alcalina]).          

/**
 * Padecimientos de las dietas hipocaloricas.
 * Definición de padecimientos que se benefician o se relacionan con una dieta hipocalorica.
 * Sobrepeso: Condicion de exceso de peso que se recomienda tratar con dieta hipocalorica.
 * Obesidad: Condición de obesidad que puede ser tratada con una dieta hipocalorica.
 * Diabetes: Condicion que se puede manejar con una dieta baja en calorias.
 * Colesterol: Nivel alto de colesterol, recomendado para dieta hipocalorica.
 * Hipertension: Condicion de alta presión arterial, recomendada para dieta hipocalorica.
 */
padecimientos([sobrepeso]).             
padecimientos([obesidad]).              
padecimientos([diabetes]).              
padecimientos([colesterol]).            
padecimientos([hipertension]).          

/**
 * Padecimientos relacionados con dietas hipercaloricas.
 * Condiciones que requieren una dieta hipercalorica para el tratamiento o mejora de los sintomas.
 * Desnutricion: Condicion de falta de nutrientes, requiere dieta hipercalorica.
 * Cancer: Enfermedad grave que puede requerir una dieta hipercalorica para mantener la energia.
 * Sida: Enfermedad viral donde es recomendable una dieta alta en calorias.
 */
padecimientos([desnutricion]).          
padecimientos([cancer]).                
padecimientos([sida]).                  

/**
 * Padecimientos asociados a las dietas detox.
 * Padecimientos que pueden beneficiarse de una dieta detox.
 * Fibromialgia: Dolor cronico en músculos y articulaciones, dieta detox recomendada.
 * Fatiga Sensacion extrema de cansancio, tratable con dieta detox.
 * Inflamacion: Inflamaciones cronicas pueden beneficiarse de una dieta detox.
 * Enfermedadautoinmune: Condiciones autoinmunes que pueden mejorar con dieta detox.
 * Transtornossistemadigestivo: Problemas digestivos tratados con dieta detox.
 */
padecimientos([fibromialgia]).          
padecimientos([fatiga]).                
padecimientos([inflamacion]).           
padecimientos([enfermedadautoinmune]).  
padecimientos([transtornossistemadigestivo]).  

/**
 * Padecimiento para dietas mediterraneas.
 * Condiciones tratadas con una dieta de estilo mediterraneo.
 * Enfermedadcardiovascular: Enfermedades del corazon, dieta mediterranea recomendada.
 */
padecimientos([enfermedadcardiovascular]).     

/**
 * Padecimientos para dietas Keto.
 * Condiciones que pueden tratarse con una dieta cetogenica (Keto).
 * Obesidad Obesidad tratada con dieta baja en carbohidratos.
 * Epilepsiainfantil: Trastorno neurologico en niños, donde la dieta Keto es recomendada.
 * Hiperlipidemia: Condicion de lipidos altos en sangre, tratada con dieta cetogénica.
 */
padecimientos([obesidad]).              
padecimientos([epilepsiainfantil]).     
padecimientos([hiperlipidemia]).        

/**
 * Padecimientos asociados a las dietas vegetarianas.
 * Condiciones tratadas con una dieta vegetariana.
 * Hipertension: Alta presion arterial, tratada con dieta vegetariana.
 * Diabetestipo2: Diabetes tipo 2, tratada con dieta vegetariana.
 * Obesidad: Obesidad que puede ser tratada con una dieta vegetariana.
 */
padecimientos([hipertension]).          
padecimientos([diabetestipo2]).         
padecimientos([obesidad]).              

/**
 * Padecimientos tratados con dietas alcalinas.
 * Condiciones relacionadas con los niveles de pH, que pueden ser tratadas con una dieta alcalina.
 * Phalto: Niveles altos de pH, dieta alcalina recomendada.
 * Phbajo: Niveles bajos de pH, dieta alcalina recomendada.
 */
padecimientos([phalto]).        
padecimientos([phbajo]).     



/**
* Definición del menú de cada una de las dietas posibles.
*/

/**
* Dieta Proteica 2000cal (Enfocada en el consumo de proteina)
*/
menuProte1(Menu) :-
    string_concat("Desayuno: Avena con leche, banano y frutas. 2 jamón, 2 queso.\n", "Merienda: Emparedado con batido de frutas.\n", Temp1),
    string_concat(Temp1, "Almuerzo: 150g carne de res, 150g verduras, 100g de frijoles.\n", Temp2),
    string_concat(Temp2, "Merienda: Fruta y 10 almendras.\n", Temp3),
    string_concat(Temp3, "Cena: 100g carne de cerdo, 2 huevos, 50g de lentejas, ensalada.\n", Temp4),
    string_concat(Temp4, "Que lo disfrute.\n", Menu).

/**
* Dieta Proteica 2500cal (Enfocada en el consumo de proteina)
*/
menuProte2(Menu) :-
    string_concat("Desayuno: 2 huevos revueltos con poco aceite, 4 tostadas, café con leche sin grasa.\n", "", Temp1),
    string_concat(Temp1, "Merienda: 200g de avena con leche, 1 fruta.\n", Temp2),
    string_concat(Temp2, "Almuerzo: Garbanzos, 2 huevos, 200g de pollo, 50g de arroz.\n", Temp3),
    string_concat(Temp3, "Merienda: Emparedado de jamón y 2 rebanadas de queso.\n", Temp4),
    string_concat(Temp4, "Cena: 150g de salmón, verduras al vapor, 200g de arroz, ensalada.\n", Temp5),
    string_concat(Temp5, "Que lo disfrute.\n", Menu).

/**
* Dieta Alcalina 1800cal 
*/
menuAlca1(Menu) :-
    string_concat("Desayuno: Zumo de tomate, 200g de cereales de avena con leche de almendras, 200 ml de té verde.\n", "", Temp1),
    string_concat(Temp1, "Merienda: 80g de fruta.\n", Temp2),
    string_concat(Temp2, "Almuerzo: 350g de sopa de garbanzos, 150ml de manzanilla.\n", Temp3),
    string_concat(Temp3, "Merienda: 300ml de batido de leche de almendras con fresas.\n", Temp4),
    string_concat(Temp4, "Cena: 400g de sopa de pollo con arroz integral y cebolla, 200ml de té de tilo.\n", Temp5),
    string_concat(Temp5, "Que lo disfrute.\n", Menu).

/**
* Dieta Alcalina 1500cal 
*/
menuAlca2(Menu) :-
    string_concat("Desayuno: 250 ml de batido verde de aguacate, limón y acelgas, 150g de tostada de pan integral con pechuga de pavo, 150 ml de té verde.\n", "", Temp1),
    string_concat(Temp1, "Merienda: 200ml de infusión.\n", Temp2),
    string_concat(Temp2, "Almuerzo: 300g de pasta integral con salteado de pescado, 200ml de manzanilla.\n", Temp3),
    string_concat(Temp3, "Merienda: 200ml de zumo de naranja.\n", Temp4),
    string_concat(Temp4, "Cena: 300g de hervido de verduras con patata, cebolla y brócoli.\n", Temp5),
    string_concat(Temp5, "Que lo disfrute.\n", Menu).

/**
* Dieta Mediterranea 3000cal 
*/
menuMedi1(Menu) :-
    string_concat("Desayuno: 1 tortilla francesa, 1 kiwi, café o infusión.\n", "", Temp1),
    string_concat(Temp1, "Merienda: 2 tostadas, 50g de queso fresco, 25g de nueces.\n", Temp2),
    string_concat(Temp2, "Almuerzo: 250g de gazpacho, 100g de pescado a la plancha, 50g de frutos secos.\n", Temp3),
    string_concat(Temp3, "Merienda: 150g de taboulé a la plancha, 30g de plátano.\n", Temp4),
    string_concat(Temp4, "Cena: 150g de verduras asadas, 100g de pulpo a la brasa, 1 manzana con canela.\n", Temp5),
    string_concat(Temp5, "Que lo disfrute.\n", Menu).

/**
* Dieta Mediterranea 1500cal 
*/
menuMedi2(Menu) :-
    string_concat("Desayuno: 60g de jamón serrano, café sin azúcar, 1 mandarina.\n", "", Temp1),
    string_concat(Temp1, "Merienda: 100g de yogur, 50g de fruta fresca.\n", Temp2),
    string_concat(Temp2, "Almuerzo: 200g de espinacas con garbanzos, 1 manzana.\n", Temp3),
    string_concat(Temp3, "Merienda: 100g de macedonia, café sin azúcar.\n", Temp4),
    string_concat(Temp4, "Cena: 150g de ensalada (tomate, lechuga y cebolla), 50g de sardinas, 25g de cuajada con miel.\n", Temp5),
    string_concat(Temp5, "Que lo disfrute.\n", Menu).

/**
* Dieta Vegetariana 1800cal 
*/
menuVege1(Menu) :-
    string_concat("Desayuno: 120g de yogur de soja con miel y fruta.\n", "", Temp1),
    string_concat(Temp1, "Merienda: 40g de pan integral con mermelada.\n", Temp2),
    string_concat(Temp2, "Almuerzo: 80g de lentejas cocidas, 120g de calabacín, 25g de cebolla, aceite de oliva, 70g de pan integral, 80g de tomate picado, 40g de fruta.\n", Temp3),
    string_concat(Temp3, "Merienda: Frutos secos.\n", Temp4),
    string_concat(Temp4, "Cena: 200g de tomate con 5 nueces, 50g de tofu, 20g de cebolla, 20g de calabacín, 200g de papas al horno.\n", Temp5),
    string_concat(Temp5, "Que lo disfrute.\n", Menu).

/**
* Dieta Vegetariana 2500cal 
*/
menuVege2(Menu) :-
    string_concat("Desayuno: 40g de tostadas integrales, 200ml de batido de soja con cacao en polvo, 40g de fruta.\n", "", Temp1),
    string_concat(Temp1, "Merienda: 40g de frutos secos.\n", Temp2),
    string_concat(Temp2, "Almuerzo: 80g de arroz, 30g de hongos, 30g de cebolla, 3 canelones, 100g de bechamel, 200g de vegetales salteados.\n", Temp3),
    string_concat(Temp3, "Merienda: 2 yogures de soja (150g), 40g de galletas integrales.\n", Temp4),
    string_concat(Temp4, "Cena: 60g de arroz, 100g de espárragos, 45g de cebolla, 30g de zanahoria, 120g de tomate, 100ml de vino blanco, 70g de pan integral, 40g de fruta.\n", Temp5),
    string_concat(Temp5, "Que lo disfrute.\n", Menu).

/**
* Dieta Keto 1800cal 
*/
menuKeto1(Menu) :-
    string_concat("Desayuno: Tortitas (2 huevos, 2 cucharadas de aceite de oliva, 2 cucharadas de jarabe de arce, esencia de vainilla, 1 cucharada de levadura en polvo, 120g de harina, leche de almendra).\n", "", Temp1),
    string_concat(Temp1, "Merienda: Chips de parmesano (50g de queso parmesano, albahaca, sal, pimienta).\n", Temp2),
    string_concat(Temp2, "Almuerzo: 1 filete de salmón, 100g de brócoli, 3 cucharadas de almendras, pimienta, nuez moscada, canela, 20g de kale.\n", Temp3),
    string_concat(Temp3, "Merienda: 100g de cecina o jamón.\n", Temp4),
    string_concat(Temp4, "Cena: 150g de ternera, 60g de col pak choi, 2 unidades de citronela, 1 manzana con canela, medio pimiento rojo, 50g de kale, caldo de huesos.\n", Temp5),
    string_concat(Temp5, "Que lo disfrute.\n", Menu).

/**
* Dieta Keto 1400cal 
*/
menuKeto2(Menu) :-
    string_concat("Desayuno: 250g de yogur de coco, 1 cucharada de proteína, 30g de almendras, 20g de mantequilla.\n", "", Temp1),
    string_concat(Temp1, "Merienda: 100g de cecina o jamón.\n", Temp2),
    string_concat(Temp2, "Almuerzo: Ensalada de pastrami con 50g de crema de queso.\n", Temp3),
    string_concat(Temp3, "Merienda: Brócoli picante (80g de brócoli asado, 10g de chile).\n", Temp4),
    string_concat(Temp4, "Cena: Gambas con arroz de coliflor (100g de coliflor, 1 cucharada de aceite de coco, 50g de judías verdes, 150g de gambas, 50g de crema de queso).\n", Temp5),
    string_concat(Temp5, "Que lo disfrute.\n", Menu).
/**
* Dieta Detox 1400cal 
*/
menuDetox1(Menu) :-
    string_concat("Desayuno: Té verde, avena con leche de soja, arándanos.\n", "", Temp1),
    string_concat(Temp1, "Merienda: Tomate natural con perejil.\n", Temp2),
    string_concat(Temp2, "Almuerzo: Alcachofas al vapor, batido de apio y piña, infusión de té verde.\n", Temp3),
    string_concat(Temp3, "Merienda: Zumo de naranja y piña.\n", Temp4),
    string_concat(Temp4, "Cena: Batido detox (apio, tomate, pepino, zumo de arándano).\n", Temp5),
    string_concat(Temp5, "Que lo disfrute.\n", Menu).

/**
* Dieta Detox 1600cal 
*/
menuDetox2(Menu) :-
    string_concat("Desayuno: Té verde, 30g de pan con tomate y aceite de oliva, arándanos.\n", "", Temp1),
    string_concat(Temp1, "Merienda: Té verde, 10 almendras.\n", Temp2),
    string_concat(Temp2, "Almuerzo: Ensalada de espinacas con ajo, semillas de mostaza al vapor, merluza a la plancha, zumo de naranja.\n", Temp3),
    string_concat(Temp3, "Merienda: Batido de papaya y piña.\n", Temp4),
    string_concat(Temp4, "Cena: Batido detox (pepino, piña, semillas de sésamo, uvas).\n", Temp5),
    string_concat(Temp5, "Que lo disfrute.\n", Menu).

/**
* Dieta Hipercalorica 2500cal 
*/
menuHiper1(Menu) :-
    string_concat("Desayuno: 200ml de leche, 60g de cereales integrales, 1 yogur, 2 piezas de fruta, café solo o infusión.\n", "", Temp1),
    string_concat(Temp1, "Merienda: 60g de pan, 90g de jamón york, 150g de tomate, café o infusión con 100ml de leche.\n", Temp2),
    string_concat(Temp2, "Almuerzo: 300g de coliflor hervida, 15g de jamón serrano en tacos, 150g de pollo asado, 150g de patatas, 60g de pan, 400g de melón.\n", Temp3),
    string_concat(Temp3, "Merienda: 30g de cereales de desayuno, 75g de nueces, 160g de manzana, 200ml de leche, café solo o infusión.\n", Temp4),
    string_concat(Temp4, "Cena: 300g de verduras, 50g de guisantes, 35g de requesón, 150g de sardinas a la plancha, 150g de espárragos, 100g de patatas cocidas, 40g de pan, 300g de mandarinas.\n", Temp5),
    string_concat(Temp5, "Que lo disfrute.\n", Menu).

/**
* Dieta Hipercalorica 3500cal 
*/
menuHiper2(Menu) :-
    string_concat("Desayuno: 2 yogures desnatados, 100g de galletas, 3 piezas de fruta, café con 100ml de leche.\n", "", Temp1),
    string_concat(Temp1, "Merienda: 100g de galletas, 100g de almendras, 100g de zanahoria cruda, 2 yogures, café solo o infusión.\n", Temp2),
    string_concat(Temp2, "Almuerzo: 100g de arroz, 3 huevos fritos, 150g de tomate, 200g de pollo a la plancha, 100g de pan, 150g de verduras al horno, 100g de cerezas.\n", Temp3),
    string_concat(Temp3, "Merienda: 100g de pan, 100g de jamón york, café solo o infusión con 200ml de leche, 2 piezas de fruta pequeñas.\n", Temp4),
    string_concat(Temp4, "Cena: 250g de alcachofas, 250g de salmón a la plancha, 150g de puré de patatas (con 100ml de leche), 100g de pan, 240g de manzana.\n", Temp5),
    string_concat(Temp5, "Que lo disfrute.\n", Menu).

/**
* Dieta Hipocalorica 1200cal 
*/
menuHipo1(Menu) :-
    string_concat("Desayuno: 1 vaso de leche desnatada, 40g de cereales, café o infusión.\n", "", Temp1),
    string_concat(Temp1, "Merienda: 20g de pan, 60g de jamón york, café o infusión.\n", Temp2),
    string_concat(Temp2, "Almuerzo: 150g de coliflor hervida, 75g de pollo asado, 100g de patatas, 20g de pan, 200g de piña natural.\n", Temp3),
    string_concat(Temp3, "Merienda: 20g de pan, 50g de queso, café o infusión.\n", Temp4),
    string_concat(Temp4, "Cena: 15g de pasta, 50g de puerros, 25g de zanahorias, 110g de pescado, 75g de lechuga, 75g de tomate, 20g de pan, 50g de cerezas.\n", Temp5),
    string_concat(Temp5, "Que lo disfrute.\n", Menu).

/**
* Dieta Hipocalorica 1500cal 
*/
menuHipo2(Menu) :-
    string_concat("Desayuno: 2 yogures desnatados, 50g de galletas, café o infusión.\n", "", Temp1),
    string_concat(Temp1, "Merienda: 50g de biscotes, 100g de queso fresco, café o infusión.\n", Temp2),
    string_concat(Temp2, "Almuerzo: 150g de verduras, 100g de albóndigas, 50g de arroz, 50g de pan, 200g de fresas.\n", Temp3),
    string_concat(Temp3, "Merienda: 25g de galletas, café con 100ml de leche desnatada.\n", Temp4),
    string_concat(Temp4, "Cena: 150g de lechuga, 150g de tomate, 100g de patatas, 200g de naranja, 3 huevos.\n", Temp5),
    string_concat(Temp5, "Que lo disfrute.\n", Menu).
