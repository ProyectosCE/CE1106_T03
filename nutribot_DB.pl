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
dieta(['keto1', keto, 1800, inicial, [hiperlipidemia], [hipertension], hit, suiza, menuKeto1]).
dieta(['keto2', keto,1400, inicial, [hiperlipidemia], [hipertension], hit, suiza, menuKeto2]).

/**
 * Dietas Detox:
 * - Dieta desintoxicante, recomendada para niveles iniciales de actividad fisica.
 */
dieta(['detox1', detox, 1400, inicial, [diabetes], [fibromialgia], crossfit, caminata, menuDetox1]).
dieta(['detox2', detox, 1600, inicial, [diabetes], [fibromialgia], crossfit, caminata, menuDetox2]).

/**
 * Dietas Hipercaloricas:
 * - Dieta rica en calorias, ideal para ganar masa muscular o tratar la desnutricion.
 */
dieta(['hipercalorica1', hipercalorica, 2500, intermedio, [obesidad], [desnutricion], powerlifting, triatlon, menuHiper1]).
dieta(['hipercalorica2', hipercalorica, 3500, avanzado, [obesidad], [desnutricion], powerlifting, triatlon, menuHiper2]).

/**
 * Dietas Hipocaloricas:
 * - Dieta baja en calorias, ideal para perder peso.
 */
dieta(['hipocalorica1', hipocalorica, 1200, inicial, [desnutricion], [sobrepeso], triatlon, yoga, menuHipo1]).
dieta(['hipocalorica2', hipocalorica, 1500, inicial, [desnutricion], [sobrepeso], triatlon, yoga, menuHipo2]).

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
    Menu = [
        'Desayuno',
        'Avena con leche, banano y frutas.',
        '2 jamon.',
        '2 queso.',
        '****************',
        'Merienda',
        'Emparedado con batido de frutas.',
        '****************',
        'Almuerzo',
        '150g carne de res.',
        '150g verduras.',
        '100g de frijoles.',
        '****************',
        'Merienda',
        'Fruta y 10 almendras.',
        '****************',
        'Cena',
        '100g carne de cerdo.',
        '2 huevos.',
        '50g de lentejas.',
        'Ensalada.',
        '****************',
        'Que lo disfrute'
    ].

/**
* Dieta Proteica 2500cal (Enfocada en el consumo de proteina)
*/                                                                                                                                                                                                                                                                                                                     
menuProte2(Menu):-
  Menu=[
 'Desayuno',
 '2 huevos revueltos poco aceite.',
'4 tostadas',
 'cafe con leche sin grasa.',
 '****************',
'Merienda',
 '200g Avena con leche',
 '1 fruta',
 '****************',
 'Almuerzo',
 'Garbanzos',
 '2 huevos',
 '200g Pollo',
 '50g gramos de arroz',
 '****************',
 'Merienda',
 'Emparedado de jamon',
 '2 rebanadas de queso',
 '****************',
 'Cena',
 '150g de salmon',
 'Verduras al vapor',
 '200g arroz',
 'Ensalada',
 '****************',
 'Que lo disfrute']. 
                                                                                                           
/**
* Dieta Alcalina 1800cal 
*/            
menuAlca1(Menu) :-
    Menu = [
        'Desayuno',
        'Zumo de tomate',
        '200g de cereales de avena con leche de almendras',
        '200 ml de te verde',
        '****************',
        'Merienda',
        '80g de fruta',
        '****************',
        'Almuerzo',
        '350g de sopa de garbanzos',
        '150ml de manzanilla',
        '****************',
        'Merienda',
        '300ml Batido de leche de almendras con fresas',
        '****************',
        'Cena',
        '400g de sopa de pollo con arroz integral y cebolla',
        '200ml de te de tilo.',
        '****************',
        'Que lo disfrute'
    ].

/**
* Dieta Alcalina 1500cal 
*/                                                                                                                                                                                                                                                                           
menuAlca2(Menu) :-
    Menu = [
        'Desayuno',
        '250 ml Batido verde de aguacate, limón y acelgas',
        '150g Tostada de pan integral con pechuga de pavo',
        '150 ml Té verde',
        '****************',
        'Merienda',
        '200ml de infusión',
        '****************',
        'Almuerzo',
        '300g de pasta integral con salteado de pescado',
        '200ml de manzanilla',
        '****************',
        'Merienda',
        '200ml Zumo de naranja',
        '****************',
        'Cena',
        '300g Hervido de verduras con patata, cebolla y brocoli',
        '****************',
        'Que lo disfrute'
    ].
                                         
/**
* Dieta Mediterranea 3000cal 
*/ 
menuMedi1(Menu) :-
    Menu = [
        'Desayuno',
        '1 tortilla francesa',
        '1 kiwi',
        'cafe o infusion',
        '****************',
        'Merienda',
        '2 Tostadas',
        '50g de Queso fresco',
        '25g de nueces',
        '****************',
        'Almuerzo',
        '250g de Gazpacho',
        '100g Pescado a la plancha',
        '50g de Frutos secos',
        '****************',
        'Merienda',
        '150g de Taboule a la plancha',
        '30g de platano',
        '****************',
        'Cena',
        '150g de Verduras asadas',
        '100g de Pulpo a la brasa',
        '1 manzana con canela',
        '****************',
        'Que lo disfrute'
    ].
                 
/**
* Dieta Mediterranea 1500cal 
*/ 
menuMedi2(Menu) :-
    Menu = [
        'Desayuno',
        '60 g de jamon serrano.',
        'Cafe sin azucar.',
        '1 mandarina.',
        '****************',
        'Merienda',
        '100g de yogurt.',
        '50g de fruta fresca.',
        '****************',
        'Almuerzo',
        '200g de Espinacas con garbanzos.',
        '1 manzana.',
        '****************',
        'Merienda',
        '100g de macedonia.',
        'Cafe sin azucar.',
        '****************',
        'Cena',
        '150g de Ensalada (Tomate, lechuga y cebolla).',
        '50g de Sardinas.',
        '25g de Cuajada con miel.',
        '****************',
        'Que lo disfrute'
    ].

/**
* Dieta Vegetariana 1800cal 
*/ 
menuVege1(Menu) :-
    Menu = [
        'Desayuno',
        '120g de yogurt de soja con miel y fruta.',
        '****************',
        'Merienda',
        '40g Pan integral con mermelada',
        '****************',
        'Almuerzo',
        '80g de lentejas cocidas',
        '120g de calabacin',
        '25g de cebolla',
        'Aceite de oliva',
        '70g de pan integral',
        '80g de tomate picado',
        '40g de fruta',
        '****************',
        'Merienda',
        'Frutos secos',
        '****************',
        'Cena',
        '200g de tomate con 5 nueces',
        '50g de tofu',
        '20g de cebolla',
        '20g de calabacin',
        '200g de papas al horno',
        '****************',
        'Que lo disfrute'
    ].
    
/**
* Dieta Alcalina 2500cal 
*/ 
menuVege2(Menu) :-
    Menu = [
        'Desayuno',
        '40g tostadas integrales',
        '200 ml de batido de soja con cacao en polvo',
        '40g de fruta',
        '****************',
        'Merienda',
        '40 gramos de frutos secos',
        '****************',
        'Almuerzo',
        '80g de arroz',
        '30g de hongos',
        '30g de cebolla',
        '3 canelones',
        '100 de bechamel',
        '200g de vegetales salteados',
        '****************',
        'Merienda',
        '2 yogures de soja (150g)',
        '40g de galletas integrales',
        '****************',
        'Cena',
        '60g de arroz',
        '100 g de espárragos',
        '45g de cebolla',
        '30g de zanahoria',
        '120g de tomate',
        '100ml de vino blanco',
        '70g de pan integral',
        '40g de fruta',
        '****************',
        'Que lo disfrute'
    ].

/**
* Dieta Keto 1800cal 
*/ 
menuKeto1(Menu) :-
    Menu = [
        'Desayuno',
        'Tortitas',
        '2 huevos',
        '2 cucharadas de aceite de oliva',
        '2 cucharadas de jarabe de arce',
        'Esencia de vainilla',
        '1 cucharada de levadura en polvo',
        '120g de harina',
        'leche de almendra',
        '2 cucharadas de mantequilla',
        '****************',
        'Merienda',
        'Chips de parmesano',
        '50g queso parmesano',
        'Albahaca',
        'Sal',
        'Pimienta',
        '****************',
        'Almuerzo',
        '1 filete de salmon',
        '100g de brocoli',
        '3 cucharadas de almendras',
        'Pimienta',
        'Nuez moscada',
        'Canela',
        '20g de kale',
        '****************',
        'Merienda',
        '100g de cecina o jamon',
        '****************',
        'Cena',
        '150g de ternera',
        '60g de col pak choi',
        '2 unidades de citronela',
        '1 manzana con canela',
        'Medio pimiento rojo',
        '50g de kale',
        'Caldo de huesos',
        '****************',
        'Que lo disfrute'
    ].
    
/**
* Dieta Alcalina 1400cal 
*/ 
menuKeto2(Menu) :-
    Menu = [
        'Desayuno',
        '250 gr de yogur de coco',
        '1 cucharada de proteína',
        '30g de almendras',
        '20g de mantequilla',
        '****************',
        'Merienda',
        '100g de cecina o jamón',
        '****************',
        'Almuerzo',
        'Ensalada de pastrami',
        '50 gr de crema de queso',
        '****************',
        'Merienda',
        'Brócoli picante',
        '80g de brócoli asado',
        '10g de chile cortados en trozos y asados',
        '****************',
        'Cena',
        'Gambas con arroz de coliflor',
        '100g de coliflor',
        '1 cucharada de aceite de coco',
        '50g de judías verdes',
        '150g de gambas',
        '50g de crema de queso',
        '1 puñado de cacahuetes machacados',
        '10g de cebollino',
        '****************',
        'Que lo disfrute'
    ].
/**
* Dieta Detox 1400cal 
*/ 
menuDetox1(Menu) :-
    Menu = [
        'Desayuno',
        'Té verde.',
        'Avena con leche de soja.',
        'Arándanos.',
        '****************',
        'Merienda',
        'Tomate natural con perejil.',
        '****************',
        'Almuerzo',
        'Alcachofas al vapor',
        'Batido de apio y piña',
        'Infusión de té verde',
        '****************',
        'Merienda',
        'Zumo de naranja y piña',
        '****************',
        'Cena',
        'Batido detox (apio, tomate, pepino, zumo de arándano)',
        '****************',
        'Que lo disfrute'
    ].

/**
* Dieta Detox 1600cal 
*/ 
menuDetox2(Menu) :-
    Menu = [
        'Desayuno',
        'Té verde.',
        '30g de pan con tomate y aceite de oliva',
        'Arándanos.',
        '****************',
        'Merienda',
        'Té verde.',
        '10 almendras.',
        '****************',
        'Almuerzo',
        'Ensalada de espinacas con ajo.',
        'Semillas de mostaza al vapor.',
        'Merluza a la plancha',
        'Zumo de naranja.',
        '****************',
        'Merienda',
        'Batido de papaya y piña',
        '****************',
        'Cena',
        'Batido detox (pepino, piña, semillas de sésamo, uvas)',
        '****************',
        'Que lo disfrute'
    ].               
              
/**
* Dieta Hipercalorica 2500cal 
*/ 
menuHiper1(Menu) :-
    Menu = [
        'Desayuno',
        '200 ml de leche',
        '60 g de cereales integrales.',
        '1 yogurt.',
        '2 piezas de fruta.',
        'Café solo o infusión.',
        '****************',
        'Merienda',
        '60 g de pan.',
        '90 g de jamón york.',
        '150 g de tomate.',
        'Café o infusión con 100 ml de leche.',
        '****************',
        'Almuerzo',
        '300 g de coliflor hervida',
        '15 g de jamón serrano en tacos.',
        '150 g de pollo asado.',
        '150 g de patatas.',
        '60 g de pan.',
        '400 g de melón.',
        '****************',
        'Merienda',
        '30 g de cereales de desayuno',
        '75 g de nueces',
        '160 g de manzana',
        '200 ml de leche.',
        'Café solo o infusión.',
        '****************',
        'Cena',
        '300 g de verduras',
        '50 g de guisantes',
        '35 g de requesón.',
        '150 g de sardinas a la plancha.',
        '150 g de espárragos',
        '100 g de patatas cocidas.',
        '40 g de pan.',
        '300 g de mandarinas.',
        '****************',
        'Que lo disfrute'
    ].
            
/**
* Dieta Hipercalorica 3500cal 
*/ 
menuHiper2(Menu) :-
    Menu = [
        'Desayuno',
        '2 yogures desnatados.',
        '100 g de galletas.',
        '3 piezas de fruta.',
        'Café con 100 ml de leche.',
        '****************',
        'Merienda',
        '100 g de galletas.',
        '100 g de almendras.',
        '100 g de zanahoria cruda.',
        '2 yogures.',
        'Café solo o infusión.',
        '****************',
        'Almuerzo',
        '100 g de arroz.',
        '3 huevos fritos.',
        '150 g de tomate.',
        '200 g de pollo a la plancha.',
        '100 g de pan.',
        '150 g de verduras al horno.',
        '100 g de cerezas.',
        '****************',
        'Merienda',
        '100 g de pan.',
        '100 g de jamón york.',
        'Café solo o infusión con 200 ml de leche.',
        '2 piezas de fruta pequeña.',
        '****************',
        'Cena',
        '250 g de alcachofas.',
        '250 g de salmón a la plancha.',
        '150 g de puré de patatas (con 100 ml de leche).',
        '100 g de pan.',
        '240 g de manzana.',
        '****************',
        'Que lo disfrute'
    ].       
   
/**
* Dieta Hipocalorica 1200cal 
*/ 
menuHipo1(Menu) :-
    Menu = [
        'Desayuno',
        '1 vaso de leche desnatada.',
        '40 g de cereales.',
        'Café o infusión.',
        '****************',
        'Merienda',
        '20 g de pan.',
        '60 g de jamón york.',
        'Café o infusión.',
        '****************',
        'Almuerzo',
        '150 g de coliflor hervida.',
        '75 g de pollo asado.',
        '100 g de patatas.',
        '20 g de pan.',
        '200 g de piña natural.',
        '****************',
        'Merienda',
        '20 g de pan.',
        '50 g de queso.',
        'Café o infusión.',
        '****************',
        'Cena',
        '15 g de pasta.',
        '50 g de puerros.',
        '25 g de zanahorias.',
        '110 g de pescado.',
        '75 g de lechuga.',
        '75 g de tomate.',
        '20 g de pan.',
        '50 g de cerezas.',
        '****************',
        'Que lo disfrute'
    ].

/**
* Dieta Hipocalorica 1500cal 
*/ 
menuHipo2(Menu) :-
    Menu = [
        'Desayuno',
        '2 yogures desnatados.',
        '50 g de galletas.',
        'Café o infusión.',
        '****************',
        'Merienda',
        '50 g de biscotes.',
        '100 g de queso fresco.',
        'Café o infusión.',
        '****************',
        'Almuerzo',
        '150 g de verduras.',
        '100 g de albóndigas.',
        '50 g de arroz.',
        '50 g de pan.',
        '200 g de fresas.',
        '****************',
        'Merienda',
        '25 g de galletas.',
        'Café con 100 ml de leche desnatada.',
        '****************',
        'Cena',
        '150 g de lechuga.',
        '150 g de tomate.',
        '100 g de patatas.',
        '200 g de naranja.',
        '3 huevos.',
        '****************',
        'Que lo disfrute'
    ].