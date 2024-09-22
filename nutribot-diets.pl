/** Base de datos de dietas
 *
 * Esta base de datos contiene diferentes planes de dieta estructurados segun
 * distintos objetivos nutricionales y estilos de vida. Cada entrada en la base
 * de datos describe un plan de comidas detallado para un dia completo.
 *
 * Ejemplo de uso:
 * ==
 * ?- dieta('Dieta Cetogenica (Keto, baja en carbohidratos)', Descripcion).
 * Descripcion = 'Desayuno: 3 huevos revueltos (150g) con 30g de queso cheddar...'.
 * ==
 */

/**
 * Dieta Cetogenica (Keto, baja en carbohidratos)
 * Descripcion: Dieta alta en grasas y baja en carbohidratos, ideal para inducir cetosis y promover la perdida de peso.
 */
dieta('Dieta Cetogenica (Keto, baja en carbohidratos)',
    'Desayuno: 3 huevos revueltos (150g) con 30g de queso cheddar, 50g de espinacas, y 2 tiras de tocino (30g). Merienda Manana: 30g de nueces y 30g de queso. Almuerzo: Ensalada de 150g de atun, 100g de aguacate, 20g de mayonesa, y 10 aceitunas (30g). Merienda Tarde: 100g de yogurt griego natural con 10g de semillas de chia. Cena: 150g de salmon a la plancha con 100g de brocoli y 10g de mantequilla.').


/** Dieta Vegetariana (Moderada en carbohidratos)
 *
 * Plan de comidas diario basado en una dieta vegetariana con moderada cantidad
 * de carbohidratos.
 */
dieta('Dieta Vegetariana (Moderada en carbohidratos)',
    'Desayuno: 50g de avena cocida con 1 platano (120g) y 20g de nueces. Merienda Manana: 1 manzana (150g) y 30g de almendras. Almuerzo: 100g de garbanzos cocidos con 150g de quinoa y 100g de vegetales al vapor (brocoli, zanahorias). Merienda Tarde: 200g de yogurt natural con 50g de fresas. Cena: 2 tortillas integrales (60g) con 100g de tofu, 50g de aguacate, lechuga (30g), y tomate (50g).').

/** Dieta Vegana (Alta en proteinas y carbohidratos)
 *
 * Dieta vegana enfocada en una alta ingesta de proteinas y carbohidratos.
 */
dieta('Dieta Vegana (Alta en proteínas y carbohidratos)',
    'Desayuno: 200g de tofu revuelto con espinacas (100g) y champinones (50g). Merienda Manana: 1 batido con 200g de leche de almendra, 1 banana (120g), y 30g de mantequilla de almendra. Almuerzo: 150g de lentejas cocidas con 100g de arroz integral y 100g de brocoli. Merienda Tarde: 50g de hummus con zanahorias (100g) y pepino (50g). Cena: 150g de tempeh salteado con 100g de quinoa y 100g de calabacin.').

/** Dieta Alcalina (Baja en proteinas animales, alta en verduras)
 *
 * Plan de comidas que sigue los principios de la dieta alcalina, promoviendo
 * alimentos bajos en proteinas animales y altos en verduras.
 */
dieta('Dieta Alcalina (Baja en proteinas animales, alta en verduras)',
    'Desayuno: Batido verde de espinacas (50g), pepino (50g), 1 manzana verde (150g), y agua de coco (200g). Merienda Manana: 1 punado de almendras (30g). Almuerzo: 150g de quinoa cocida con 100g de aguacate, 100g de tomate, y 50g de cebolla. Merienda Tarde: 1 batata cocida (100g). Cena: 200g de ensalada verde (espinacas, lechuga, pepino, zanahoria) con 50g de aguacate y 10g de aceite de oliva.').

/** Dieta Proteica (Para usuarios con alta actividad fisica)
 *
 * Dieta alta en proteinas y balanceada para usuarios con alta actividad fisica.
 */
dieta('Dieta Proteica (Para usuarios con alta actividad física)',
    'Desayuno: 4 claras de huevo (160g) y 1 huevo entero (50g) con 50g de pavo y 50g de espinacas. Merienda Manana: 30g de almendras y 1 batido de proteinas (con 30g de proteina en polvo). Almuerzo: 150g de pechuga de pollo a la plancha, 100g de arroz integral y 100g de brocoli. Merienda Tarde: 100g de requeson bajo en grasa con 100g de arandanos. Cena: 150g de salmon a la plancha con 150g de esparragos y 10g de aceite de oliva.').

/**
 * Dieta Baja en Calorias (1200-1400 kcal)
 * Descripcion: Esta dieta esta enfocada en la reduccion de calorias, incluyendo alimentos bajos en grasas y azucares.
 */
dieta('Dieta Baja en Calorias (1200-1400 kcal)',
    'Desayuno: 2 claras de huevo (80g) con espinacas (50g) y 1 rebanada de pan integral (30g). Merienda Manana: 1 manzana pequena (120g). Almuerzo: 100g de pechuga de pollo a la plancha, 50g de arroz integral, y 100g de brocoli. Merienda Tarde: 1 yogurt bajo en grasa (120g). Cena: Ensalada de 150g de lechuga, 50g de pepino, 50g de tomate, y 100g de salmon a la plancha.').

/**
 * Dieta Cetogenica (Para actividad fisica moderada)
 * Descripcion: La dieta cetogenica promueve el consumo de grasas y proteinas, restringiendo los carbohidratos para inducir cetosis.
 */
dieta('Dieta Cetogenica (Para actividad fisica moderada)',
    'Desayuno: 2 huevos revueltos (100g) con 50g de jamon de pavo y 30g de queso cheddar. Merienda Manana: 30g de almendras. Almuerzo: 150g de pechuga de pollo con 100g de esparragos y 50g de aguacate. Merienda Tarde: 50g de queso con 1 lonja de jamon (30g). Cena: 200g de pescado blanco con 100g de espinacas y 10g de aceite de oliva.').

/**
 * Dieta Baja en Carbohidratos (Para perder peso)
 * Descripcion: Enfocada en la reduccion de carbohidratos, esta dieta ayuda a la perdida de peso y el control de niveles de azucar en sangre.
 */
dieta('Dieta Baja en Carbohidratos (Para perder peso)',
    'Desayuno: 3 claras de huevo (120g) y 1 huevo entero (50g) con 30g de espinacas. Merienda Manana: 100g de yogurt griego sin azucar con 20g de nueces. Almuerzo: 100g de pechuga de pollo a la plancha, 50g de esparragos, y 50g de aguacate. Merienda Tarde: 1 batido de proteina (30g de proteina en polvo). Cena: 100g de pescado a la plancha con 100g de ensalada de espinacas.').

/**
 * Dieta Vegetariana (Para usuarios con actividad baja)
 * Descripcion: Dieta sin carne enfocada en alimentos ricos en nutrientes, ideal para usuarios con un nivel bajo de actividad fisica.
 */
dieta('Dieta Vegetariana (Para usuarios con actividad baja)',
    'Desayuno: 1 rebanada de pan integral (30g) con 50g de aguacate. Merienda Manana: 1 manzana pequena (100g). Almuerzo: 100g de tofu a la plancha con 100g de quinoa y 50g de esparragos. Merienda Tarde: 1 taza de te verde. Cena: 100g de tempeh con 100g de espinacas salteadas.').

/**
 * Dieta Proteica (Para usuarios con musculacion intensa)
 * Descripcion: Alta en proteinas para apoyar la construccion muscular y la recuperacion post-entrenamiento.
 */
dieta('Dieta Proteica (Para usuarios con musculacion intensa)',
    'Desayuno: 150g de claras de huevo con 100g de espinacas y 50g de jamon. Merienda Manana: 30g de nueces con 1 batido de proteinas. Almuerzo: 200g de carne de res magra, 100g de batata, y 100g de brocoli. Merienda Tarde: 100g de queso cottage con 50g de fresas. Cena: 150g de pescado a la plancha con 100g de ensalada verde.').

/**
 * Dieta Paleo (Alta en proteinas y grasas saludables)
 * Descripcion: Se basa en el consumo de alimentos enteros y naturales, excluyendo alimentos procesados, cereales y lacteos.
 */
dieta('Dieta Paleo (Alta en proteinas y grasas saludables)',
    'Desayuno: 3 huevos (150g) revueltos con 50g de espinacas y 50g de aguacate. Merienda Manana: 30g de nueces mixtas. Almuerzo: 150g de pechuga de pollo a la parrilla con 100g de batata al horno y 100g de ensalada de verduras (pepino, tomate, espinacas). Merienda Tarde: 1 manzana (150g) con 20g de mantequilla de almendra. Cena: 200g de salmon a la parrilla con 100g de esparragos salteados.').

/**
 * Dieta Vegana (Alta en carbohidratos para actividad fisica intensa)
 * Descripcion: Dieta sin productos de origen animal, dise�ada para soportar altos niveles de actividad fisica.
 */
dieta('Dieta Vegana (Alta en carbohidratos para actividad fisica intensa)',
    'Desayuno: 200g de avena cocida con 100g de arandanos y 20g de semillas de chia. Merienda Manana: 1 batido de 200g de leche de almendra, 1 banana (120g), 30g de mantequilla de mani. Almuerzo: 150g de lentejas cocidas con 100g de arroz integral y 100g de espinacas salteadas. Merienda Tarde: 50g de hummus con 100g de zanahorias y 50g de pepino. Cena: 150g de tofu salteado con 100g de quinoa y 100g de brocoli.').

/**
 * Dieta Mediterranea (Balanceada en grasas saludables)
 * Descripcion: Basada en la dieta tradicional del Mediterraneo, esta dieta es rica en grasas saludables, vegetales y proteinas magras.
 */
dieta('Dieta Mediterranea (Balanceada en grasas saludables)',
    'Desayuno: 1 rebanada de pan integral (30g) con 50g de aguacate y 1 huevo duro (50g). Merienda Manana: 30g de almendras. Almuerzo: 150g de pescado a la parrilla con 100g de ensalada de pepino, tomate y aceitunas (10g). Merienda Tarde: 1 yogurt griego (150g) con 50g de fresas. Cena: 200g de pechuga de pollo con 100g de espinacas y 50g de quinoa.').

/**
 * Dieta Alcalina (Baja en proteinas animales, moderada en grasas y carbohidratos)
 * Descripcion: Dieta enfocada en el equilibrio del pH del cuerpo, favoreciendo alimentos alcalinos como frutas, verduras y legumbres.
 */
dieta('Dieta Alcalina (Baja en proteinas animales, moderada en grasas y carbohidratos)',
    'Desayuno: Batido verde con espinacas (50g), pepino (50g), 1 manzana verde (150g) y agua de coco (200g). Merienda Manana: 1 punado de nueces mixtas (30g). Almuerzo: 150g de quinoa cocida con aguacate (100g), tomate (100g), y pepino (50g). Merienda Tarde: 1 batata cocida (100g). Cena: 200g de ensalada verde con lechuga, espinacas, pepino y zanahoria, con 50g de aguacate y 10g de aceite de oliva.').

/**
 * Dieta Baja en Calorias (1000-1200 kcal, para perdida de peso rapida)
 * Descripcion: Esta dieta se centra en una rapida reduccion de peso, limitada a 1000-1200 kcal por dia.
 */
dieta('Dieta Baja en Calorias (1000-1200 kcal, para perdida de peso rapida)',
    'Desayuno: 1 clara de huevo (50g) con espinacas (50g) y 1 rebanada de pan integral (30g). Merienda Manana: 1 manzana pequena (100g). Almuerzo: 100g de pechuga de pollo a la plancha, 50g de brocoli y 50g de batata. Merienda Tarde: 1 yogurt bajo en grasa (120g). Cena: 100g de pescado blanco a la plancha con 100g de esparragos al vapor.').


