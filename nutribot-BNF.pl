/** NutriBot BNF

Este modulo define un sistema de conversacion simple en Prolog para
NutriTec. Permite reconocer palabras clave, clasificar oraciones y
validar su correccion gramatical de acuerdo a una gramatica de frases
nominales y verbales.

El sistema incluye palabras clave de inicio y fin de conversacion, asi
como respuestas positivas y negativas, y utiliza reglas para verificar
si las oraciones son gramaticalmente correctas.

@author Jose Eduardo Campos Salazar
@version 1.0
*/


:-style_check(-singleton).

/** start/1
 *
 * Palabras clave para iniciar la conversacion con el sistema.
 *
 * @param Lista Una lista de palabras clave que inician la conversacion.
 *
 * ==
 * ?- start([hola]).
 * true.
 * ==
 */
start([hola]).
start([iniciar]).
start([buenas]).
start([buenos]).
start([nutritec]).

/** final/1
 *
 * Palabras clave para finalizar la conversacion con el sistema.
 *
 * @param Lista Una lista de palabras clave que finalizan la
 * conversacion.
 *
 * ==
 * ?- final([gracias]).
 * true.
 * ==
 */
final([gracias]).
final([muchas,gracias]).
final([chao]).
final([adios]).

/** negative/2
 *
 * Define respuestas negativas en el sistema.
 *
 * @param Palabras Una lista de palabras que representa una respuesta negativa.
 * @param Restante La lista restante despues de procesar la respuesta
 * negativa.
 *
 * ==
 * ?- negative([no,hago,ejercicio],S).
 * S = [hago, ejercicio].
 * ==
 */
negative([no|S],S).
negative(['No'|S],S).
negative([nunca|S],S).
negative(['Nunca'|S],S).
negative([jamas|S],S).
negative(['Jamas'|S],S).
negative([nada|S],S).
negative(['Nada'|S],S).

/** positive/2
 *
 * Define respuestas positivas en el sistema.
 *
 * @param Palabras Una lista de palabras que representa una respuesta positiva.
 * @param Restante La lista restante despues de procesar la respuesta
 * positiva.
 *
 * ==
 * ?- positive([si,me,gustaria],S).
 * S = [me, gustaria].
 * ==
 */
positive([si|S],S).
positive([claro|S],S).

/** determinante/2
 *
 * Define determinantes en una oracion.
 *
 * @param Palabras Una lista de palabras que comienza con un determinante.
 * @param Restante La lista restante despues de procesar el
 * determinante.
 *
 * ==
 * ?- determinante([yo,hago,ejercicio],S).
 * S = [hago, ejercicio].
 * ==
 */
determinante([yo|S],S).
determinante(['Yo'|S],S).

/** sustantivo_g/2
 *
 * Define un sustantivo general en una oracion.
 *
 * @param Palabras Una lista de palabras con un sustantivo general.
 * @param Restante La lista restante despues de procesar el sustantivo.
 *
 * ==
 * ?- sustantivo_g([persona,es,muy,saludable],S).
 * S = [es, muy, saludable].
 * ==
 */
sustantivo_g([_|S],S).

/** verb/2
 *
 * Define los verbos conjugados que pueden aparecer en una oracion.
 *
 * @param Palabras Una lista de palabras que contiene un verbo valido.
 * @param Restante La lista restante despues de procesar el verbo.
 *
 * ==
 * ?- verb([deseo,llevar,una,dieta],S).
 * S = [llevar, una, dieta].
 * ==
 */
verb([deseo|S],S).
verb([tengo|S],S).
verb([gustaria|S],S).
verb([pensado|S],S).
verb([llevar|S],S).
verb([estoy|S],S).
verb([diagnosticado|S],S).
verb([habia|S],S).
verb([realizar|S],S).
verb([quiero|S],S).
verb([hago|S],S).
verb([realizo|S],S).
verb([me,gustaria|S],S).
verb([me,diagnosticaron|S],S).
verb([deseo,llevar|S],S).
verb(['Deseo'|S],S).
verb(['Tengo'|S],S).
verb(['Gustaria'|S],S).
verb(['Pensado'|S],S).
verb(['Llevar'|S],S).
verb(['Estoy'|S],S).
verb(['Diagnosticado'|S],S).
verb(['Habia'|S],S).
verb(['Realizar'|S],S).
verb(['Quiero'|S],S).
verb(['Hago'|S],S).
verb(['Realizo'|S],S).
verb(['Me','gustan'|S],S).
verb(['Me','gustaria'|S],S).
verb(['Me','diagnosticaron'|S],S).
verb(['Deseo','llevar'|S],S).

/** oracion/2
 *
 * Verifica si una lista de palabras es una oracion valida de acuerdo
 * con las reglas gramaticales.
 *
 * @param Palabras Una lista de palabras que representa una oración.
 * @param Restante Una lista vacia al finalizar, lo que indica que la
 * oracion es valida.
 *
 * ==
 * ?- oracion([yo,deseo,llevar,una,dieta],[]).
 * true.
 * ==
 */
oracion(A,B):- sintagma_nominal(A,C).

/** sintagma_nominal/2
 *
 * Elimina el primer sintagma nominal encontrado en la oracion.
 *
 * @param Palabras Una lista de palabras que contiene un sintagma nominal.
 * @param Restante La lista restante despues de procesar el sintagma
 * nominal.
 *
 * ==
 * ?- sintagma_nominal([yo,quiero,una,dieta],S).
 * S = [dieta].
 * ==
 */
sintagma_nominal(A,B):- determinante(A,C), sintagma_verbal(C,Z), sustantivo_g(Z,B).
sintagma_nominal(A,B):- sintagma_verbal(A,C), sustantivo_g(C,B).
sintagma_nominal(A,B):- sintagma_verbal(A,B).

/** sintagma_verbal/2
 *
 * Elimina el primer sintagma verbal encontrado en la oracion.
 *
 * @param Palabras Una lista de palabras que contiene un sintagma verbal.
 * @param Restante La lista restante despues de procesar el sintagma
 * verbal.
 *
 * ==
 * ?- sintagma_verbal([deseo,una,dieta],S).
 * S = [una,dieta].
 * ==
 */
sintagma_verbal(A,B):- verb(A,B).

/** validacion_gramatical/1
 *
 * Valida si la oracion digitada por el usuario es gramaticalmente
 * correcta.
 *
 * @param Oracion Una lista de palabras que representa la oracion a
 * validar.
 *
 * ==
 * ?- validacion_gramatical([yo,quiero,llevar,una,dieta]).
 * true.
 * ==
 */
validacion_gramatical(Oracion):- oracion(Oracion,[]), !.
validacion_gramatical(Oracion):- nl, writeln('Oracion gramaticalmente incorrecta'), writeln('Escriba de nuevo su oracion'), nl, validacion_gramatical(_).
