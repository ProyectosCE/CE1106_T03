/**
 * Lista de mensajes en la interfaz.
 * @type {Array<Object>}
 */
let messages = [];

/**
 * Estado que indica si el reconocimiento de voz está activo.
 * @type {boolean}
 */
let isListening = false;

/**
 * Estado que indica si el Text-to-Speech está activado.
 * @type {boolean}
 */
let isTTSActive = false;

/**
 * Agrega un mensaje a la interfaz del chat.
 * @param {string} text - El texto del mensaje.
 * @param {string} sender - El remitente del mensaje, puede ser 'user' o 'bot'.
 */
function addMessage(text, sender) {
  const chatBox = document.getElementById('chat-box');
  const messageDiv = document.createElement('div');
  messageDiv.className = `message ${sender}`;
  messageDiv.textContent = text;
  chatBox.appendChild(messageDiv);
  chatBox.scrollTop = chatBox.scrollHeight; // Scroll automático al final
}

/**
 * Envía una consulta al servidor y maneja la respuesta.
 * Muestra el mensaje del usuario en la interfaz y el mensaje del bot tras la respuesta del servidor.
 * Si está activado, utiliza Text-to-Speech para leer la respuesta en voz alta.
 * @returns {Promise<void>}
 */
async function sendQuery() {
  const queryInput = document.getElementById('input-query');
  const queryText = queryInput.value.trim();
  if (queryText === '') return;

  // Mostrar el mensaje del usuario
  addMessage(queryText, 'user');

  // Limpiar el campo de entrada
  queryInput.value = '';

  try {
    const response = await fetch('http://localhost:8080/chat', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ query: queryText })
    });

    if (!response.ok) {
      throw new Error(`Error en la respuesta: ${response.status}`);
    }

    const data = await response.json();
    const botMessage = data.response;

    // Mostrar la respuesta del bot
    addMessage(botMessage, 'bot');

    // Si está activado el Text-to-Speech, leer en voz alta
    if (isTTSActive) {
      speakText(botMessage);
    }

  } catch (error) {
    console.error("Error al hacer la consulta:", error);
    const errorMessage = 'Error al conectar con el servidor.';
    addMessage(errorMessage, 'bot');
    if (isTTSActive) {
      speakText(errorMessage);
    }
  }
}

/**
 * Activa el reconocimiento de voz del navegador.
 * Si el navegador no soporta reconocimiento de voz, muestra una alerta.
 * Envía la transcripción del reconocimiento como consulta al servidor.
 */
function handleSpeechRecognition() {
  const SpeechRecognition = window.SpeechRecognition || window.webkitSpeechRecognition;
  if (!SpeechRecognition) {
    alert("Tu navegador no soporta reconocimiento de voz.");
    return;
  }

  const recognition = new SpeechRecognition();
  recognition.lang = 'es-ES';  // Idioma del reconocimiento

  recognition.onstart = () => {
    isListening = true;
    document.getElementById('speech-btn').textContent = '🎤 Escuchando...';
  };

  recognition.onend = () => {
    isListening = false;
    document.getElementById('speech-btn').textContent = '🎤';
  };

  recognition.onerror = (event) => {
    console.error('Error en el reconocimiento de voz:', event.error);
    document.getElementById('error-message').textContent = 'Hubo un error con el reconocimiento de voz.';
  };

  recognition.onresult = (event) => {
    const transcript = event.results[0][0].transcript;
    sendQuery(transcript);  // Enviar la transcripción como mensaje
  };

  recognition.start();  // Comenzar a escuchar
}

/**
 * Activa o desactiva la funcionalidad de Text-to-Speech (TTS) basado en el estado del checkbox.
 */
function toggleTTS() {
  isTTSActive = document.getElementById('tts-toggle').checked;
}

/**
 * Convierte un texto a voz usando la API de Text-to-Speech del navegador.
 * @param {string} text - El texto a ser leído en voz alta.
 */
function speakText(text) {
  const synth = window.speechSynthesis;
  if (synth) {
    const utterance = new SpeechSynthesisUtterance(text);
    utterance.lang = 'es-CR';  // Configurar el idioma de la voz
    synth.speak(utterance);
  }
}
