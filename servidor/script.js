let messages = [];
let isListening = false;
let isTTSActive = false;

// Función para agregar un mensaje a la interfaz
function addMessage(text, sender) {
  const chatBox = document.getElementById('chat-box');
  const messageDiv = document.createElement('div');
  messageDiv.className = `message ${sender}`;
  messageDiv.textContent = text;
  chatBox.appendChild(messageDiv);
  chatBox.scrollTop = chatBox.scrollHeight; // Scroll automático al final
}

// Función para enviar la consulta al servidor
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

// Función para activar el reconocimiento de voz
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

// Función para activar/desactivar Text-to-Speech
function toggleTTS() {
  isTTSActive = document.getElementById('tts-toggle').checked;
}

// Función para hablar el texto usando Text-to-Speech
function speakText(text) {
  const synth = window.speechSynthesis;
  if (synth) {
    const utterance = new SpeechSynthesisUtterance(text);
    utterance.lang = 'es-CR';  // Configurar el idioma de la voz
    synth.speak(utterance);
  }
}
