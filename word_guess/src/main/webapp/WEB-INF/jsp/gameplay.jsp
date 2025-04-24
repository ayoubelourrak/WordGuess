<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>WordGuess - Game #${game.id}</title>
  <style>
    body {
      font-family: Arial, sans-serif;
      background-color: #f5f5f5;
      margin: 0;
      padding: 0;
    }

    .header {
      background-color: #4CAF50;
      color: white;
      padding: 15px 20px;
      display: flex;
      justify-content: space-between;
      align-items: center;
    }

    .header h1 {
      margin: 0;
      font-size: 24px;
    }

    .header-right {
      display: flex;
      align-items: center;
    }

    .header-right a {
      color: white;
      text-decoration: none;
      margin-left: 20px;
    }

    .header-right a:hover {
      text-decoration: underline;
    }

    .container {
      max-width: 900px;
      margin: 20px auto;
      padding: 0 20px;
    }

    .game-info {
      background-color: white;
      border-radius: 8px;
      box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);
      padding: 20px;
      margin-bottom: 20px;
    }

    .game-info h2 {
      margin-top: 0;
      color: #333;
    }

    .word-display {
      text-align: center;
      margin: 30px 0;
    }

    .word-display .letter {
      display: inline-block;
      width: 40px;
      height: 40px;
      line-height: 40px;
      text-align: center;
      font-size: 24px;
      font-weight: bold;
      margin: 0 5px;
      border-bottom: 2px solid #333;
      color: #333;
    }

    .game-status {
      font-size: 18px;
      font-weight: bold;
      text-align: center;
      margin-bottom: 20px;
      padding: 10px;
      border-radius: 4px;
    }

    .waiting {
      background-color: #fff3cd;
      color: #856404;
    }

    .in_progress {
      background-color: #d1ecf1;
      color: #0c5460;
    }

    .completed {
      background-color: #d4edda;
      color: #155724;
    }

    .players-list {
      display: flex;
      flex-wrap: wrap;
      gap: 10px;
      margin-bottom: 20px;
    }

    .player {
      background-color: #f8f9fa;
      border-radius: 4px;
      padding: 8px 15px;
    }

    .player.current {
      background-color: #4CAF50;
      color: white;
    }

    .player.winner {
      background-color: #ffc107;
      color: #333;
    }

    .guessing-form {
      background-color: white;
      border-radius: 8px;
      box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);
      padding: 20px;
      margin-bottom: 20px;
    }

    .form-row {
      display: flex;
      margin-bottom: 15px;
    }

    .form-group {
      flex: 1;
      margin-right: 10px;
    }

    .form-group:last-child {
      margin-right: 0;
    }

    label {
      display: block;
      margin-bottom: 5px;
      font-weight: bold;
      color: #555;
    }

    input[type="text"] {
      width: 100%;
      padding: 10px;
      border: 1px solid #ddd;
      border-radius: 4px;
      font-size: 16px;
      box-sizing: border-box;
    }

    .button {
      background-color: #4CAF50;
      color: white;
      border: none;
      padding: 10px 15px;
      text-align: center;
      text-decoration: none;
      display: inline-block;
      font-size: 16px;
      border-radius: 4px;
      cursor: pointer;
    }

    .button:hover {
      background-color: #45a049;
    }

    .button.disabled {
      background-color: #cccccc;
      cursor: not-allowed;
    }

    .history {
      background-color: white;
      border-radius: 8px;
      box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);
      padding: 20px;
    }

    .history h3 {
      margin-top: 0;
      color: #333;
    }

    .history-section {
      margin-bottom: 20px;
    }

    .history-list {
      display: flex;
      flex-wrap: wrap;
      gap: 5px;
    }

    .history-item {
      background-color: #f8f9fa;
      border-radius: 4px;
      padding: 5px 10px;
    }

    .correct {
      background-color: #d4edda;
      color: #155724;
    }

    .incorrect {
      background-color: #f8d7da;
      color: #721c24;
    }

    .alert {
      padding: 15px;
      margin-bottom: 20px;
      border-radius: 4px;
      font-weight: bold;
    }

    .alert-danger {
      background-color: #f8d7da;
      color: #721c24;
      border: 1px solid #f5c6cb;
    }

    .alert-success {
      background-color: #d4edda;
      color: #155724;
      border: 1px solid #c3e6cb;
    }

    .game-code {
      font-family: monospace;
      font-size: 18px;
      padding: 5px 10px;
      background-color: #f8f9fa;
      border-radius: 4px;
      margin-left: 10px;
    }

    .timer {
      text-align: center;
      font-size: 24px;
      font-weight: bold;
      margin: 15px 0;
      color: #333;
    }
    
    .timer.urgent {
      color: #dc3545;
    }
    
    .action-buttons {
      display: flex;
      justify-content: space-between;
      margin-top: 20px;
    }
    
    .button-secondary {
      background-color: #6c757d;
      color: white;
      border: none;
      padding: 10px 15px;
      text-align: center;
      text-decoration: none;
      display: inline-block;
      font-size: 16px;
      border-radius: 4px;
      cursor: pointer;
    }
    
    .button-secondary:hover {
      background-color: #5a6268;
    }
    
    .button-danger {
      background-color: #dc3545;
      color: white;
      border: none;
      padding: 10px 15px;
      text-align: center;
      text-decoration: none;
      display: inline-block;
      font-size: 16px;
      border-radius: 4px;
      cursor: pointer;
    }
    
    .button-danger:hover {
      background-color: #c82333;
    }
  </style>
</head>
<body>
<div class="header">
  <h1>WordGuess</h1>
  <div class="header-right">
    <a href="/word_guess/games">Game List</a>
    <a href="/word_guess/logout">Logout</a>
  </div>
</div>

<div class="container">
  <div class="game-info">
    <h2>
      Game #${game.id}
    </h2>

    <div class="game-status ${game.status}">
      <c:choose>
        <c:when test="${game.status == 'waiting'}">
          Waiting for players (${game.players.size()}/${game.maxPlayers})
        </c:when>
        <c:when test="${game.status == 'in_progress'}">
          <c:choose>
            <c:when test="${game.currentPlayer == username}">
              It's your turn to guess!
            </c:when>
            <c:otherwise>
              ${game.currentPlayer}'s turn to guess
            </c:otherwise>
          </c:choose>
        </c:when>
        <c:when test="${game.status == 'completed'}">
          Game completed!
        </c:when>
      </c:choose>
    </div>

    <div class="timer" id="turn-timer" style="display: none;">
      Time left: <span id="timer-seconds">30</span> seconds
    </div>

    <div class="word-display">
      <c:forEach var="letter" items="${game.maskedWord.toCharArray()}">
        <span class="letter">${letter}</span>
      </c:forEach>
    </div>

    <h3>Players</h3>
    <div class="players-list">
      <c:forEach var="player" items="${game.players}">
        <div class="player ${player == game.currentPlayer ? 'current' : ''} ${player == game.winner ? 'winner' : ''}">
            ${player} ${player == username ? '(You)' : ''}
          <c:if test="${player == game.currentPlayer}">
            <span>⭐</span>
          </c:if>
          <c:if test="${player == game.winner}">
            <span>🏆</span>
          </c:if>
        </div>
      </c:forEach>
    </div>

    <div class="action-buttons">
      <c:if test="${game.status == 'waiting' && game.creator == username}">
        <form id="start-game-form" action="/word_guess/games/${game.id}/start" method="post">
          <button type="submit" class="button" id="start-game-button">Start Game</button>
        </form>
      </c:if>
      
      <form action="/word_guess/games/${game.id}/leave" method="post">
        <button type="submit" class="button-danger" id="leave-game-button">Leave Game</button>
      </form>
    </div>
  </div>

  <div id="alert" class="alert" style="display: none;"></div>

  <!-- The guessing form will be dynamically created by JavaScript when needed -->

  <div class="history">
    <h3>Game History</h3>

    <div class="history-section">
      <h4>Guessed Letters</h4>
      <div class="history-list" id="letters-list">
        <c:forEach var="letter" items="${game.guessedLetters}">
          <div class="history-item ${game.maskedWord.contains(letter) ? 'correct' : 'incorrect'}">
              ${letter}
          </div>
        </c:forEach>
      </div>
    </div>

    <div class="history-section">
      <h4>Guessed Words</h4>
      <div class="history-list" id="words-list">
        <c:forEach var="word" items="${game.guessedWords}">
          <div class="history-item ${game.word != null && word == game.word ? 'correct' : 'incorrect'}">
              ${word}
          </div>
        </c:forEach>
      </div>
    </div>
  </div>
</div>
<script>
  // Game data
  const gameId = '${game.id}';
  const username = '${username}';
  const wsUrl = '${apiWebsocketBaseUrl}' + '/ws/gameplay/' + gameId;

  // Current game state
  let currentGame = {
    status: '${game.status}',
    currentPlayer: '${game.currentPlayer}',
    maskedWord: '${game.maskedWord}',
    word: '${game.word}',
    guessedLetters: [],
    guessedWords: []
  };

  console.log(currentGame);

  // WebSocket connection
  let socket;

  function connectWebSocket() {
    const wsUrlWithParams = wsUrl + '?username=' + encodeURIComponent(username);
    socket = new WebSocket(wsUrlWithParams);

    socket.onopen = function(e) {
      console.log('WebSocket connection established');
      // Initial ping to keep the connection alive
      pingInterval = setInterval(() => {
        if (socket.readyState === WebSocket.OPEN) {
          socket.send(JSON.stringify({ type: 'ping' }));
        }
      }, 29000); // Ping every 29 seconds
    };

    socket.onmessage = function(event) {
      try {
        const message = JSON.parse(event.data);
        console.log('Received message:', message);

        if (message.type === 'game_update') {
          updateGameState(message.data);
        } else if (message.type === 'error') {
          showAlert(message.message, 'danger');
        }
      } catch (error) {
        console.error('Error processing message:', error);
      }
    };

    socket.onclose = function(event) {
      console.log('WebSocket connection closed');
      // Try to reconnect after a delay
      clearInterval(pingInterval);
      setTimeout(connectWebSocket, 2000);
    };

    socket.onerror = function(error) {
      console.error('WebSocket error:', error);
      socket.close();
    };
  }

  // Update game state with WebSocket data
  function updateGameState(game) {
    currentGame = game;

    // Update game status
    updateGameStatus(game);

    // Update masked word
    if (game.status !== 'completed' || !game.word) {
      console.log('Game in progress');
      console.log(game);
      updateWordDisplay(game.maskedWord);
    } else if (game.status === 'completed' && game.word) {
      // Show the actual word if completed
      console.log('Game completed');
      updateWordDisplay(game.word);
    }

    // Update form visibility based on game status
    if (game.status === 'in_progress') {
      createGuessingFormIfNeeded();
      if (game.currentPlayer === username) {
        enableGuessingForm();
        startTurnTimer();
        document.getElementById('turn-timer').style.display = 'block';
      } else {
        disableGuessingForm();
        clearTurnTimer();
      }
    } else {
      removeGuessingForm();
      clearTurnTimer();
    }

    // Update players list
    updatePlayersList(game.players, game.currentPlayer, game.winner);

    // Update guessed letters
    updateGuessedLetters(game.guessedLetters, game.maskedWord);

    // Update guessed words
    updateGuessedWords(game.guessedWords, game.word);

    // Update start game button visibility
    const startGameButton = document.getElementById('start-game-button');
    if (startGameButton) {
      const startButtonParent = startGameButton.closest('form');
      if (game.status !== 'waiting' || game.creator !== username) {
        if (startButtonParent) startButtonParent.style.display = 'none';
      } else {
        if (startButtonParent) startButtonParent.style.display = 'block';
      }
    }

    // Show error if any
    if (game.error) {
      showAlert(game.error, 'danger');
    }
  }

  // Update game status message
  function updateGameStatus(game) {
    const statusElement = document.querySelector('.game-status');

    // Update CSS class
    statusElement.className = 'game-status ' + game.status;

    // Update status message based on game state
    if (game.status === 'waiting') {
      const playerCount = game.players ? game.players.length : 0;
      statusElement.textContent = 'Waiting for players (' + playerCount + '/' + game.maxPlayers + ')';
    } else if (game.status === 'in_progress') {
      if (game.currentPlayer === username) {
        statusElement.textContent = "It's your turn to guess!";
      } else {
        statusElement.textContent = game.currentPlayer + "'s turn to guess";
      }
    } else if (game.status === 'completed') {
      statusElement.textContent = `Game completed!`;
    }
  }

  // Update the word display
  function updateWordDisplay(word) {
    console.log('The word is: ' + word);
    const wordDisplay = document.querySelector('.word-display');
    let html = '';

    for (let i = 0; i < word.length; i++) {
      html += '<span class="letter">' + word[i] + '</span>';
    }

    console.log(html);

    wordDisplay.innerHTML = html;
  }

  // Update players list
  function updatePlayersList(players, currentPlayer, winner) {
    const playersContainer = document.querySelector('.players-list');
    let html = '';

    for (let i = 0; i < players.length; i++) {
      const player = players[i];
      const isCurrentPlayer = player === currentPlayer;
      const isWinner = player === winner;
      const isYou = player === username;

      html += '<div class="player ' + (isCurrentPlayer ? 'current ' : ' ') +
              (isWinner ? 'winner' : '') + '">' +
              player + ' ' + (isYou ? '(You) ' : ' ') +
              (isCurrentPlayer ? '<span>⭐</span>' : '') +
              (isWinner ? '<span>🏆</span>' : '') +
              '</div>';
    }

    playersContainer.innerHTML = html;
  }

  // Update guessed letters
  function updateGuessedLetters(letters, word) {
    const lettersContainer = document.getElementById('letters-list');
    let html = '';

    for (let i = 0; i < letters.length; i++) {
      const letter = letters[i];
      const isCorrect = word && word.includes(letter);
      html += '<div class="history-item ' + (isCorrect ? 'correct' : 'incorrect') + '">' +
              letter +
              '</div>';
    }

    lettersContainer.innerHTML = html;
  }

  // Update guessed words
  function updateGuessedWords(words, correctWord) {
    const wordsContainer = document.getElementById('words-list');
    let html = '';

    for (let i = 0; i < words.length; i++) {
      const word = words[i];
      const isCorrect = correctWord && word === correctWord;
      html += '<div class="history-item ' + (isCorrect ? 'correct' : 'incorrect') + '">' +
              word +
              '</div>';
    }

    wordsContainer.innerHTML = html;
  }

  // Enable guessing form
  function enableGuessingForm() {
    const letterButton = document.getElementById('letter-button');
    const wordButton = document.getElementById('word-button');

    if (letterButton) letterButton.disabled = false;
    if (wordButton) wordButton.disabled = false;

    if (letterButton) letterButton.classList.remove('disabled');
    if (wordButton) wordButton.classList.remove('disabled');
  }

  // Disable guessing form
  function disableGuessingForm() {
    const letterButton = document.getElementById('letter-button');
    const wordButton = document.getElementById('word-button');

    if (letterButton) letterButton.disabled = true;
    if (wordButton) wordButton.disabled = true;

    if (letterButton) letterButton.classList.add('disabled');
    if (wordButton) wordButton.classList.add('disabled');
  }

  // Create guessing form if it doesn't exist
  function createGuessingFormIfNeeded() {
    // Check if the form already exists
    if (document.querySelector('.guessing-form')) {
      return;
    }

    // Create the form
    const formHTML = `
        <div class="guessing-form">
          <h3>Make a Guess</h3>

          <div class="form-row">
            <div class="form-group">
              <label for="letter-guess">Guess a Letter</label>
              <div style="display: flex;">
                <input type="text" id="letter-guess" maxlength="1" pattern="[A-Za-z]">
                <button type="button" id="letter-button" class="button" onclick="guessLetter()">
                  Guess Letter
                </button>
              </div>
            </div>
          </div>

          <div class="form-row">
            <div class="form-group">
              <label for="word-guess">Guess the Word</label>
              <div style="display: flex;">
                <input type="text" id="word-guess" pattern="[A-Za-z]+">
                <button type="button" id="word-button" class="button" onclick="guessWord()">
                  Guess Word
                </button>
              </div>
            </div>
          </div>
        </div>
      `;

    // Insert after alert
    const alertDiv = document.getElementById('alert');
    alertDiv.insertAdjacentHTML('afterend', formHTML);

    // Initialize buttons
    if (currentGame.currentPlayer !== username) {
      disableGuessingForm();
    }
  }

  // Remove guessing form
  function removeGuessingForm() {
    const form = document.querySelector('.guessing-form');
    if (form) {
      form.remove();
    }
  }

  // Show alert message
  function showAlert(message, type) {
    const alertElement = document.getElementById('alert');
    alertElement.className = 'alert alert-' + type;
    alertElement.textContent = message;
    alertElement.style.display = 'block';

    // Hide after 5 seconds
    setTimeout(() => {
      alertElement.style.display = 'none';
    }, 5000);
  }

  let timerInterval;
  let secondsLeft = 30;

  // Update the timer display
  function updateTimer() {
    const timerElement = document.getElementById('turn-timer');
    const secondsElement = document.getElementById('timer-seconds');
    
    if (currentGame.status === 'in_progress' && currentGame.currentPlayer === username) {
      // Show timer when it's the player's turn
      timerElement.style.display = 'block';
      secondsElement.textContent = secondsLeft;
      
      // Add urgent class when less than 10 seconds left
      if (secondsLeft <= 10) {
        timerElement.classList.add('urgent');
      } else {
        timerElement.classList.remove('urgent');
      }
    } else {
      // Hide timer when it's not the player's turn
      timerElement.style.display = 'none';
    }
  }

  // Start the turn timer
  function startTurnTimer() {
    // Clear any existing timer
    clearInterval(timerInterval);
    
    // Reset seconds
    secondsLeft = 30;

    const timerElement = document.getElementById('turn-timer');
    timerElement.style.display = 'block';
    
    // Update timer display
    updateTimer();
    
    // Start the interval
    timerInterval = setInterval(() => {
      secondsLeft--;
      updateTimer();
      
      if (secondsLeft <= 0) {
        // Time's up, clear the interval
        clearInterval(timerInterval);
      }
    }, 1000);
  }

  // Clear the turn timer
  function clearTurnTimer() {
    clearInterval(timerInterval);
    document.getElementById('turn-timer').style.display = 'none';
  }

  function guessLetter() {
    const letterInput = document.getElementById('letter-guess');
    const letter = letterInput.value.trim().toLowerCase();

    if (!letter.match(/^[a-z]$/i)) {
        showAlert('Please enter a single letter', 'danger');
        return;
    }

    sendGuess(letter);
    letterInput.value = '';
  }

  function guessWord() {
      const wordInput = document.getElementById('word-guess');
      const word = wordInput.value.trim().toLowerCase();

      if (!word.match(/^[a-z]+$/i) || word.length < 2) {
          showAlert('Please enter a valid word (at least 2 letters)', 'danger');
          return;
      }

      sendGuess(word);
      wordInput.value = '';
  }

  // Send guess to server
  function sendGuess(guess) {
    if (socket && socket.readyState === WebSocket.OPEN) {
      const message = {
        type: 'guess',
        guess: guess
      };

      socket.send(JSON.stringify(message));
    } else {
      showAlert('Connection error. Please try again.', 'danger');
    }
  }

  // Connect to WebSocket when page loads
  window.addEventListener('load', connectWebSocket);

  // Close Websocket when leaving
  window.addEventListener('beforeunload', function() {
    if (socket && socket.readyState === WebSocket.OPEN) {
      socket.close();
    }
  });

  document.addEventListener('DOMContentLoaded', function() {
    const startGameForm = document.getElementById('start-game-form');
    if (startGameForm) {
      startGameForm.addEventListener('submit', function(e) {
        e.preventDefault();

        // Get the game ID from the form action URL
        const actionUrl = startGameForm.getAttribute('action');

        // Send AJAX request
        fetch(actionUrl, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json'
          }
        })
                .then(response => response.json())
                .then(data => {
                  if (!data.success && data.error) {
                    // Show error alert
                    showAlert(data.error, 'danger');
                  }
                })
                .catch(error => {
                  console.error('Error:', error);
                  showAlert('Failed to start game. Please try again.', 'danger');
                });
      });
    }
  });
</script>
</body>
</html>