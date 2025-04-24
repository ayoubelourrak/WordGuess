<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>WordGuess - Games</title>
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

    .header-right span {
      margin-right: 20px;
    }

    .header-right a {
      color: white;
      text-decoration: none;
    }

    .header-right a:hover {
      text-decoration: underline;
    }

    .container {
      max-width: 900px;
      margin: 20px auto;
      padding: 0 20px;
    }

    .alert {
      padding: 15px;
      margin-bottom: 20px;
      border-radius: 4px;
    }

    .alert-danger {
      background-color: #f2dede;
      color: #a94442;
      border: 1px solid #ebccd1;
    }

    .alert-success {
      background-color: #d4edda;
      color: #155724;
      border: 1px solid #c3e6cb;
    }

    .tabs {
      display: flex;
      border-bottom: 1px solid #ddd;
      margin-bottom: 20px;
    }

    .tab {
      padding: 10px 20px;
      cursor: pointer;
      background-color: #f9f9f9;
      border: 1px solid #ddd;
      border-bottom: none;
      border-radius: 4px 4px 0 0;
      margin-right: 5px;
    }

    .tab.active {
      background-color: white;
      border-bottom: 1px solid white;
      margin-bottom: -1px;
    }

    .tab-content {
      display: none;
    }

    .tab-content.active {
      display: block;
    }

    .game-list {
      display: grid;
      grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
      gap: 20px;
      margin-top: 20px;
    }

    .game-card {
      background-color: white;
      border-radius: 8px;
      box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);
      padding: 15px;
      transition: transform 0.2s;
    }

    .game-card:hover {
      transform: translateY(-5px);
    }

    .game-card h3 {
      margin-top: 0;
      color: #333;
    }

    .game-card p {
      color: #666;
      margin: 10px 0;
    }

    .game-card .button {
      background-color: #4CAF50;
      color: white;
      border: none;
      padding: 8px 15px;
      text-align: center;
      text-decoration: none;
      display: inline-block;
      font-size: 14px;
      border-radius: 4px;
      cursor: pointer;
      width: 100%;
      box-sizing: border-box;
      margin-top: 10px;
    }

    .game-card .button:hover {
      background-color: #45a049;
    }

    .form-group {
      margin-bottom: 15px;
    }

    label {
      display: block;
      margin-bottom: 5px;
      font-weight: bold;
      color: #555;
    }

    input[type="text"],
    input[type="number"],
    select {
      width: 100%;
      padding: 10px;
      border: 1px solid #ddd;
      border-radius: 4px;
      font-size: 16px;
      box-sizing: border-box;
    }

    .button-primary {
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

    .button-primary:hover {
      background-color: #45a049;
    }

    .empty-state {
      text-align: center;
      padding: 40px 20px;
      color: #666;
    }

    .empty-state p {
      margin-bottom: 20px;
    }
  </style>
</head>
<body>
<div class="header">
  <h1>WordGuess</h1>
  <div class="header-right">
    <span>Welcome, ${sessionScope.username}</span>
    <a href="/word_guess/logout">Logout</a>
  </div>
</div>

<div class="container">
  <div id="alert" class="alert" style="display: none;"></div>

  <c:if test="${not empty error}">
    <div class="alert alert-danger">${error}</div>
  </c:if>

  <c:if test="${not empty success}">
    <div class="alert alert-success">${success}</div>
  </c:if>

  <div class="tabs">
    <div class="tab active" onclick="showTab('public-games')">Public Games</div>
    <div class="tab" onclick="showTab('join-private')">Join Private Game</div>
    <div class="tab" onclick="showTab('create-game')">Create Game</div>
  </div>

  <div id="public-games" class="tab-content active">
    <h2>Available Public Games</h2>

    <div class="game-list" id="games-container">
      <c:choose>
        <c:when test="${empty games}">
          <div class="empty-state">
            <p>No public games available.</p>
            <button class="button-primary" onclick="showTab('create-game')">Create a Game</button>
          </div>
        </c:when>
        <c:otherwise>
          <c:forEach items="${games}" var="game">
            <div class="game-card">
              <h3>Game #${game.id}</h3>
              <p>Creator: ${game.creator}</p>
              <p>Players: ${game.players.size()}/${game.maxPlayers}</p>
              <p>Status: ${game.status}</p>
              <a href="/word_guess/games/${game.id}/join" class="button">Join Game</a>
            </div>
          </c:forEach>
        </c:otherwise>
      </c:choose>
    </div>
  </div>

  <div id="join-private" class="tab-content">
    <h2>Join Private Game</h2>

    <form action="/word_guess/games/join" method="post" onsubmit="return validateJoinForm()">
      <div class="form-group">
        <label for="gameCode">Game Code</label>
        <input type="text" id="gameCode" name="gameCode" required>
      </div>

      <button type="submit" class="button-primary">Join Game</button>
    </form>
  </div>

  <div id="create-game" class="tab-content">
    <h2>Create New Game</h2>

    <form:form action="/word_guess/games/create" method="post" modelAttribute="newGame" onsubmit="return validateCreateForm()">
      <div class="form-group">
        <label for="maxPlayers">Max Players</label>
        <form:input path="maxPlayers" type="number" id="maxPlayers" min="2" max="50" value="2" />
      </div>

      <div class="form-group">
        <label for="isPublic">Game Visibility</label>
        <form:select path="public" id="isPublic">
          <form:option value="true">Public</form:option>
          <form:option value="false">Private</form:option>
        </form:select>
      </div>

      <button type="submit" class="button-primary">Create Game</button>
    </form:form>
  </div>
</div>

<script>
  // Game state
  let currentGames = [];

  // Function to show the selected tab
  function showTab(tabId) {
    // Hide all tab contents
    document.querySelectorAll('.tab-content').forEach(tab => {
      tab.classList.remove('active');
    });

    // Deactivate all tabs
    document.querySelectorAll('.tab').forEach(tab => {
      tab.classList.remove('active');
    });

    // Show selected tab content
    document.getElementById(tabId).classList.add('active');

    // Activate the clicked tab
    Array.from(document.querySelectorAll('.tab')).find(tab =>
            tab.getAttribute('onclick').includes(tabId)
    ).classList.add('active');
  }

  // Form validation
  function validateJoinForm() {
    const gameCode = document.getElementById('gameCode').value.trim();
    if (!gameCode) {
      showAlert('Please enter a game code', 'danger');
      return false;
    }
    return true;
  }

  function validateCreateForm() {
    const maxPlayers = document.getElementById('maxPlayers').value;
    if (maxPlayers < 2 || maxPlayers > 50) {
      showAlert('Max players must be between 2 and 50', 'danger');
      return false;
    }
    return true;
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

  // WebSocket connection for real-time game updates
  const username = '${sessionScope.username}';
  const wsUrl = '${apiWebsocketBaseUrl}' + '/ws/games';

  let socket;

  function connectWebSocket() {
    const wsUrlWithParams = wsUrl + '?username=' + encodeURIComponent(username);
    socket = new WebSocket(wsUrlWithParams);

    socket.onopen = function(e) {
      console.log('WebSocket connection established');
    };

    socket.onmessage = function(event) {
      try {
        const message = JSON.parse(event.data);
        console.log('Received message:', message);

        if (message.type === 'games_list_update') {
          updateGamesList(message.data);
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
      setTimeout(connectWebSocket, 2000);
    };

    socket.onerror = function(error) {
      console.error('WebSocket error:', error);
      socket.close();
    };
  }

  // Update games list with WebSocket data
  function updateGamesList(games) {
    // Store the current games
    currentGames = games;

    const gamesContainer = document.getElementById('games-container');

    if (!games || games.length === 0) {
      gamesContainer.innerHTML = `
                <div class="empty-state">
                    <p>No public games available.</p>
                    <button class="button-primary" onclick="showTab('create-game')">Create a Game</button>
                </div>
            `;
      return;
    }

    let html = '';

    for (let i = 0; i < games.length; i++) {
      const game = games[i];
      const playerCount = Array.isArray(game.players) ? game.players.length : (game.players ? game.players.size : 0);

      html += '<div class="game-card">' +
              '<h3>Game #' + game.id + '</h3>' +
              '<p>Creator: ' + game.creator + '</p>' +
              '<p>Players: ' + playerCount + '/' + game.maxPlayers + '</p>' +
              '<p>Status: ' + game.status + '</p>' +
              '<a href="/word_guess/games/' + game.id + '/join" class="button">Join Game</a>' +
              '</div>';
    }

    gamesContainer.innerHTML = html;
  }

  // Connect to WebSocket when page loads
  window.addEventListener('load', function() {
    // Initialize from server-rendered data first
    const initialGames = [];
    <c:forEach items="${games}" var="game">
    initialGames.push({
      id: '${game.id}',
      creator: '${game.creator}',
      players: ${game.players.size()},
      maxPlayers: ${game.maxPlayers},
      status: '${game.status}'
    });
    </c:forEach>

    if (initialGames.length > 0) {
      currentGames = initialGames;
    }

    connectWebSocket();
  });
</script>
</body>
</html>
