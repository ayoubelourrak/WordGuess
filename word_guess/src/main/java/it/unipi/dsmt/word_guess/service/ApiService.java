package it.unipi.dsmt.word_guess.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import it.unipi.dsmt.word_guess.model.Game;
import it.unipi.dsmt.word_guess.model.User;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.HashMap;

@Service
public class ApiService {

    private final HttpClient httpClient = HttpClient.newBuilder().version(HttpClient.Version.HTTP_1_1).build();
    private final ObjectMapper objectMapper = new ObjectMapper();

    @Value("${api.base-url}")
    private String apiBaseUrl;

    /**
     * Register a new user
     * @param user User to register
     * @return Registration result
     */
    public Map<String, Object> registerUser(User user) {
        try {
            Map<String, String> requestData = new HashMap<>();
            requestData.put("username", user.getUsername());
            requestData.put("password", user.getPassword());

            String requestBody = objectMapper.writeValueAsString(requestData);

            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(apiBaseUrl + "/api/users/register"))
                    .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                    .POST(HttpRequest.BodyPublishers.ofString(requestBody))
                    .build();

            HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
            return parseResponse(response.body());
        } catch (Exception e) {
            Map<String, Object> errorResponse = new HashMap<>();
            errorResponse.put("success", false);
            errorResponse.put("error", "Failed to connect to server: " + e.getMessage());
            return errorResponse;
        }
    }

    /**
     * Login a user
     * @param user User to login
     * @return Login result with authentication token
     */
    public Map<String, Object> loginUser(User user) {
        try {
            Map<String, String> requestData = new HashMap<>();
            requestData.put("username", user.getUsername());
            requestData.put("password", user.getPassword());

            String requestBody = objectMapper.writeValueAsString(requestData);

            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(apiBaseUrl + "/api/users/login"))
                    .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                    .POST(HttpRequest.BodyPublishers.ofString(requestBody))
                    .build();

            HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
            return parseResponse(response.body());
        } catch (Exception e) {
            Map<String, Object> errorResponse = new HashMap<>();
            errorResponse.put("success", false);
            errorResponse.put("error", "Failed to connect to server: " + e.getMessage());
            return errorResponse;
        }
    }

    /**
     * Get all public games
     * @param username Current username
     * @return List of public games
     */
    public List<Game> getPublicGames(String username) {
        try {
            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(apiBaseUrl + "/api/games"))
                    .header("X-Username", username)
                    .GET()
                    .build();

            HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
            Map<String, Object> responseMap = parseResponse(response.body());

            if ((boolean) responseMap.get("success")) {
                JsonNode gamesNode = objectMapper.readTree(response.body()).get("games");
                return objectMapper.readValue(gamesNode.toString(), new TypeReference<List<Game>>() {});
            }
            return Collections.emptyList();
        } catch (Exception e) {
            return Collections.emptyList();
        }
    }

    /**
     * Get game by ID
     * @param gameId Game ID
     * @param username Current username
     * @return Game or null if not found
     */
    public Game getGame(String gameId, String username) {
        try {
            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(apiBaseUrl + "/api/games/" + gameId))
                    .header("X-Username", username)
                    .GET()
                    .build();

            HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
            Map<String, Object> responseMap = parseResponse(response.body());

            if ((boolean) responseMap.get("success")) {
                JsonNode gameNode = objectMapper.readTree(response.body()).get("game");
                return objectMapper.readValue(gameNode.toString(), Game.class);
            }
            return null;
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Create a new game
     * @param game Game to create
     * @param username Current username
     * @return Created game or null if creation failed
     */
    public Game createGame(Game game, String username) {
        try {
            Map<String, Object> requestData = new HashMap<>();
            requestData.put("maxPlayers", game.getMaxPlayers());
            requestData.put("isPublic", game.isPublic());

            String requestBody = objectMapper.writeValueAsString(requestData);

            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(apiBaseUrl + "/api/games/create"))
                    .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                    .header("X-Username", username)
                    .POST(HttpRequest.BodyPublishers.ofString(requestBody))
                    .build();

            HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
            Map<String, Object> responseMap = parseResponse(response.body());

            if ((boolean) responseMap.get("success")) {
                JsonNode gameNode = objectMapper.readTree(response.body()).get("game");
                return objectMapper.readValue(gameNode.toString(), Game.class);
            }
            return null;
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Join a game
     * @param gameId Game ID to join
     * @param username Current username
     * @return Joined game or null if join failed
     */
    public Game joinGame(String gameId, String username) {
        try {
            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(apiBaseUrl + "/api/games/" + gameId + "/join"))
                    .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                    .header("X-Username", username)
                    .POST(HttpRequest.BodyPublishers.ofString("{}"))
                    .build();

            HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
            Map<String, Object> responseMap = parseResponse(response.body());

            if ((boolean) responseMap.get("success")) {
                JsonNode gameNode = objectMapper.readTree(response.body()).get("game");
                return objectMapper.readValue(gameNode.toString(), Game.class);
            }
            return null;
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Start a game (creator only) and return full response
     * @param gameId Game ID to start
     * @param username Current username
     * @return Response map including success status and any error messages
     */
    public Map<String, Object> startGameWithResponse(String gameId, String username) {
        try {
            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(apiBaseUrl + "/api/games/" + gameId + "/start"))
                    .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                    .header("X-Username", username)
                    .POST(HttpRequest.BodyPublishers.ofString("{}"))
                    .build();

            HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
            return parseResponse(response.body());
        } catch (Exception e) {
            Map<String, Object> errorResponse = new HashMap<>();
            errorResponse.put("success", false);
            errorResponse.put("error", "Connection error: " + e.getMessage());
            return errorResponse;
        }
    }

    /**
     * Leave a game
     * @param gameId Game ID to leave
     * @param username Current username
     * @return true if leave was successful, false otherwise
     */
    public boolean leaveGame(String gameId, String username) {
        try {
            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(apiBaseUrl + "/api/games/" + gameId + "/leave"))
                    .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                    .header("X-Username", username)
                    .POST(HttpRequest.BodyPublishers.ofString("{}"))
                    .build();

            HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
            Map<String, Object> responseMap = parseResponse(response.body());

            return (boolean) responseMap.get("success");
        } catch (Exception e) {
            return false;
        }
    }

    /**
     * Parse JSON response
     * @param responseBody Response body
     * @return Parsed response as Map
     */
    private Map<String, Object> parseResponse(String responseBody) {
        try {
            return objectMapper.readValue(responseBody, new TypeReference<Map<String, Object>>() {});
        } catch (JsonProcessingException e) {
            Map<String, Object> errorResponse = new HashMap<>();
            errorResponse.put("success", false);
            errorResponse.put("error", "Failed to parse server response");
            return errorResponse;
        }
    }
}
