package it.unipi.dsmt.word_guess.controller;

import it.unipi.dsmt.word_guess.model.Game;
import it.unipi.dsmt.word_guess.service.ApiService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import jakarta.servlet.http.HttpSession;
import java.util.List;
import java.util.Map;

@Controller
public class GameController {

    @Value("${api.websocket-base-url}")
    private String wsBaseUrl;
    private final ApiService apiService;

    @Autowired
    public GameController(ApiService apiService) {
        this.apiService = apiService;
    }

    /**
     * Default route - redirect to games list
     */
    @GetMapping("/")
    public String index() {
        return "redirect:/games";
    }

    /**
     * Show the list of public games
     */
    @GetMapping("/games")
    public String showGamesList(Model model, HttpSession session) {
        String username = (String) session.getAttribute("username");
        List<Game> games = apiService.getPublicGames(username);

        model.addAttribute("games", games);
        model.addAttribute("newGame", new Game());
        model.addAttribute("apiWebsocketBaseUrl", wsBaseUrl);
        return "gamelist";
    }

    /**
     * Create a new game
     */
    @PostMapping("/games/create")
    public String createGame(@ModelAttribute("newGame") Game game, HttpSession session, RedirectAttributes redirectAttributes) {
        String username = (String) session.getAttribute("username");

        // Create the game
        Game createdGame = apiService.createGame(game, username);

        if (createdGame != null) {
            // Redirect to the newly created game
            return "redirect:/games/" + createdGame.getId();
        } else {
            // Add error message and return to game list
            redirectAttributes.addFlashAttribute("error", "Failed to create game");
            return "redirect:/games";
        }
    }

    /**
     * Join a game by ID
     */
    @GetMapping("/games/{gameId}/join")
    public String joinGame(@PathVariable String gameId, HttpSession session, RedirectAttributes redirectAttributes) {
        String username = (String) session.getAttribute("username");

        // Join the game
        Game joinedGame = apiService.joinGame(gameId, username);

        if (joinedGame != null) {
            // Redirect to the game
            return "redirect:/games/" + gameId;
        } else {
            // Add error message and return to game list
            redirectAttributes.addFlashAttribute("error", "Failed to join game");
            return "redirect:/games";
        }
    }

    /**
     * Join a game by code (private game)
     */
    @PostMapping("/games/join")
    public String joinGameByCode(@RequestParam("gameCode") String gameCode, HttpSession session, RedirectAttributes redirectAttributes) {
        // Validate game code
        if (gameCode == null || gameCode.trim().isEmpty()) {
            redirectAttributes.addFlashAttribute("error", "Game code is required");
            return "redirect:/games";
        }

        // Try to join the game
        return joinGame(gameCode, session, redirectAttributes);
    }

    /**
     * Show the gameplay page for a specific game
     */
    @GetMapping("/games/{gameId}")
    public String showGameplay(@PathVariable String gameId, Model model, HttpSession session, RedirectAttributes redirectAttributes) {
        String username = (String) session.getAttribute("username");

        // Get the game
        Game game = apiService.getGame(gameId, username);

        if (game != null) {
            // Check if user is a player in this game
            if (game.getPlayers() != null && game.getPlayers().contains(username)) {
                model.addAttribute("game", game);
                model.addAttribute("username", username);
                model.addAttribute("apiWebsocketBaseUrl", wsBaseUrl);
                return "gameplay";
            } else {
                // User is not a player, redirect to join the game
                return "redirect:/games/" + gameId + "/join";
            }
        } else {
            // Game not found, return to game list
            redirectAttributes.addFlashAttribute("error", "Game not found");
            return "redirect:/games";
        }
    }

    @PostMapping("/games/{gameId}/start")
    @ResponseBody
    public Map<String, Object> startGame(@PathVariable String gameId, HttpSession session) {
        String username = (String) session.getAttribute("username");

        // Start the game
        Map<String, Object> response = apiService.startGameWithResponse(gameId, username);
        return response;
    }

    /**
     * Leave a game
     */
    @PostMapping("/games/{gameId}/leave")
    public String leaveGame(@PathVariable String gameId, HttpSession session, RedirectAttributes redirectAttributes) {
        String username = (String) session.getAttribute("username");

        // Leave the game
        boolean success = apiService.leaveGame(gameId, username);

        if (success) {
            // Redirect to game list
            redirectAttributes.addFlashAttribute("success", "You have left the game");
            return "redirect:/games";
        } else {
            // Add error message and return to game
            redirectAttributes.addFlashAttribute("error", "Failed to leave game");
            return "redirect:/games/" + gameId;
        }
    }
}