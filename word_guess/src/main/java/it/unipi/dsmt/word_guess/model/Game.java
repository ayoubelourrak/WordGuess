package it.unipi.dsmt.word_guess.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class Game {
    private String id;
    private String word;
    private String maskedWord;
    private int maxPlayers;
    @JsonProperty("isPublic")
    private boolean isPublic;
    private String creator;
    private List<String> players;
    private String currentPlayer;
    private List<String> guessedLetters;
    private List<String> guessedWords;
    private String status;
    private String winner;
    private String error;
    private long createdAt;
}
