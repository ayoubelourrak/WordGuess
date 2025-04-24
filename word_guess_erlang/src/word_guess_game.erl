%%%-------------------------------------------------------------------
%%% @doc word_guess game logic implementation
%%%-------------------------------------------------------------------
-module(word_guess_game).

%% API
-export([
    process_guess/2,
    get_random_word/0,
    to_json/1,
    player_left/2,
    start_game/2,
    turn_timeout/1,
    get_player_index/2
]).

-include("word_guess_records.hrl").

%%===================================================================
%% API
%%===================================================================

%% @doc Process a letter or word guess and update the game state
-spec process_guess(#game{}, string()) -> {atom(), #game{}}.
process_guess(Game, Guess) ->
    case Game#game.status of
        completed ->
            {error, Game#game{error = "Game is already completed"}};
        in_progress ->
            case string:len(Guess) of
                1 -> process_letter_guess(Game, Guess);
                _ -> process_word_guess(Game, Guess)
            end;
        _ ->
            {error, Game#game{error = "Game is not in progress"}}
    end.

%% @doc Return a random word from JSON file
-spec get_random_word() -> string().
get_random_word() ->
    % Path to the JSON file
    FilePath = case file:get_cwd() of
        {ok, Cwd} -> filename:join([Cwd, "priv/words.json"]);
        _ -> "priv/words.json"  % Fallback if current directory can't be determined
    end,
    
    % Try to load the words from the JSON file
    case file:read_file(FilePath) of
        {ok, Binary} ->
            try
                % Parse the JSON content
                JsonData = jsx:decode(Binary, [return_maps]),
                
                % Extract the words list
                case maps:get(<<"words">>, JsonData, undefined) of
                    undefined ->
                        % Fallback if JSON structure is incorrect
                        io:format("[ERROR] Invalid JSON structure in words file~n"),
                        get_fallback_word();
                    Words when is_list(Words), length(Words) > 0 ->
                        % Pick a random word from the list
                        WordBinary = lists:nth(rand:uniform(length(Words)), Words),
                        Word = binary_to_list(WordBinary),
                        io:format("[INFO] Chosen Word: ~p~n", [Word]),
                        Word;
                    _ ->
                        % Fallback if words is not a list or is empty
                        io:format("[ERROR] Invalid words list in JSON file~n"),
                        get_fallback_word()
                end
            catch
                Error:Reason ->
                    % Fallback if JSON parsing fails
                    io:format("[ERROR] Failed to parse JSON: ~p:~p~n", [Error, Reason]),
                    get_fallback_word()
            end;
        {error, Reason} ->
            % Fallback if file can't be read
            io:format("[ERROR] Failed to read words file: ~p~n", [Reason]),
            get_fallback_word()
    end.

%% @doc Return a fallback word if JSON reading fails
-spec get_fallback_word() -> string().
get_fallback_word() ->
    % Fallback list in case JSON file can't be read
    FallbackWords = [
        "computer", "software", "internet", "database", "algorithm"
    ],
    Word = lists:nth(rand:uniform(length(FallbackWords)), FallbackWords),
    io:format("[INFO] Using fallback word: ~p~n", [Word]),
    Word.

%% @doc Convert game record to JSON representation
-spec to_json(#game{}) -> map().
to_json(Game) ->
    #{
        id => list_to_binary(Game#game.id),
        word => case Game#game.status of
            completed -> list_to_binary(Game#game.word);
            _ -> null
        end,
        maskedWord => list_to_binary(Game#game.maskedWord),
        maxPlayers => Game#game.maxPlayers,
        isPublic => Game#game.isPublic,
        creator => list_to_binary(Game#game.creator),
        players => [list_to_binary(Player) || Player <- Game#game.players],
        currentPlayer => case length(Game#game.players) of
            0 -> null;
            Len when Game#game.currentPlayerIndex < Len ->
                list_to_binary(lists:nth(Game#game.currentPlayerIndex + 1, Game#game.players));
            _ -> null  % Index out of range, return null
        end,
        guessedLetters => [list_to_binary([Letter]) || Letter <- Game#game.guessedLetters],
        guessedWords => [list_to_binary(Word) || Word <- Game#game.guessedWords],
        status => atom_to_binary(Game#game.status, utf8),
        createdAt => Game#game.createdAt,
        error => case Game#game.error of
            undefined -> null;
            Error -> list_to_binary(Error)
        end,
        winner => case Game#game.winner of
            undefined -> null;
            Winner -> list_to_binary(Winner)
        end
    }.

%% @doc Handle player leaving the game
-spec player_left(#game{}, string()) -> {atom(), #game{}}.
player_left(Game, Player) ->
    % Check if player is in the game
    case lists:member(Player, Game#game.players) of
        false ->
            {error, Game#game{error = "Player not in game"}};
        true ->
            % Remove player from the list
            UpdatedPlayers = lists:delete(Player, Game#game.players),
            
            % Check if this was the last player
            case length(UpdatedPlayers) of
                0 ->
                    % No players left, mark game as completed
                    {completed, Game#game{
                        players = UpdatedPlayers,
                        status = completed
                    }};
                1 when Game#game.status =:= in_progress ->
                    % Only one player left in an active game, they win
                    [Winner | _] = UpdatedPlayers,
                    {winner, Game#game{
                        players = UpdatedPlayers,
                        status = completed,
                        winner = Winner
                    }};
                _ ->
                    % Handle current player index if the leaving player was the current player
                    CurrentPlayerIndex = Game#game.currentPlayerIndex,
                    CurrentPlayer = lists:nth(CurrentPlayerIndex + 1, Game#game.players),
                    
                    % Determine the new current player index
                    NewIndex = case Player =:= CurrentPlayer of
                        true ->
                            % Current player left, use the same index (will point to next player)
                            CurrentPlayerIndex rem length(UpdatedPlayers);
                        false ->
                            % Someone else left, adjust index if needed
                            PlayerIndex = get_player_index(Game#game.players, Player),
                            if
                                PlayerIndex < CurrentPlayerIndex ->
                                    % Adjust index down by one
                                    (CurrentPlayerIndex - 1) rem length(UpdatedPlayers);
                                true ->
                                    % Keep the same index
                                    CurrentPlayerIndex rem length(UpdatedPlayers)
                            end
                    end,
                    
                    {ok, Game#game{
                        players = UpdatedPlayers,
                        currentPlayerIndex = NewIndex
                    }}
            end
    end.

%% @doc Start the game (called by game creator)
-spec start_game(#game{}, string()) -> {atom(), #game{}}.
start_game(Game, Creator) ->
    case Game#game.status of
        waiting ->
            % Verify the request is from the creator
            case Creator =:= Game#game.creator of
                true ->
                    % Start the game if there are at least 2 players
                    case length(Game#game.players) >= 2 of
                        true ->
                            {ok, Game#game{status = in_progress}};
                        false ->
                            {error, Game#game{error = "Not enough players, minimum 2"}}
                    end;
                false ->
                    {error, Game#game{error = "Only the creator can start the game"}}
            end;
        _ ->
            {error, Game#game{error = "Game is already started or completed"}}
    end.

%% @doc Handle turn timeout
-spec turn_timeout(#game{}) -> {atom(), #game{}}.
turn_timeout(Game) ->
    case Game#game.status of
        in_progress ->
            % Move to the next player
            NextPlayerIndex = (Game#game.currentPlayerIndex + 1) rem length(Game#game.players),
            {timeout, Game#game{currentPlayerIndex = NextPlayerIndex}};
        _ ->
            {error, Game#game{error = "Game is not in progress"}}
    end.

%%===================================================================
%% Internal functions
%%===================================================================

%% @doc Process a single letter guess
-spec process_letter_guess(#game{}, string()) -> {atom(), #game{}}.
process_letter_guess(Game, [Letter]) ->
    % Check if this letter was already guessed
    GuessedLetters = Game#game.guessedLetters,
    case lists:member(Letter, GuessedLetters) of
        true ->
            {error, Game#game{error = "Letter already guessed"}};
        false ->
            % Update guessed letters
            UpdatedGuessedLetters = [Letter | GuessedLetters],
            
            % Check if letter is in the word
            Word = Game#game.word,
            case is_letter_in_word(Letter, Word) of
                true ->
                    % Update masked word
                    UpdatedMaskedWord = update_masked_word(Game#game.maskedWord, Word, Letter),
                    
                    % Check if the word is completely revealed
                    case lists:member($_, UpdatedMaskedWord) of
                        false ->
                            % Word guessed - game completed
                            Players = Game#game.players,
                            CurrentPlayerIndex = Game#game.currentPlayerIndex,
                            Winner = lists:nth(CurrentPlayerIndex + 1, Players),
                            
                            {win, Game#game{
                                maskedWord = UpdatedMaskedWord,
                                guessedLetters = UpdatedGuessedLetters,
                                status = completed,
                                winner = Winner
                            }};
                        true ->
                            % Continue playing, same player's turn (correct guess)
                            {correct, Game#game{
                                maskedWord = UpdatedMaskedWord,
                                guessedLetters = UpdatedGuessedLetters
                            }}
                    end;
                false ->
                    % Wrong guess, next player's turn
                    NextPlayerIndex = (Game#game.currentPlayerIndex + 1) rem length(Game#game.players),
                    
                    {wrong, Game#game{
                        guessedLetters = UpdatedGuessedLetters,
                        currentPlayerIndex = NextPlayerIndex
                    }}
            end
    end.

%% @doc Process a word guess
-spec process_word_guess(#game{}, string()) -> {atom(), #game{}}.
process_word_guess(Game, Word) ->
    % Check if this word was already guessed
    GuessedWords = Game#game.guessedWords,
    case lists:member(Word, GuessedWords) of
        true ->
            {error, Game#game{error = "Word already guessed"}};
        false ->
            % Update guessed words
            UpdatedGuessedWords = [Word | GuessedWords],
            
            % Check if the guess is correct
            case string:equal(Word, Game#game.word, true) of
                true ->
                    % Word guessed - game completed
                    Players = Game#game.players,
                    CurrentPlayerIndex = Game#game.currentPlayerIndex,
                    Winner = lists:nth(CurrentPlayerIndex + 1, Players),
                    
                    {win, Game#game{
                        maskedWord = Game#game.word,
                        guessedWords = UpdatedGuessedWords,
                        status = completed,
                        winner = Winner
                    }};
                false ->
                    % Wrong guess, next player's turn
                    NextPlayerIndex = (Game#game.currentPlayerIndex + 1) rem length(Game#game.players),
                    
                    {wrong, Game#game{
                        guessedWords = UpdatedGuessedWords,
                        currentPlayerIndex = NextPlayerIndex
                    }}
            end
    end.

%% @doc Check if a letter is in the word
-spec is_letter_in_word(char(), string()) -> boolean().
is_letter_in_word(Letter, Word) ->
    lists:member(Letter, Word).

%% @doc Update the masked word with correctly guessed letter
-spec update_masked_word(string(), string(), char()) -> string().
update_masked_word(MaskedWord, Word, Letter) ->
    update_masked_word(MaskedWord, Word, Letter, []).

update_masked_word([], [], _Letter, Acc) ->
    lists:reverse(Acc);
update_masked_word([_M | MRest], [W | WRest], Letter, Acc) when W =:= Letter ->
    update_masked_word(MRest, WRest, Letter, [Letter | Acc]);
update_masked_word([M | MRest], [_ | WRest], Letter, Acc) ->
    update_masked_word(MRest, WRest, Letter, [M | Acc]).

%% @doc Get index of a player in the player list
-spec get_player_index([string()], string()) -> integer().
get_player_index(Players, Player) ->
    get_player_index(Players, Player, 0).

get_player_index([], _, _) ->
    -1;
get_player_index([Player | _], Player, Index) ->
    Index;
get_player_index([_ | Rest], Player, Index) ->
    get_player_index(Rest, Player, Index + 1).