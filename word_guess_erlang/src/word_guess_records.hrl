%% User record
-record(user, {
    username :: string(),        % Primary key
    password_hash :: binary(),   % SHA-256 hash of password
    createdAt :: integer()      % Unix timestamp
}).

%% Game record
-record(game, {
    id :: string(),              % Game identifier (primary key)
    word :: string(),            % Word to guess
    maskedWord :: string(),     % Word with unguessed letters masked
    maxPlayers :: integer(),    % Maximum number of players
    isPublic :: boolean(),      % Whether the game is publicly visible
    creator :: string(),         % Username of game creator
    players = [] :: [string()],  % List of player usernames
    currentPlayerIndex = 0 :: integer(), % Index of current player in players list
    guessedLetters = [] :: [char()], % List of guessed letters
    guessedWords = [] :: [string()], % List of guessed words
    status = waiting :: waiting | in_progress | completed, % Game status
    winner :: string(),          % Username of the winner (if completed)
    error :: string(),           % Error message if any
    createdAt :: integer()      % Unix timestamp
}).