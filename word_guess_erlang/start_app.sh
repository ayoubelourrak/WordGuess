#!/bin/bash
set -e

# Create Mnesia directory if it doesn't exist
mkdir -p mnesia_data

# Build the application
rebar3 compile

# Start with the proper configuration
NODE_NAME="word_guess@$(hostname -s)"
erl -pa _build/default/lib/*/ebin \
    -name $NODE_NAME \
    -mnesia dir '"mnesia_data"' \
    -eval "application:ensure_all_started(word_guess_erlang)." \
    -noshell