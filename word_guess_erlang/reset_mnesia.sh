#!/bin/bash
# Reset the Mnesia database

# Build the application to ensure all modules are up to date
rebar3 compile

# Start Erlang and call the reset_database function
NODE_NAME="word_guess@$(hostname -s)"
erl -pa _build/default/lib/*/ebin \
    -name $NODE_NAME \
    -eval "word_guess_mnesia:reset_database(), init:stop()." \
    -noshell