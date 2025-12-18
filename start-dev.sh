#!/bin/bash

# Script to create a tmux session for general development
# Window 1: Claude
# Window 2: shell

SESSION_NAME=$(basename "$PWD")

# Check if tmux session already exists
if tmux has-session -t "$SESSION_NAME" 2>/dev/null; then
    echo "Session '$SESSION_NAME' already exists. Attaching to existing session."
    tmux attach-session -t "$SESSION_NAME"
    exit 0
fi

# Create new tmux session
echo "Creating tmux session '$SESSION_NAME'..."

# Create session with first window (Claude)
tmux new-session -d -s "$SESSION_NAME" -n "claude"

# Create second window (shell)
tmux new-window -t "$SESSION_NAME:2" -n "shell"

# Start Claude in first window
tmux send-keys -t "$SESSION_NAME:1" "c-z1 --dangerously-skip-permissions" Enter

# Focus on the first window
tmux select-window -t "$SESSION_NAME:1"

# Set terminal title to session name
echo -ne "\033]0;$SESSION_NAME\007"

# Attach to the session
tmux attach-session -t "$SESSION_NAME"