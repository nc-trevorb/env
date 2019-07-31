#!/usr/bin/env sh

# usage: ./new_session.sh dev
session_name="$1"

echo '0: tmux ls'
tmux ls
echo "1: creating new session ${session_name}"
echo "1: creating new session $1"
tmux new-session -d "$session_name"
echo '2'
tmux ls
echo '2.5'
tmux attach-session -t "$session_name"
echo '3'

# echo '1'
# tmux new-session -d "$session_name" "source ${HOME}/env/tmux/${session_name}.sh"
# echo '2'
# tmux attach-session -t "$session_name"
# echo '3'
