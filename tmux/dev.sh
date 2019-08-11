#!/usr/bin/env sh

# # env window
# tmux new-window -t dev:7 -n 'env' -c "${HOME}/env"
# tmux send-keys 'e' Enter
# tmux split-window -v -p 30
# tmux send-keys 'd ~/env' Enter C-l

# emacs window
tmux new-window -t dev:8 -n '.emacs.d' -c "${HOME}/env/emacs"
tmux send-keys 'e' Enter
tmux split-window -v -p 30
tmux send-keys 'd ~/env/emacs' Enter C-l # get rid of these after pane_current_path works again

# org window
tmux new-window -t dev:9 -n 'org' -c "${HOME}/org"
tmux send-keys 'e t.org' Enter
tmux split-window -h -p 30
tmux send-keys 'd ~/org' Enter C-l 't' Enter
