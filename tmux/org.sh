#!/usr/bin/env sh

dc org

tmux new-window -t org:2 -n 'paste' -c "${HOME}/org"
tmux send-keys 'e scripts/paste.rb' Enter

tmux new-window -t org:3 -n '.emacs.d' -c "${HOME}/env/emacs"
tmux send-keys 'e modalka-keys.el init.el' Enter

tmux new-window -t org:4 -n 'zshrc' -c "${HOME}"
tmux send-keys 'e .zshrc' Enter

tmux rename-window -t 1 'todo'
tmux select-window -t 1
tmux send-keys 'e log.org' Enter
