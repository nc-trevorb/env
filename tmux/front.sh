#!/usr/bin/env sh

tmux new-window -t dev:2 -n 'admin-ui' -c "${HOME}/code/admin-ui"
tmux send-keys 'e' Enter
tmux split-window -v -p 30 -c "${HOME}/code/admin-ui"
tmux send-keys 'nvm_ && clear' Enter

tmux new-window -t dev:3 -n 'ux-framework' -c "${HOME}/code/ux-framework"
tmux send-keys 'e' Enter
tmux split-window -v -p 30 -c "${HOME}/code/ux-framework"
tmux send-keys 'nvm_ && clear' Enter

tmux new-window -t dev:4 -n 'js-api-client' -c "${HOME}/code/js-api-client"
tmux send-keys 'e' Enter
tmux split-window -v -p 30 -c "${HOME}/code/js-api-client"

tmux new-window -t dev:5 -n 'design-objects' -c "${HOME}/code/design-objects"
tmux send-keys 'e' Enter
tmux split-window -v -p 30 -c "${HOME}/code/design-objects"
