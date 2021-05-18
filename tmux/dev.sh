#!/usr/bin/env sh

# # env window
# tmux new-window -t dev:7 -n 'env' -c "${HOME}/env"
# tmux send-keys 'e' Enter
# tmux split-window -v -p 30
# tmux send-keys 'd ~/env' Enter C-l

# emacs window
# tmux new-window -t dev:8 -n '.emacs.d' -c "${HOME}/env/emacs"
# tmux send-keys 'e' Enter
# tmux split-window -v -p 30
# tmux send-keys 'd ~/env/emacs' Enter C-l # get rid of these after pane_current_path works again

# org window
# tmux new-window -t dev:9 -n 'org' -c "${HOME}/org"
# tmux send-keys 'e t.org' Enter
# tmux split-window -h -p 30
# tmux send-keys 'd ~/org' Enter C-l 't' Enter

# tmux select-pane -t 1

cd ~/code/appserver

tmux new-window -t dev:2 -n 'console' -c "${HOME}/code/appserver"
tmux new-window -t dev:9 -n 'flik' -c "${HOME}/code/appserver"
tmux send-keys 'flik repl system_test_workflow' Enter
tmux send-keys 'status' Enter
tmux split-window -h -c "${HOME}/code/docker-shared"
tmux send-keys 'e script/update.rb script/apps/updatable_app.rb' Enter

tmux select-window -t 1
tmux rename-window -t 1 'appserver'
tmux send-keys 'e' Enter
tmux split-window -v -p 30
tmux select-pane -U
