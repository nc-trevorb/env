# uncomment this line to enable zsh startup profiling, then run `zprof`
# zmodload zsh/zprof

function shrug() {
    echo -n '¯\_(ツ)_/¯' | pbcopy
}

SHOWTRACE=false
function trace() {
    if [[ $SHOWTRACE == true ]]; then
        ruby ~/trace.rb "$1"
    fi
}

function new_trace() {
    if [[ $SHOWTRACE == true ]]; then
        ruby ~/trace.rb --inc
        trace "$1"
    fi
}

function trace_reset() {
    ruby ~/trace.rb --reset
}

function trace_start() {
    trace_reset
    trace "$1"
}

# trace_start "zshrc"

OS_NAME='macOS'
# OS_NAME='debian'
# OS_NAME='openbsd'

### OS-specific ###
case "$OS_NAME" in
    openbsd)
        function t() {
            if [[ "$PWD" == "$HOME" ]]; then
                # remove this when tmux pane_current_path is working
                echo "can't use tree in home"
            else
                tree -F "$@"
            fi
        }
        # alias t='tree'
        alias ls='colorls -G'
        alias l='ls -lhF'

        export LC_CTYPE="en_US.UTF-8"
        export PAGER='less -XRW'

        ps_os_color="$(tput setab 6 _ _)$(tput setaf 0 _ _)"
        ps_color="$(tput setab 1 _ _)$(tput setaf 0 _ _)"
        ;;

    macOS)
        alias t='tree -C --dirsfirst -I "coverage|build|dist|*srv|elm-stuff|_build|*.byte|*.native|__pycache__|static" '
        alias ls='ls -G'
        alias l='ls -lhp'
        alias grep='grep --color=auto'
        alias v='vagrant'

        ps_os_color="$(tput setab 3)$(tput setaf 0)"
        ps_color="$(tput setab 5)$(tput setaf 0)"
        ;;

    debian)
        alias t='tree -C --dirsfirst -I "coverage|build|dist|*srv|elm-stuff|_build|*.byte|*.native|__pycache__|static" '
        alias ls='ls -G'
        alias l='ls -lhp'
        alias grep='grep --color=auto'
        alias v='vagrant'

        ps_os_color="$(tput setab 2)$(tput setaf 0)"
        ps_color="$(tput setab 4)$(tput setaf 0)"
        ;;
esac

### Completion ###
setopt COMPLETE_IN_WORD
autoload -U compinit
compinit
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' # case insensitive completion


### Colors ###
export CLICOLOR=1
autoload colors
colors


### History ###
export HISTFILE=$HOME/.history_zsh
export HISTSIZE=10000
export SAVEHIST=10000
setopt EXTENDED_HISTORY     # add timestamps to history
setopt APPEND_HISTORY       # adds history
setopt INC_APPEND_HISTORY   # adds history incrementally
setopt SHARE_HISTORY        # share across sessions
setopt HIST_IGNORE_ALL_DUPS # don't record dupes in history
setopt HIST_IGNORE_DUPS
setopt HIST_REDUCE_BLANKS


### Settings ###
unsetopt correct_all
bindkey -e                                       # use emacs key bindings
# bindkey '^r' history-incremental-search-backward # make Control-r work
bindkey '^[[Z' reverse-menu-complete             # shift-tab to cycle backwards
bindkey "^[[3~" delete-char                      # make delete key work
bindkey "^[3;5~" delete-char                     # make delete key work
# bindkey "^U" backward-kill-line                  # C-u acts like bash
autoload -U select-word-style
select-word-style bash

bindkey '^b' backward-kill-word
bindkey '^f' kill-word
bindkey '^p' yank
# bindkey "^'" history-incremental-search-backward
bindkey "^:" redo
bindkey '^u' undo
bindkey '^v' delete-char-or-list
bindkey '^t' kill-line
bindkey '^w' kill-line

bindkey "^['" history-incremental-search-backward
bindkey '^[N' history-incremental-search-forward
bindkey '^[E' history-incremental-search-backward
bindkey '^[h' backward-char
bindkey '^[n' down-line-or-history
bindkey '^[e' up-line-or-history
bindkey '^[i' forward-char
bindkey '^[l' backward-word
bindkey '^[y' forward-word
bindkey '^[o' forward-word
bindkey '^[z' beginning-of-line
bindkey "^[-" end-of-line

bindkey '^[w' kill-whole-line
bindkey '^[t' kill-whole-line
bindkey "^[:" redo
bindkey '^[u' undo
bindkey '^[<' reset-prompt

# can't be escape char
# stty intr ^[<

setopt LOCAL_OPTIONS # allow functions to have local options
setopt LOCAL_TRAPS   # allow functions to have local traps
setopt PROMPT_SUBST
# setopt AUTO_CD
setopt RC_QUOTES     # 'allow ''single'' quote interpolation'
stty -ixon -ixoff    # disable scroll lock
export EDITOR=vim
set -o emacs
# export TZ="America/Chicago"

function try_source() {
    [ -f "$1" ] && source "$1"
}

try_source $HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
try_source $HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=244'
# try_source $HOME/.zsh/
# try_source /usr/local/share/chruby/chruby.sh
# try_source /usr/local/share/chruby/auto.sh
# try_source /usr/local/share/gem_home/gem_home.sh


### Custom history ###
COMMAND_LOGGING=true
CUSTOM_HISTFILE=~/env/zsh/history/current
CUSTOM_HISTDIR=~/env/zsh/history/

function dont_log_that() {
    local ocl=$COMMAND_LOGGING
    export COMMAND_LOGGING=false
    echo -e '$d\n$d\nwq' | ed $CUSTOM_HISTFILE # deletes last two lines ( one is `export COMMAND_LOGGING=false`)
    export COMMAND_LOGGING=$ocl
}

mkdir -p $CUSTOM_HISTDIR
touch $CUSTOM_HISTFILE

function log_commands() {
    if [[ $(cat $CUSTOM_HISTFILE | wc -l) -gt 5000 ]]; then
        echo "logging ${CUSTOM_HISTFILE}"
        mv $CUSTOM_HISTFILE $CUSTOM_HISTDIR/$(date +%Y_%m_%d)
        touch $CUSTOM_HISTFILE
    fi
    [[ $COMMAND_LOGGING = true ]] && echo "$(date '+%Y-%m-%d\t%H:%M:%S')\t$(pwd)\t$1" >> $CUSTOM_HISTFILE
}

if [[ ! "$preexec_functions" == *log_commands* ]]; then
    preexec_functions+=("log_commands")
fi

### Prompt ###
# plain='\e[0m'

# function ps1_() {
#         if git rev-parse 2> /dev/null; then
#                 ps_git_branch="${ps_color}git:$(git rev-parse --abbrev-ref HEAD)${plain} "
#         else
#                 ps_git_branch=""
#         fi

#         if [[ -z $PS_PYTHON_ENV ]]; then
#                 ps_python_env=""
#         else
#                 ps_python_env="${ps_color}python:$PS_PYTHON_ENV${plain} "
#         fi

#         local ps_ruby=$(type chruby > /dev/null && chruby | grep \* | sed 's/.* //g')
#         if [[ -z $ps_ruby ]]; then
#                 ps_ruby_version=""
#         else
#                 ps_ruby_version="${ps_color}ruby:${ps_ruby}${plain} "
#         fi

#         ps_os="${ps_os_color}${OS_NAME}${plain} "
#         ps_timestamp="${ps_color}$(date +'%H:%M:%S')${plain} "
#         ps_dir="${ps_color}%~${plain} "
#         ps_env_vars="${ps_ruby_version}${ps_python_env}${ps_git_branch}"

#         echo "${ps_os}${ps_timestamp}${ps_dir}${ps_env_vars}"
# }

# export PS1='$(ps1_)
# > '


prompt_time_color='cyan'
prompt_path_color='blue'
prompt_branch_color='magenta'
prompt_ruby_version_color='red'

function inside_git_repo() { git rev-parse --git-dir > /dev/null 2>&1 }

function ps_color() {
    local color=$1
    local str=$2

    echo "%{$bg[${color}]$fg[black]%}${str}%{$reset_color%}"
}

function ps_element() {
    echo "$(ps_color $1 $2)"
}

function ps_element_field() {
    echo "$(ps_color $1 $2)"
}

function ps_pwd() { ps_element $prompt_path_color "%~" }
function ps_timestamp() { ps_element $prompt_time_color "$(date +'%H:%M:%S')" }
function ps_git_branch() {
    if inside_git_repo; then
        local branch_name=$(git rev-parse --abbrev-ref HEAD)
        ps_color $prompt_branch_color "$branch_name"
    fi
}

function ps_extras() {
    local ps_branch=$(ps_git_branch)
    local ps_ruby_version="$(echo "${RUBY_ROOT##*/}")"
    local ps_gem_home="$(echo "${GEM_HOME##*/}")"
    local ps_nvm_flag=''

    # -n means non-empty string
    if [[ -n $ps_ruby_version ]]; then
        ps_ruby_version=$(ps_color red "${ps_ruby_version}")
    fi

    if [[ -n $ps_branch ]]; then
        ps_branch=$(ps_color red "${ps_branch}")
        if [[ -n $ps_gem_home ]] || [[ -n $ps_ruby_version ]]; then
            ps_branch="${ps_branch} "
        fi
    fi

    if [[ -n $ps_gem_home ]]; then
        ps_gem_home=$(ps_color red "${ps_gem_home}")
        if [[ -n $ps_branch ]] || [[ -n $ps_ruby_version ]]; then
            ps_gem_home="@${ps_gem_home}"
        fi
    fi

    if [[ $NVM_FLAG == 'true' ]]; then
        ps_nvm_flag=$(ps_color yellow 'nvm')
        if [[ -n "${ps_branch}${ps_ruby_version}${ps_gem_home}" ]]; then
            ps_nvm_flag="${ps_nvm_flag} "
        fi
    fi

    local elements="${ps_nvm_flag}${ps_branch}${ps_ruby_version}${ps_gem_home}"
    if [[ -n $elements ]]; then
        echo "$elements"
    else
        echo ""
    fi
}

export PS1='$(ps_timestamp) $(ps_pwd) $(ps_extras)
> '

### Aliases ###
alias sudo='sudo ' # make aliases work with "sudo"
alias be='bundle exec '

alias rg='rg -i -M 500'
alias rga='rg --no-ignore'
alias rgall='rg -M 0 --no-ignore'

alias e='emacs'
alias org='LOAD_ORG=true emacs'

alias t2='t -L 2'

### Functions ###
function tmux_() {
    echo 'here'
    ./env/tmux/new_session.sh dev
}

function f() {
    find $1 -not -path "*/.git/*"
}

# function d() {
#     # echo '---'
#     # echo $1
#     # echo ${1##*/}
#     # echo ${1##*/} | tr \[:upper:] \[:lower:]
#     # weird behavior that has been causing issues:
#     # pushd -> runs chpwd
#     # pushd -q -> does not run chpwd
#     # this is documented: https://github.com/zsh-users/zsh/blob/master/Functions/Chpwd/cdr#L76-L78

#     # new idea: use cd instead
#     # this loses the history of dirs, but I rarely use that
#     # I can probably get good-enough behavior for that by parsing .zsh/history/current to get list of last few dirs
#     # can't use the same pattern `du; du; du` because that alters the hist file
#     # so run `du`, and "interactively" get the dir to go back to
#     # behavior is still a little different then (not actually popping a stack, just adding a new entry on top)
#     pushd -q "$@"
# }

alias d='cd'
alias du='cd -'

function duu() {
    ruby ~/duu.rb
    cd $(cat ~/duufile)
}

# function du () {
#     popd -q "$@"
# }

# function d() {
#     echo 'd start'
#     if [[ -z "$1" ]]; then
#         pushd $HOME > /dev/null
#     else
#         pushd "$1" > /dev/null
#     fi
#     echo 'd end'
#     # chpwd
# }

# function du() {
#     popd # 2>&1 > /dev/null
#     # chpwd
# }

function d.() {
    chpwd
}

function ds() {
    dirs | tr ' ' '\n'
}

function pss() {
    local process_names=$(echo $@ | sed 's/ /\\|/g')
    ps aux | grep -v grep | grep "$process_names"
}

function wip() {
    if [[ $@ == "" ]]; then
        local msg=$(date +'%H:%M:%S')
    else
        local msg="$(echo $(date +'%H:%M:%S') '==>' $@)"
    fi

    git add -A
    git commit -m $msg
}

export NC_USER_ID=trevorb


# opam configuration
test -r /home/vagrant/.opam/opam-init/init.zsh && . /home/vagrant/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

TZ='America/Denver'; export TZ
bindkey "^[-" end-of-line
alias doc='docker'
alias dom='docker-compose'
export NVM_FLAG='false'
export NVM_DIR="$HOME/.nvm"
function nvm_() {
    printf 'setting up nvm...'
    . "/usr/local/opt/nvm/nvm.sh"
    export NVM_FLAG='true'
    echo 'done'
}

# r usually repeats the last command
alias r='echo no'

# for appserver integration tests
export target_api=sit

export NC_JIRA_IDIOT_MODE=true
function changed-spec-files() {
    git diff master --stat --name-only --diff-filter=d | grep '_spec.rb$' | paste -sd ' ' -
}

function s() {
    local script_name=$1; shift

    if [[ "$script_name" == "testall" ]]; then
        ./script/test $(git diff master --stat --name-only --diff-filter=d | grep '_spec.rb$' | paste -sd ' ' -)
    else
        ./script/$script_name $@
    fi
}

function ss() {
    local script_name=$1; shift

    if ([[ "$script_name" == "test" ]] || [[ "$script_name" == "testall" ]]); then
        if [[ "$(basename $PWD)" == "appserver" ]]; then
            docker-compose restart app_spring
        fi
    fi

    if [[ "$script_name" == "testall" ]]; then
        ./script/test $(git diff master --stat --name-only --diff-filter=d | grep '_spec.rb$' | paste -sd ' ' -)
    else
        ./script/$script_name $@
    fi
}

function pw() {
    echo -n $(rg $1.:: ~/org/keep.org | sed 's/.*:: //g') | pbcopy
}

function username() {
    echo -n $(rg $1-username.:: ~/org/keep.org | sed 's/.*:: //g') | pbcopy
}

function commands() {
    rg "/Users/trevorb[^	]*	${1}" ~/terminal_histories | sed "s/.*\/Users\/trevorb[^	]*	//g" | sort | uniq
}

function dull_red()     { echo "$(tput setaf 1)$1$(tput sgr0)" }
function dull_green()   { echo "$(tput setaf 2)$1$(tput sgr0)" }
function dull_yellow()  { echo "$(tput setaf 3)$1$(tput sgr0)" }
function dull_blue()    { echo "$(tput setaf 4)$1$(tput sgr0)" }
function dull_magenta() { echo "$(tput setaf 5)$1$(tput sgr0)" }
function dull_cyan()    { echo "$(tput setaf 6)$1$(tput sgr0)" }
function dull_white()   { echo "$(tput setaf 7)$1$(tput sgr0)" }
function dull_black()   { echo "$(tput setaf 8)$1$(tput sgr0)" }
function hot_red()      { echo "$(tput setaf 9)$1$(tput sgr0)" }
function hot_green()    { echo "$(tput setaf 10)$1$(tput sgr0)" }
function hot_yellow()   { echo "$(tput setaf 11)$1$(tput sgr0)" }
function hot_blue()     { echo "$(tput setaf 12)$1$(tput sgr0)" }
function hot_magenta()  { echo "$(tput setaf 13)$1$(tput sgr0)" }
function hot_cyan()     { echo "$(tput setaf 14)$1$(tput sgr0)" }
function hot_white()    { echo "$(tput setaf 15)$1$(tput sgr0)" }
function hot_black()    { echo "$(tput setaf 16)$1$(tput sgr0)" }

# sshaa: ssha appserver - sets iterm2 badge, connects to appserver with ssha, and unsets badge when closed
# first arg: server
# mt|prod -> multitenant
# ta -> transamerica
# jh -> john hancock
# sit -> sit
# uat -> uat

# examples:
# sshaa sit -> ssha session -t sit_appserver nonprod
# sshaa ta -> ssha session -t transamerica_appserver ta
# sshaa ta ro -> ssha session -t transamerica_appserver_ro ta

function set_iterm_badge() {
    printf "\e]1337;SetBadgeFormat=%s\a" $(echo -n "${1}" | base64)
}

function iterm_clear_badge() {
    set_iterm_badge ""
}

export SSHAA_COMMAND="session -u root -t"
# export SSHAA_COMMAND="rails -t"

function sshaa() {
    local server_name=""
    local server_env=""
    local maybe_ro=""
    local error_msg=""
    local server_has_ro_option=true
    local badge="${1}"

    case "${1}" in
        ta) server_name="transamerica"; server_env="ta" ;;
        jh) server_name="johnhancock"; server_env="jh" ;;
        mt|prod) server_name="prod"; server_env="prod" ;;
        sit) server_name="sit"; server_env="nonprod"; server_has_ro_option=false ;;
        uat) server_name="uat"; server_env="nonprod"; server_has_ro_option=false ;;

        *) error_msg="bad server name: '${1}', must be one of [ta, jh, mt|prod, sit, uat]" ;;
    esac

    case "${2}" in
        ro)
            if ${server_has_ro_option}; then
                maybe_ro="_ro"
                badge="${badge} RO"
            else
                error_msg="server ${server_name} has no read-only option"
            fi
            ;;
        "") ;;
        *) error_msg="last arg must be 'ro' or blank" ;;
    esac

    if [[ -n "${error_msg}" ]]; then
        echo "something went wrong: ${error_msg}"
    else
        set_iterm_badge "${badge}"
        local sshaa_cmd="ssha ${SSHAA_COMMAND} ${server_name}_appserver${maybe_ro} ${server_env}"
        echo $sshaa_cmd
        eval $sshaa_cmd
    fi
    set_iterm_badge ""
}


function run_command() {
    dull_blue "$1"
    echo "..."
    echo "$2"
    # FIXME redirect somewhere
    # (this is dumping every line of output, where aws overwrites last line)
    eval "$2"
    dull_green "done"
    echo ""
}
function aws_date() {
    date '+%Y-%m-%d'
}

function s3_archive_and_deploy_() {
    local aws_env="${1}"
    local file_name="${2}"
    local local_path="~/Downloads/${file_name}"
    local archive_path="s3://nc-nonprod-deployment/afa/${aws_env}/archive/$(aws_date)/${file_name}"
    local deploy_path="s3://nc-nonprod-deployment/afa/${aws_env}/${file_name}"

    run_command "copying local artifacts to archive" "aws s3 cp ${local_path} ${archive_path} --profile softwareDeployLead"
    run_command "deploying artifacts from archive" "aws s3 cp ${archive_path} ${deploy_path} --profile softwareDeployLead"
}

function s3_run_deploy() {
    s3_archive_and_deploy_ $1 universe-outcome-server-combo.zip
    s3_archive_and_deploy_ $1 analytic-afa.war
}


function npmtest() {
    npm run test $@ -- --coverage=false
}

function front-ps() {
    pss grunt webpack
}

function open-() {
    case "${1}" in
        lastpass)
            pw lastpass
            open 'https://lastpass.com/misc_login.php'
            ;;
        ups)
            pw UPS
            open 'https://www.ups.com/track?loc=en_US&requester=ST/track?loc=en_US'
            ;;
        *)
            echo "unknown: '$1'"
    esac
}

# chruby.sh
# a few commits behind https://github.com/postmodern/chruby but has some changes too:
### adds auto-gem_home
### more verbose output
### gem_home works in each ruby's .gem dir, rather than the project dir
##### should revisit this, there might be a reason not to do this
CHRUBY_VERSION="0.3.9"
RUBIES=()

for dir in "$PREFIX/opt/rubies" "$HOME/.rubies"; do
    [[ -d "$dir" && -n "$(ls -A "$dir")" ]] && RUBIES+=("$dir"/*)
done
unset dir

function chruby_reset() {
    [[ -z "$RUBY_ROOT" ]] && return

    PATH=":$PATH:"; PATH="${PATH//:$RUBY_ROOT\/bin:/:}"
    [[ -n "$GEM_ROOT" ]] && PATH="${PATH//:$GEM_ROOT\/bin:/:}"

    if (( UID != 0 )); then
        [[ -n "$GEM_HOME" ]] && PATH="${PATH//:$GEM_HOME\/bin:/:}"

        GEM_PATH=":$GEM_PATH:"
        [[ -n "$GEM_HOME" ]] && GEM_PATH="${GEM_PATH//:$GEM_HOME:/:}"
        [[ -n "$GEM_ROOT" ]] && GEM_PATH="${GEM_PATH//:$GEM_ROOT:/:}"
        GEM_PATH="${GEM_PATH#:}"; GEM_PATH="${GEM_PATH%:}"

        unset GEM_HOME
        [[ -z "$GEM_PATH" ]] && unset GEM_PATH
    fi

    PATH="${PATH#:}"; PATH="${PATH%:}"
    unset RUBY_ROOT RUBY_ENGINE RUBY_VERSION RUBYOPT GEM_ROOT
    hash -r
}

function chruby_use() {
#    new_trace 'chruby_use'
    if [[ ! -x "$1/bin/ruby" ]]; then
        echo "chruby: $1/bin/ruby not executable" >&2
        return 1
    fi

    [[ -n "$RUBY_ROOT" ]] && chruby_reset

    export RUBY_ROOT="$1"
    export RUBYOPT="$2"
    export PATH="$RUBY_ROOT/bin:$PATH"

#    trace 'eval'
    eval "$(RUBYGEMS_GEMDEPS="" "$RUBY_ROOT/bin/ruby" - <<EOF
puts "export RUBY_ENGINE=#{Object.const_defined?(:RUBY_ENGINE) ? RUBY_ENGINE : 'ruby'};"
puts "export RUBY_VERSION=#{RUBY_VERSION};"
begin; require 'rubygems'; puts "export GEM_ROOT=#{Gem.default_dir.inspect};"; rescue LoadError; end
EOF
)"
    #echo "using $(dull_red $(basename $1))\n"
    export PATH="${GEM_ROOT:+$GEM_ROOT/bin:}$PATH"
#    trace 'after eval'

    if (( UID != 0 )); then
        export GEM_HOME="$HOME/.gem/$RUBY_ENGINE/$RUBY_VERSION"
        export GEM_PATH="$GEM_HOME${GEM_ROOT:+:$GEM_ROOT}${GEM_PATH:+:$GEM_PATH}"
        export PATH="$GEM_HOME/bin:$PATH"
    fi

    hash -r
}

function chruby()
{
#    new_trace "chruby for ${1}"
    case "$1" in
        -h|--help)
            echo "usage: chruby [RUBY|VERSION|system] [RUBYOPT...]"
            ;;
        -V|--version)
            echo "chruby: $CHRUBY_VERSION"
            ;;
        on)
            CHRUBY_AUTOSWITCH=true
            GEMHOME_AUTOSWITCH=true
            ;;
        off)
            CHRUBY_AUTOSWITCH=false
            GEMHOME_AUTOSWITCH=false
            ;;
        env)
            show_key_value ruby "${RUBY_ROOT##*/}"
            show_key_value gemset "${GEM_HOME##*/}"
            ;;
        full-env)
            echo ''
            echo "$(dull_red PATH:)"
            echo $PATH | tr ':' '\n'
            echo ''
            echo "$(dull_red GEM_HOME) \t=>\t$GEM_HOME"
            echo "$(dull_red GEM_ROOT) \t=>\t$GEM_ROOT"
            echo "$(dull_red GEM_PATH) \t=>\t$GEM_PATH"
            echo ''
            echo "$(dull_red RUBY_ENGINE) \t=>\t$RUBY_ENGINE"
            echo "$(dull_red RUBY_ROOT) \t=>\t$RUBY_ROOT"
            ;;
        ""|list)
            local dir ruby
            for dir in "${RUBIES[@]}"; do
                dir="${dir%%/}"; ruby="${dir##*/}"
                if [[ "$dir" == "$RUBY_ROOT" ]]; then
                    echo " * ${ruby} ${RUBYOPT}"
                else
                    echo "   ${ruby}"
                fi

            done
            ;;
        system) chruby_reset ;;
        *)
            local dir ruby match
            for dir in "${RUBIES[@]}"; do
                dir="${dir%%/}"; ruby="${dir##*/}"
                case "$ruby" in
                    "$1")match="$dir" && break ;;
                    *"$1"*)match="$dir" ;;
                esac
            done

            if [[ -z "$match" ]]; then
                local no_match="chruby could not find match for $(dull_red $1.)"
                local still_using="still using $(dull_green "$(chruby | grep '^ \* ' | sed 's/ \* //')")"
                #echo "\n  $(hot_red $no_match)\n  ($still_using)\n" >&2
                return 1
            fi

            shift
            chruby_use "$match" "$*"
            ;;
    esac
}

# auto.sh
unset RUBY_AUTO_VERSION
export CHRUBY_AUTOSWITCH=true
export GEMHOME_AUTOSWITCH=true
# export NEW_CD=true

function chruby_auto() {
#    new_trace "chruby_auto"
    local dir="$PWD/" version

    until [[ -z "$dir" ]]; do
        dir="${dir%/*}"

        if { read -r version <"$dir/.ruby-version"; } 2>/dev/null || [[ -n "$version" ]]; then
            version="${version%%[[:space:]]}"
            version="${version%%@*}" # strip rvm gemset name

            # trace "found '${version}' in '${dir}'"
            if [[ "$version" == "$RUBY_AUTO_VERSION" ]]; then
                if [[ $GEMHOME_AUTOSWITCH == true ]]; then
                    gem_home_auto $dir
                fi

                return
            else
#                trace "updating RUBY_AUTO_VERSION from '${RUBY_AUTO_VERSION}' to '${version}'"
                RUBY_AUTO_VERSION="$version"
                # echo "\$ chruby $(dull_red $version) -> RUBY_AUTO_VERSION"
                chruby "$version"

                if [[ $GEMHOME_AUTOSWITCH == true ]]; then
                    gem_home_auto $dir
                fi

                return $?
            fi
        fi
    done

    if [[ -n "$RUBY_AUTO_VERSION" ]]; then
#        trace "resetting RUBY_AUTO_VERSION"
        chruby_reset
        unset RUBY_AUTO_VERSION
    fi
}

function gem_home_auto() {
    new_trace "gem_home_auto with $1"
    #echo "\$ gem_home $(dull_red ${PWD##*/})"

    gem_home -
    gem_home "$1"
}

function gem_home_push()
{
    local ruby_engine ruby_version gem_dir

    eval "$(ruby - <<EOF
puts "ruby_engine=#{defined?(RUBY_ENGINE) ? RUBY_ENGINE : 'ruby'};"
puts "ruby_version=#{RUBY_VERSION};"
EOF
)"

    # gem_dir="$HOME/.gem/sets/$ruby_engine/$ruby_version/${PWD##*/}"
    gemset_name="${1##*/}"
    gemset="$HOME/.gem/sets/$ruby_engine/$ruby_version/$gemset_name"
    mkdir -p "$gemset" && pushd -q "$1" || return 1
    [[ "$GEM_HOME" == "$gemset" ]] && return
    #echo "using $(dull_red ${PWD##*/})\n"

    export GEM_HOME="$gemset"
    export GEM_PATH="$gemset${GEM_PATH:+:}$GEM_PATH"
    export PATH="$gemset/bin${PATH:+:}$PATH"

    popd -q
}

function gem_home_pop()
{
    # trace 'gem_home_pop, removing:'
    # gem_home | head -1
    local gem_dir="${GEM_PATH%%:*}"

    PATH=":$PATH:"
    GEM_PATH=":$GEM_PATH:"

    PATH="${PATH//:$gem_dir\/bin:/:}"
    GEM_PATH="${GEM_PATH//:$gem_dir:/:}"

    PATH="${PATH#:}"; PATH="${PATH%:}"
    GEM_PATH="${GEM_PATH#:}"; GEM_PATH="${GEM_PATH%:}"

    GEM_HOME="${GEM_PATH%%:*}"
}

function gem_home()
{
    local ruby_engine ruby_version ruby_api_version gem_dir
    local version="0.1.0"

    case "$1" in
        -V|--version) echo "gem_home: $version" ;;
        -h|--help)
            cat <<USAGE
usage: gem_home [OPTIONS] [DIR|-]
Options:
  -V, --version Prints the version
  -h, --help  Prints this message
Arguments:
  DIR Sets DIR as the new \$GEM_HOME
  - Reverts to the previous \$GEM_HOME
Examples:
  $ gem_home path/to/project
  $ gem_home -
  $ gem_home --version
USAGE
            ;;
        "")
            [[ -z "$GEM_PATH" ]] && return

            local gem_path="$GEM_PATH:"

            until [[ -z "$gem_path" ]]; do
                gem_dir="${gem_path%%:*}"

                if [[ "$gem_dir" == "$GEM_HOME" ]]; then
                    echo " * $gem_dir"
                else
                    echo "   $gem_dir"
                fi

                gem_path="${gem_path#*:}"
            done
            ;;
        -)  new_trace "gem_home: pop"; gem_home_pop ;;
        *)  new_trace "gem_home: push with $1"; gem_home_push "$1" ;;
    esac
}

function show_key_value() {
    printf "%10s: %s\n" "$(dull_red $1)" $2
}

function chpwd() {
   # new_trace "chpwd"
   [[ $CHRUBY_AUTOSWITCH == true ]] && chruby_auto
}

chpwd # run chruby_auto for new shells
trace_reset

# uncommented home - works
# commented home - works
# uncommented appserver - works
# commented appserver - works
# uncommented jira - broken
# commented jira - broken

export TZ='America/Chicago'
