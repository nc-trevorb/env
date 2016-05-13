
function clean_status() {
    [[ -z $(git status --porcelain) ]]
}

function pull_master() {
    if clean_status; then
        git co master
        git pull
    else
        echo 'git is dirty, skipping pull'
    fi
}

function update_ruby_app() {
    chruby env
    pull_master
    bundle
    bundle exec rake db:migrate
    RAILS_ENV=test bundle exec rake db:migrate
    git co db/schema.rb
}

function update() {
    case "$1" in
        appserver)
            update_ruby_app
            bundle exec rake market_data:use_latest
            ;;
        lac_server)
            update_ruby_app
            ;;
        vault)
            update_ruby_app
            ;;
        ui)
            pull_master

            npm install
            bower install
            ;;
        *)
            echo "don't know how to update '${1}'"
            ;;
    esac
}

function serve() {
    case "$1" in
        appserver)
            bundle exec rails server -e local_production -p 3000
            ;;
        lac_server)
            bundle exec passenger start -p 3001
            ;;
        vault)
            torquebox start -p 3002
            ;;
        ui)
            grunt serve:local
            ;;
        *)
            echo "don't know how to serve '${1}'"
            ;;
    esac
}

code_dir=/Users/trevorb/code

# <local dir name>:<github repo name>
apps=(
    appserver:rails
    lac_server:likeassets_connect_server
    vault:likeassets_connect_vault
    # ui:nextcapital-ui
)

for app in "${apps[@]}"; do
    app_name="${app%%:*}"
    origin="git@github.com:BLC/${app##*:}"
    dir="$code_dir/$app_name"

    pushd $dir >/dev/null
    echo "vvvvvvvvvvvvvvvvv $app_name vvvvvvvvvvvvvvvvv"
    $1 $app_name
    popd >/dev/null
done

echo 'done'
