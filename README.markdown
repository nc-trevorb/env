### about

This script is basically a fork of [chruby](https://github.com/postmodern/chruby) and
[gem_home](https://github.com/postmodern/gem_home).  It is supposed to be a replacement for RVM, so
it does two things: 1) switch ruby version, and 2) manage gemsets.

It is slightly different from the real chruby/gem_home: 1) slightly more verbose output for
autoswitching, and 2) it installs gems to a different location (`gem_home` typically installs them
to `$PROJECT_ROOT/.gem`, but this script puts them in
`~/.gem/sets/$ruby_engine/$ruby_version/$project_name`).

### why

[RVM has to be used from a login shell](http://stackoverflow.com/questions/9336596/rvm-installation-not-working-rvm-is-not-a-function).
I was having trouble getting it to work from a ruby script.

### install

This currently only works for zsh.  The original works for both bash and zsh
(https://github.com/postmodern/chruby/blob/master/share/chruby/auto.sh#L31), so it shouldn't be too
much work to make it compatible again (I changed it to only autoswitch when the pwd changes instead
of every command - I'm guessing bash some way to do that also).

### usage

    [~] chruby
       jruby-1.7.20
       ruby-1.9.3-p448
       ruby-2.2.4

    [~] cd code/appserver
    $ chruby ruby-1.9.3-p448
    using ruby-1.9.3-p448
    
    $ gem_home appserver
    using appserver
    
    [master][~/code/appserver] chruby
       jruby-1.7.20
     * ruby-1.9.3-p448
       ruby-2.2.4

    [master][~/code/appserver] chruby 2.2
    using ruby-2.2.4
    
    [master][~/code/appserver] chruby
       jruby-1.7.20
       ruby-1.9.3-p448
     * ruby-2.2.4

