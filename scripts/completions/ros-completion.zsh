#compdef ros

# The MIT License
# Copyright (c) 2015-2016 Kiwamu Ishikura
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

# The recommended way to install this script is to copy to '~/.zsh/_ros', and
# then add the following to your ~/.zshrc file:
#
#  fpath=(~/.zsh $fpath)

_contains_word () {
    local w word=$1; shift
    for w in $@; do
        if [[ $w = $word ]]
        then
            return 0
        fi
    done
    return 1
}

_notify-send () {
    #notify-send $1 $2
}

_ros() {
    local -a opts_take_two opts_short opts_long i options arguments _cmds lisp

    _cmds=(
        'install:Install a given implementation or a system for roswell environment'
        'config:Get and set options'
        'setup:Initial setup'
        'version:Show the roswell version information'
        'help:Show Command help'
        'run:Run repl'
        'wait:Wait forever'
        'use:change default implementation'
        'list:List Information'
        'init:Create new ros script'
        'emacs:launch emacs with slime'
        'dump:Dump image for faster startup or Make Executable'
        'delete:Delete installed implementations'
        'asdf:manage asdf version'
        'build:Make executable from script'
    )
    opts_take_two=(-w --wrap -m --image -L --lisp -l --load -S --source-registry -s --system
                   --load-system -p --package -sp --system-package -e --eval --require -r --restart
                   -E --entry -i --init -ip --print --write -iw -F --final)
    opts_short=(-w -m -L -l -S -p -sp -e -q -r -E -i -ip -iw -F -R +R -Q +Q -v)
    opts_long=(--help --wrap --image --lisp --load --source-registry --load-system
               --package --system-package --eval --require --quit --restart --entry --init
               --print --write --final --rc --no-rc --quicklisp --no-quicklisp --verbose
               --quiet --test)
    _notify-send zsqxqxq //$#words
    for ((i=2; i <= $#words; i++)); do
        _notify-send zs! /$words[i]/$i/$#words
        if _contains_word $words[i] $opts_take_two
        then
            case $words[i] in
                -L|--lisp)
                    lisp=$words[i+1]
                    ;;
            esac
            (( i++ ))
            _notify-send zs!! /$words[i]/$i/$#words
        elif _contains_word $words[i] $opts_short $opts_long
        then
            {}
        elif [[ $i = $CURRENT ]]
        then
            if [[ ${words[i]:0:1} = '-' ]];
            then
                _arguments \
                    '--help[Show help]' \
                    {-w,--wrap}'[Run the script with a shell wrapper CODE, e.g. rlwrap]:CODE:_files' \
                    {-m,--image}'[continue from Lisp image IMAGE]:IMAGE:_files' \
                    {-L,--lisp}'[Run the script with a lisp impl NAME\[/VERSION\]]' \
                    {-l,--load}'[load lisp FILE while building]:FILE:_files' \
                    {-S,--source-registry}'[override source registry of asdf systems]' \
                    {-s,--system}'[load asdf SYSTEM while building]' \
                    '--load-system[same as --system (buildapp compatibility)]' \
                    {-p,--package}'[change current package to PACKAGE]' \
                    {-sp,--system-package}'[combination of -s SP and -p SP]' \
                    {-e,--eval}'[evaluate FORM while building]' \
                    '--require[require MODULE while building]' \
                    {-q,--quit}'[quit lisp here]' \
                    {-r,--restart}'[restart from build by calling (FUNC)]' \
                    {-E,--entry}'[restart from build by calling (FUNC argv)]' \
                    {-i,--init}'[evaluate FORM after restart]' \
                    {-ip,--print}'[evaluate and princ FORM after restart]' \
                    {-iw,--write}'[evaluate and write FORM after restart]' \
                    {-F,--final}'[evaluate FORM before dumping image]' \
                    {-R,--rc}'[try read /etc/rosrc, ~/.roswell/init.lisp]' \
                    {+R,--no-rc}'[skip /etc/rosrc, ~/.roswell/init.lisp]' \
                    {-Q,--quicklisp}'[use quicklisp (default)]' \
                    {+Q,--no-quicklisp}'[do not use quicklisp]' \
                    {-v,--verbose}'[be quite noisy while building]' \
                    '--quiet[be quite quiet while building (default)]' \
                    '--test[for test purpose]'
                return
            fi
        else
            _path_files
            return
        fi
    done
    case $words[$#words-1] in
        -w|--wrap)
            ;;
        -L|--lisp)
            _alternative "lisp:lisps:($(ros list installed 2> /dev/null))"
            ;;
        -l|--load)
            _files -g "*.(lisp|lsp|l)(-.)"
            ;;
        -S|--source-registry|--load-system)
            ;;
        -p|--package)
            ;;
        -sp|--system-package)
            ;;
        -e|--eval)
            ;;
        --require)
            ;;
        -r|--restart)
            ;;
        -E|--entry)
            ;;
        -i|--init)
            ;;
        -ip|--print)
            ;;
        -iw|--write)
            ;;
        -F|--final)
            ;;
        -m|--image)
            _alternative "image:images:($(ros list dump $lisp 2> /dev/null))"
            ;;
        *)
            _alternative 'args:custom args:{_describe "commands" _cmds}' \
                         'files:filenames:_files -g "*.ros(-.)"'
            ;;
    esac
}

_ros "$@"
