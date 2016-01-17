#compdef ros

# The MIT License
# Copyright (c) 2014-2016 SANO Masatoshi
# Copyright (c) 2015-2016 Kiwamu Ishikura
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


_ros() {
    WORDS=()
    for w in $words[1,(($CURRENT - 1))]; do
        if [[ $w != -* ]]; then WORDS+=$w; fi
    done
    if (( $#WORDS == 1 )); then
        _arguments \
            '--help[Show help]' \
            {-w,--wrap}'[Run the script with a shell wrapper CODE, e.g. rlwrap]:CODE:_files' \
            {-m,--image}'[continue from Lisp image IMAGE]:IMAGE:_files' \
            {-L,--lisp}'[Run the script with a lisp impl NAME\[/VERSION\] if present, e.g. sbcl-bin, sbcl/1.2.16. Fails otherwise]' \
            {-l,--load}'[load lisp FILE while building]:FILE:_files' \
            {-S,--source-registry}'[override source registry of asdf systems]' \
            '--load-system[same as --source-registry (buildapp compatibility)]' \
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
            '--test[for test purpose]' \
            '*::command:__ros_command'
    else
        _arguments '*::command:__ros_command'
    fi
}

__ros_command() {
    local -a _ros_cmds
    _ros_cmds=(
        'install:Install a given implementation (e.g. sbcl, ccl) or a quicklisp system (e.g. alexandria) for roswell environment'
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
        'build:Make executable from script'
    )
    if (( CURRENT == 1 )) then
        _describe -t commands 'command' _ros_cmds || compadd "$@"
    else
      local curcontext="$curcontext"
      cmd="${${_ros_cmds[(r)$WORDS[2]:*]%%:*}}"
        if (( $#cmd )); then
            _call_function ret _ros_$cmd
        else
            _message "unknown ros command: $WORDS[2]"
        fi            
    fi
}

_ros "$@"