#!/bin/bash

# currently just a stub.
# would it be possible to generate the completion from the roswell scripts by themselves?

# https://www.debian-administration.org/article/317/An_introduction_to_bash_completion_part_2

__contains_word () {
    local w word=$1; shift
    for w in "$@"; do
        [[ $w = "$word" ]] && return
    done
}

_ros() 
{
    local cur prev opts i lisp
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    declare -a opts=(
        [long]="--help --wrap --image --lisp --load --source-registry --load-system
                --package --system-package --eval --require --quit --restart --entry --init
                --print --write --final --rc --no-rc --quicklisp --no-quicklisp --verbose
                --quiet --test"
        [short]="-w -m -L -l -S -p -sp -e -q -r -E -i -ip -iw -F -R +R -Q +Q -v"
        [take_two]="-w --wrap -m --image -L --lisp -l --load -S --source-registry -s --system
                   --load-system -p --package -sp --system-package -e --eval --require -r --restart
                   -E --entry -i --init -ip --print --write -iw -F --final"
    )
    subcommands="install config setup version help run wait use list init emacs dump delete build"
    subcommands_caserule=$(echo $subcommands | sed 's/ /|/g')

    for ((i=1; i < COMP_CWORD; i++)); do
        if __contains_word "${COMP_WORDS[i]}" ${opts[take_two]}
        then
            case "${COMP_WORDS[i]}" in
                -L|--lisp)
                    lisp="${COMP_WORDS[i+1]}"
                    ;;
            esac
            (( i++ ))
        elif __contains_word "${COMP_WORDS[i]}" ${opts[*]}
        then
            :
        elif ((i = COMP_CWORD - 1));
        then
            # espects these commands output completion candidates to the stdout
            # see src/CONTRIBUTING.md
            case $prev in
                # initially thought emacs has some default shell complete, but it turned out it doesnt
                # therefore the default completion suffices
                # emacs)
                #     # in /usr/share/bash-completion/bash_completion
                #     _command_offset $((COMP_CWORD-1))
                #     ;;
                setup|version|init|run|emacs|wait)
                    # these commands provide only the default completion
                    # it takes no arguments and it does some real jobs
                    # or there are no meaningful subsubcommands
                    return 124
                    # default completion
                    ;;
                build)
                    # complete filenames
                    COMPREPLY=( $(compgen -f -G '*.ros' -- ${cur} ) $(compgen -d -S / -- ${cur}) )
                    ;;
                config|dump|install|delete|list|installed|versions|use)
                    if [ -z $cur ]
                    then
                        COMPREPLY=( $(compgen -W "$($COMP_LINE 2> /dev/null)" -- ${cur}) )
                    else
                        partial=$(echo $COMP_LINE | cut -d' ' -f-$(($COMP_CWORD)))
                        COMPREPLY=( $(compgen -W "$($partial 2> /dev/null)" -- ${cur}) )
                    fi
                    ;;
                *)
                    return 124
                    ;;
            esac
            return 0
        else
            return 124
        fi
    done

    case $prev in
        -L|--lisp)
            COMPREPLY=( $(compgen -W "$(ros list installed 2> /dev/null)" -- ${cur}) )
            ;;
        -m|--image)
            COMPREPLY=( $(compgen -W "$(ros list dump $lisp 2> /dev/null)" -- ${cur}) )
            ;;
        -l|--load)
            COMPREPLY=( $(compgen -f -- ${cur} | grep -e '.*\.lisp' -e '.*\.l' -e '.*\.lsp' ) $(compgen -d -S / -- ${cur}) )
            ;;
        -S|--source-registry)
            # it should be multiple pathes which are separated by ':'
            ;;
        *)
            if [[ $cur == --* ]]
            then
                COMPREPLY=( $(compgen -W "${opts[*]}" -- ${cur}) )
            elif [[ $cur == [+-]* ]]
            then
                COMPREPLY=( $(compgen -W "${opts[short]}" -- ${cur}) )
            else
                COMPREPLY=( $(compgen -f -G '*.ros' -- ${cur} ) $(compgen -d -S / -- ${cur}) )
                COMPREPLY+=( $(compgen -W "${subcommands}" -- ${cur}))
            fi
            ;;
    esac
    return 0
}

complete -F _ros ros
# trap _ros DEBUG
