#!/bin/bash

# currently just a stub.
# would it be possible to generate the completion from the roswell scripts by themselves?

# https://www.debian-administration.org/article/317/An_introduction_to_bash_completion_part_2

_ros() 
{
    local cur prev opts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    opts="--help -w --wrap -m --image -L --lisp -l --load -S
    --source-registry --load-system -p --package -sp --system-package -e
    --eval --require -q --quit -r --restart -E --entry -i --init -ip
    --print -iw --write -F --final -R --rc +R --no-rc -Q --quicklisp +Q
    --no-quicklisp -v --verbose --quiet --test"

    subcommands="install config setup version help run wait use list init emacs dump delete build"
    subcommands_caserule=$(echo $subcommands | sed 's/ /|/g')

    case $prev in
        # initially thought emacs has some default shell complete, but it turned out it doesnt
        # therefore the default completion suffices
        # emacs)
        #     # in /usr/share/bash-completion/bash_completion
        #     _command_offset $((COMP_CWORD-1))
        #     ;;
        -L|--lisp)
            COMPREPLY=( $(compgen -W "$(ros list installed 2> /dev/null)" -- ${cur}) )
            ;;
        setup|version|init|run|emacs|wait)
            # these commands provide only the default completion
            # it takes no arguments and it does some real jobs
            # or there are no meaningful subsubcommands
            return 124
            # default completion
            ;;
        build)
            # complete filenames
            COMPREPLY=( $(compgen -A file -o filenames -- ${cur}) ) ;;
        *)
            if [[ $cur == [+-]* ]]
            then
                COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
            elif [[ $prev == ros ]]
            then
                COMPREPLY=( $(compgen -W "${subcommands}" -- ${cur}) )
            else
                # espects these commands output completion candidates to the stdout
                # see src/CONTRIBUTING.md
                case $prev in
                    config|dump|install|delete|list|installed|versions|use)
                        if [ -z $cur ]
                        then
                            COMPREPLY=( $(compgen -W "$($COMP_LINE 2> /dev/null)" -- ${cur}) )
                        else
                            partial=$(echo $COMP_LINE | cut -d' ' -f-$(($COMP_CWORD)))
                            COMPREPLY=( $(compgen -W "$($partial 2> /dev/null)" -- ${cur}) )
                        fi
                        ;;
                esac
            fi
            ;;
    esac
    return 0
}

complete -F _ros ros
# trap _ros DEBUG
