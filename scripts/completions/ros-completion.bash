#!/bin/bash

# currently just a stub.
# would it be possible to generate the completion from the roswell scripts by themselves?

# https://www.debian-administration.org/article/317/An_introduction_to_bash_completion_part_2

_ros() 
{
    local cur prev opts sopts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    opts="--help --wrap --image --lisp --load --source-registry --load-system
    --package --system-package --eval --require --quit --restart --entry --init
    --print --write --final --rc --no-rc --quicklisp --no-quicklisp --verbose
    --quiet --test"
    sopts="-w -m -L -l -S -p -sp -e -q -r -E -i -ip -iw -F -R +R -Q +Q -v"
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
        -m|--image)
            COMPREPLY=( $(compgen -W "$(ros list dump 2> /dev/null)" -- ${cur}) )
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
            _filedir 'ros'
            ;;
        -l|--load)
            _filedir '@(lisp|lsp|l)'
            ;;
        -S|--source-registry)
            # it should be multiple pathes which are separated by ':'
            ;;
        *)
            if [[ $cur == --* ]]
            then
                COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
            elif [[ $cur == [+-]* ]]
            then
                COMPREPLY=( $(compgen -W "${sopts}" -- ${cur}) )
            elif [[ $prev == ros ]]
            then
                _filedir 'ros'
                COMPREPLY+=( $(compgen -W "${subcommands}" -- ${cur}))
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
                    *)
                        return 124
                        ;;
                esac
            fi
            ;;
    esac
    return 0
}

complete -F _ros ros
# trap _ros DEBUG
