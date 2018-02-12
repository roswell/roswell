ros-template - Edit template for ros-init

# synopsis

* **ros template** command args...

# Subcommands

init template-name

  : Create new template in local-project directory.

deinit template-name

  : Remove a template from local-project directory.

list

  : Show file list and states of the files *type* *chmod* *rewrite* in templates.

checkout [template-name]

  :  When invoked without template-name,this command shows candidates for current templates to operate.If with template-name, this command choose the name as current template. 

add [template-name] [files ...]

  : Add files to current template.First parameter could taken as a template-name when current template are *default*.

cat [template-name] [files ...]

  : Show contents in the current template.

edit [template-name] [files ...]

  : Edit content of files in template using `/usr/bin/editor`.

rm [template-name] [files ...]

  : Remove file form current template.

type [template-name] [type] [files ...]

  : Change file type. current choice for *type*s are *djula* or *copy* {{name}} {{author}} {{email}} {{universal_time}} are available variable in the template file. Withoug files, change default type.

chmod [template-name] [mode] [files ...] 

  : Change file mode bits to generate. specify mode in octal format.

rewrite [template-name] file rewrite-rule

  : Change file name using djula template.Variables {{name}} {{author}} {{email}} {{universal_time}} are available for rewrite-rule.

export [template-name] [directory]

  : Export files in current template to current directory.

import [directory]

  : Import template in current directory

help

  : Show the subcommand help.

<!-- somecommand -->
 
<!--   : description. end with a period. -->

# Description

The _ros-template_(1) command manages templates of projects.

## Create template

First, the `init` sub-command creates an empty template.

    $ ros template init sample-template

Many of sub-commands take a template name as first argument. But you can omit it by the `checkout` sub-commands. After the following, such sub-commands are applied to `sample-template`.

    $ ros template checkout sample-template

Next, the `add` sub-command adds local file[s] to template. Then, the `list` sub-command shows information of files in template.

    $ echo "Hello {{ author }}!!" > sample.txt
    $ ros template add sample.txt
    $ ros template list
          copy  sample.txt 

The word *copy* means a strategy when applying the template. There are the following 2 strategies.

- *copy* simply copies the file as-is.
- *djula* processes the content of the file by template engine, [Djula](http://mmontone.github.io/djula/).
    - Available variables are explained later.

The `type` sub-command changes it. In addition, default strategy (*copy*) can be changed for each template by the `type` sub-command without file names (Ex. `ros template type djula`).

    $ ros template type djula sample.txt
    $ ros template list
          djula sample.txt 

The `sample.txt` will be simply output as `sample.txt` in default. But you can change it by the `rewrite` sub-command. In the following example, it will be output as `sample_<project name>.txt`.

    $ ros template rewrite sample.txt "sample-{{ name }}.txt"
    $ ros template list
          djula sample.txt -> "sample-{{ name }}.txt"

Note: Rewrite rules are always processed by Djula irrespective of strategy of the file.

## Apply template

_ros-init_(1) can specify a template.

    $ mkdir temp ; cd temp
    $ ros init sample-template some-project
    $ ls
    sample-some-project.txt
    $ cat sample-some-project.txt # Assume that "author" is "alien"
    Hello alien!!

The file name and its content are processed by Djula as explained in the above. The following variables can be used in defaut.

- {{name}}: A project name specified in _ros-init_(1)
- {{author}}: An author name extracted from config of Git or `whoami`
- {{email}}: An e-mail address extracted from config of Git or created using `whoami` and `hostname`
- {{universal_time}}: A universal time created by `get-universal-time` function

In addition, you can use original variables as the followings.

    $ echo "Hello {{ area }}!!" > sample.txt
    $ ros template add sample.txt # Note: It overwrites existing
    $ mkdir temp ; cd temp
    $ ros init sample-template roswell --area 51
    $ cat sample-roswell.txt
    Hello 51!!

## Export and import template

Basically, _ros-template_(1) is designed to internally manage added files.

However, if you want to, for example, manage it using Git in a local directory or to install distributed templates, you can use the `export` and `import` sub-commands.

First, the `export` sub-command exports added files and a setting file `roswell.init.<template name>.asd` that is internally created and editted.

    $ ros template export dir
    $ ls dir
    roswell.init.sample-template.asd  sample.txt
    $ cat dir/sample.txt
    Hello {{ area }}!!

Second, the `import` sub-command imports them.

    # Assume that this is another machine...
    $ ros template list sample-template # nothing is output 
    $ ls downloaded
    roswell.init.sample-template.asd  sample.txt
    $ ros template import downloaded
    $ ros template list sample-template
          djula sample.txt -> "sample-{{ name }}.txt"

Note: If there is a template whose name is same, it will be overwritten.

<!-- # options -->
<!--  -->
<!-- # Environmental Variables -->

# SEE ALSO
_ros_(1)  _ros-init_(1)
