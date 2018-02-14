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

rm [template-name] [files ...]

  : Remove file form current template.

type [template-name] [type] [files ...]

  : Change file type. current choice for *type*s are *djula* or *copy* {{name}} {{author}} {{email}} {{universal_time}} are available variable in the template file.

chmod [template-name] [mode] [files ...] 

  : Change file mode bits to generate. specify mode in octal format.

rewrite [template-name] file rewrite-rule

  : Change file name using djula template.Variables {{name}} {{author}} {{email}} {{universal_time}} are available for rewrite-rule.

help

  : Show the subcommand help.

<!-- somecommand -->
 
<!--   : description. end with a period. -->

# description

<!-- # options -->
<!--  -->
<!-- # Environmental Variables -->

# SEE ALSO
_ros_(1)  _ros-init_(1)
