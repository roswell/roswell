




# default behavior for command line output

if some command requires a subcommand and the subcommand is missing, output should be printed as follows:

* auxilary information should be printed to the stderr. These are read by human.
* list of candidates for the missing subcommand should be printed to the stdout. These are not only read by human, but also read by shell completion facility.

