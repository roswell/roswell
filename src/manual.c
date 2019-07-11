#include "opt.h"

#ifndef HAVE_WINDOWS_H

DEF_SUBCMD(cmd_man) {
  cond_printf(1,"cmd_man:%d\n",length(arg_));
  char* man= which("man");
  if(strlen(man))
    exec_arg(stringlist_array(conss(man,rest(arg_))));
  return 1;
}

#endif
