/* -*- tab-width : 2 -*- */
#include "opt.h"

DEF_SUBCMD(cmd_man) {
  int argc=length(arg_);
  cond_printf(1,"cmd_man:%d\n",argc);
  return (0);
}
