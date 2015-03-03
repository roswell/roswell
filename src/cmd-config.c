#include <stdio.h>
#include "util.h"
#include "opt.h"

int cmd_config(int argc, const char **argv)
{
  char* home=configdir();
  char* path=cat(home,"config",NULL);
  int ret;
  if(argc==1) {
    printf("oneshot:\n");
    print_opts(local_opt);
    printf("local:\n");
    print_opts(global_opt);
    ret= 1;
  }else {
    struct opts* opt=global_opt;
    struct opts** opts=&opt;
    // TBD parse options
    if(argc==2) {
      unset_opt(opts, argv[1]);
      save_opts(path,opt);
      ret= 2;
    }else if(argc>2) {
      if(strcmp(argv[1],"set")==0) {
        set_opt(opts, argv[2],(char*)argv[3],0);
        save_opts(path,opt);
      }else if (strcmp(argv[1],"show")==0) {
        printf("%s\n",_get_opt(opt,argv[2]));
      }else {
        set_opt(opts, argv[1],(char*)argv[2],0);
        save_opts(path,opt);
      }
      ret= 3;
    }
  }
  s(home),s(path);
  return 0;
}
