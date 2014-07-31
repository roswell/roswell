#include <stdio.h>
#include <string.h>

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "opt.h"
#include "util.h"
char** argv_orig;
int argc_orig;
struct opts* global_opt;
struct opts* local_opt=NULL;

extern int cmd_install(int argc,char **argv);
extern int cmd_download(int argc,char **argv);
extern int cmd_tar(int argc,char **argv);
extern int cmd_version(int argc,char **argv);
extern int cmd_opt(int argc,char **argv);
extern int cmd_run(int argc,char **argv);

int cmd_notyet(int argc,char **argv)
{
  printf("notyet\n");
}

static struct sub_command commands[] = {
  { "help", cmd_notyet},
  { "version", cmd_version},
  { "install", cmd_install},
  { "run",cmd_run},
  { "config", cmd_opt},
  { "set", cmd_notyet},
  { "tar",cmd_tar},
  { "download",cmd_download},
};

int main (int argc,char **argv) {
  char **subargv=NULL;
  char *subcmd=NULL;
  int i;
  int found=0;
  struct sub_command* j;
  argv_orig=argv;
  argc_orig=argc;
  char* path=s_cat(homedir(),q("config"),NULL);
  global_opt=load_opts(path);
  s(path);
  if(argc==1) {
    subcmd="help";
    subargv=NULL;
    argc=0;
  } else {
    argc--;
    argv++;
    
    while(argc>0 && argv[0][0]=='-') {
      struct opts* opt=local_opt;
      struct opts** opts=&opt;
      if(argv[0][1]!='-' && argv[0][1]!='\0' && argc>1 && argv[1][0]!='-') {
	set_opt(opts, &argv[0][1],(char*)argv[1],0);
	argc--;
	argv++;
      }else{
	int index=1;
	if(argv[0][1]=='-') ++index;
	set_opt(opts, &argv[0][index],(char*)"1",0);
      }
      local_opt=*opts;
      argc--;
      argv++;
    }
  }
  if(argc>0) {
    subcmd=argv[0];
    subargv=&argv[1];
    argc--;
    for(i=0;i<sizeof(commands)/sizeof(struct sub_command);i++) {
      j = &commands[i];
      if(strcmp(subcmd,j->name)==0) {
	found=1;
	j->call(argc,subargv);
	break;
      }
    }
    if(!found) {
      printf("action %s not found\n",subcmd);
    }
  }
  free_opts(global_opt);
}

/* ./lsp tar -xvf ~/.lsp/archives/sbcl-1.2.1.tar.bz2 */
