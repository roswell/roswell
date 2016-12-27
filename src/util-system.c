/* -*- tab-width : 2 -*- */
#include "util.h"

#ifndef HAVE_WINDOWS_H

char* system_(char* cmd) {
  FILE *fp;
  char buf[256];
  char* s=q("");
  if((fp=popen(cmd,"r")) ==NULL) {
    printf("Error:%s\n",cmd);
    exit(EXIT_FAILURE);
  }
  while(fgets(buf,256,fp) !=NULL) {
    s=s_cat2(s,q(buf));
  }
  (void)pclose(fp);
  return s;
}

int system_redirect(const char* cmd,char* filename) {
  pid_t pid;
  int fd[2];
  if(pipe(fd)==-1) {
    perror("pipe");
    return -1;
  }
  pid=fork();
  if(pid==-1) {
    perror("fork");
    return -1;
  }
  if(pid==0) {
    int argc;
    char** argv=parse_cmdline((char*)cmd,&argc);
    /* standard output */
    close(fd[0]);
    close(1),close(2);
    dup2(fd[1],1),dup2(fd[1],2);
    close(fd[1]);
    execvp(argv[0],argv);
  }else {
    FILE *in,*out;
    close(fd[1]);
    if((out=fopen(filename,"a"))!=NULL) {
      if((in=fdopen(fd[0], "r"))!=NULL) {
        int c;
        while((c = fgetc(in)) != EOF) {
          if(fputc(c, out) == EOF) {
            fclose(in);
            fclose(out);
            return 0;
          }
        }
        fclose(in);
      }
      fclose(out);
    }
  }
  return(0);
}

int System(const char* command) {
  return system(command);
}

void exec_arg(char** arg) {
  execvp(arg[0],&(arg[0]));
}
#endif
