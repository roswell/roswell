/* -*- tab-width : 2 -*- */
#include "util.h"

#ifndef HAVE_WINDOWS_H
char* homedir_helper(void) {
  char* user=getenv("SUDO_USER");
  struct passwd *pwd= getpwuid(getuid());
  if(user)
    pwd=getpwnam(user);
  return pwd?q_(pwd->pw_dir):NULL;
}

char* truename(const char* path) {
  char* ret=realpath(path,NULL);
  return ret?ret:which((char*)path);
}

int ensure_directories_exist (char* path) {
  int len = strlen(path);
  cond_printf(1,"ensure_directories_exist:%s\n",path);
  if(len) {
    for(--len;(path[len]!=SLASH[0]||len==-1);--len);
    path=subseq(path,0,len+1);
  }else
    path=q(path);
  
  if(!directory_exist_p(path)) {
    pid_t  pid=fork();
    if(pid==-1) {
      perror("fork");
      return 0;
    }
    if(pid==0) {
      char* cmd=cat("mkdir -p ",path,NULL);
      setup_uid(0);
      if(System(cmd)!=0) {
        fprintf(stderr,"failed:%s\n",cmd);
        exit(1);
      }
      s(cmd);
      exit(0);
    }else {
      int status;
      s(path);
      waitpid(pid,&status,0);
      return WEXITSTATUS(status);
    }
    s(path);
  }
  return 1;
}

int directory_exist_p (char* path) {
  struct stat sb;
  return stat(path, &sb) == 0 && S_ISDIR(sb.st_mode)?1:0;
}

int change_directory(const char* path) {
  return chdir(path);
}

int delete_directory(char* pathspec,int recursive) {
  char* cmd=s_cat2(q(recursive?"rm -rf ":"rmdir "),q(pathspec));
  int ret=System(cmd);
  s(cmd);
  return ret==0;
}

#endif

char* homedir(void) {
  char *c=q_(PACKAGE"_HOME");
  char *env=getenv(c);
  s(c);
  return env?append_trail_slash(q(env)):
    ((c=homedir_helper())?append_trail_slash(c):NULL);
}

char* configdir(void) {
  char* home=homedir();
  return home?s_cat2(home,q("."PACKAGE SLASH)):NULL;
}

char* subcmddir(void) {
  return lispdir();
}


char* pathname_directory(char* path) {
  int i;
  char* ret;
  for(i=strlen(path)-1;i>=0&&path[i]!=SLASH[0];--i);
  ret=append_trail_slash(subseq(path,0,i));
  s(path);
  return ret;
}

char* file_namestring(char* path) {
  int i;
  char* ret;
  for(i=strlen(path)-1;i>=0&&path[i]!='/';--i);
  ret=(path[i]=='/')?subseq(path,i+1,0):q(path);
  s(path);
  return ret;
}
