#include "opt.h"

#ifndef HAVE_WINDOWS_H
char* homedir(void) {
  char* user=getenv("SUDO_USER");
  struct passwd *pwd= getpwuid(getuid());
  if(user && getuid()==0)
    pwd=getpwnam(user);
  return pwd?q_(pwd->pw_dir):NULL;
}

char* truename(const char* path) {
  char* buf=alloc(PATH_MAX);
  char* ret=realpath(path,buf);
  if(ret)
    return buf;
  s(buf);
  return which((char*)path);
}

int ensure_directories_exist(char* path) {
  int len = strlen(path);
  cond_printf(1,"ensure_directories_exist:%s\n",path);
  if(len) {
    for(--len;(path[len]!=DIRSEP[0]||len==-1);--len);
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
  cond_printf(1,"change_directory:%s\n",path);
  return chdir(path);
}

int delete_directory(char* pathspec,int recursive) {
  char* cmd=s_cat2(q(recursive?"rm -rf ":"rmdir "),q(pathspec));
  int ret=System(cmd);
  s(cmd);
  return ret==0;
}

char* currentdir(void) {
  char buf[2048];
  return append_trail_slash(q_(getcwd(buf,2048)));
}

int is_valid_path(const char *path) {
  return path[0] == '/';
}

#else

char* homedir(void);
char* currentdir(void);
int is_valid_path(const char *);
#endif

char* configdir(void) {
  char *c=upcase(q_(PACKAGE"_HOME")); /* e.g. ROSWELL_HOME */
  char *env=getenv(c);

  if (env) /* note: env can be a NULL */
  {
      if (!is_valid_path(env))
      {
          cond_printf(0,"Error: %s must be absolute. Got: %s \n",c,env);
	  abort();
      }
      s(c);                     /* note : this frees c. */
      return append_trail_slash(q(env));
  }
  {
      s(c);                     /* note : this frees c. */
      env = homedir();          /* use homedir instead of ROSWELL_DIR */
      if (env)                  /* env is not null */
      {
          return s_cat2(append_trail_slash(env),q("."PACKAGE DIRSEP));
      }
      {
          return NULL;
      }
  }
}

char* subcmddir(void) {
  return lispdir();
}

char* pathname_directory(char* path) {
  int i;
  char* ret;
  for(i=strlen(path)-1;i>=0&&path[i]!=DIRSEP[0];--i);
  ret=(i>=0)?subseq(path,0,i+1):append_trail_slash(".");
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

char* impldir(char* arch,char* os,char* impl,char* version) {
  return cat("impls",DIRSEP,arch,DIRSEP,os,DIRSEP,impl,DIRSEP,version,NULL);
}

char* basedir(void) {
  char* cd_;
  cond_printf(1,"roswellenv=%s\n",get_opt(PACKAGE_NAME"env",1));
  if(get_opt(PACKAGE_NAME"env",1)) {
    cd_ = cat(configdir(),"env",DIRSEP,get_opt(PACKAGE_NAME"env",1),DIRSEP,NULL);
    if(directory_exist_p(cd_))
      return cd_;
    s(cd_);
  }
  cd_ = s_cat2(currentdir(),q("."PACKAGE DIRSEP));
  if(directory_exist_p(cd_))
    return cd_;
  s(cd_);
  return configdir();
}

