#include "util.h"

#ifndef HAVE_WINDOWS_H

char* uname_s(void) {
  char *p,*p2;
  p2=remove_char("\r\n",p=system_("uname"));
  s(p);
  return downcase(p2);
}

char* uname_m(void) {
  char *p=system_("uname -m");
  char *p2;
  p2=remove_char("\r\n",p);
  s(p);
  if(strcmp(p2,"i686")==0) {
    s(p2);
    return q("x86");
  }
  if(strcmp(p2,"i386")==0) {
    s(p2);
    return q("x86");
  }
  if(strcmp(p2,"amd64")==0) {
    s(p2);
    return q("x86-64");
  }
  if(strcmp(p2,"aarch64")==0) {
    s(p2);
    return q("arm64");
  }
  if(strcmp(p2,"armv6l")==0 ||
     strcmp(p2,"armv7l")==0) {
    char* result=system_("readelf -A /proc/self/exe |grep Tag_ABI_VFP_args|wc -l");
    char* result2=remove_char("\r\n",result);
    s(result);
    if(strcmp(result2,"0")!=0) {
      s(result2);
      return q("armhf");
    }else {
      s(result2);
      return q("armel");
    }
  }
  if(strcmp(p2,"armv5tejl")==0) {
    s(p2);
    return q("armel");
  }
  return substitute_char('-','_',p2);
}

char* which(char* cmd) {
  char* which_cmd=cat("command -v \"",cmd,"\"",NULL);
  cond_printf(1,"which cmd:%s\n",which_cmd);
  char* p=system_(which_cmd);
  cond_printf(1,"which result:%s\n",p);
  p=substitute_char('\0','\r',substitute_char('\0','\n',p));
  char* p2=p?remove_char("\r\n",p):q("");
  s(p),s(which_cmd);
  return p2;
}

LVal directory(char* path) {
  LVal ret=0;
  DIR* dir=opendir(path);
  struct dirent *dirent;

  if(dir==NULL)
    return 0;
  while((dirent=readdir(dir))!=0) {
    if(!(strcmp(dirent->d_name,".")==0 ||
         strcmp(dirent->d_name,"..")==0)) {
      char* str=q(dirent->d_name);
      if(dirent->d_type & DT_DIR)
        str=s_cat2(str,q("/"));
      ret=conss(str,ret);
    }
  }
  closedir(dir);
  return ret;
}

void signal_callback_handler(int signum) {
  printf("Caught signal %d\n",signum);
  exit(1);
}

LVal atexit_delete=0;
int setup_atexit=0;
void atexit_handler(void) {
  LVal n,l;
  for(l=atexit_delete;l;l=n) {
    delete_file(firsts(l));
    s(firsts(l));
    n=rest(l);
    dealloc((void*)l);
  }
}

void delete_at_exit(char* file_to_delete) {
  atexit_delete=conss(q(file_to_delete),atexit_delete);
  if(!setup_atexit) {
    signal(SIGHUP,  signal_callback_handler);
    signal(SIGINT,  signal_callback_handler);
    signal(SIGPIPE, signal_callback_handler);
    signal(SIGQUIT, signal_callback_handler);
    signal(SIGTERM, signal_callback_handler);
    atexit(atexit_handler);
  }
  setup_atexit=1;
}

void setup_uid(int euid_or_uid) {
  if(getuid()==0) {
    char *uid_str=getenv("SUDO_UID"),*gid_str=getenv("SUDO_GID");
    uid_t uid=uid_str?atoi(uid_str):0;
    gid_t gid=gid_str?atoi(gid_str):0;

    if(euid_or_uid) {
      if(!(setegid(gid)==0 &&
           seteuid(uid)==0))
        cond_printf(0,"Error setegid/seteuid \n");
    }else {
      setgroups(0, NULL);
      if(!(setgid(gid)==0 &&
           setuid(uid)==0))
        cond_printf(0,"Error setgid/setuid \n");
    }
  }
}

int mklockdir(char* path) {
  return mkdir(path,0700);
}

#endif

int lock_apply(char* symbol,int remove) {
  char *p=s_cat(configdir(),q("tmp"),q(SLASH),NULL);
  int ret=0;
  ensure_directories_exist(p),s(p);
  p=s_cat(configdir(),q("tmp"SLASH"lock."PACKAGE"."),q(symbol),NULL);
  if(remove<2) {
    cond_printf(1,"%slock!:%s\n",remove?"un":"",symbol);
    while(remove?rmdir(p):mklockdir(p));
  }else{ /* prove lockfile*/
    ret=directory_exist_p(p);
    cond_printf(1,"lock %s exist status=%d",symbol,ret);
  }
  s(p);
  return ret;
}

void cond_printf(int v,char* format,...) {
  if((v&verbose) == v) {
    va_list list;
    va_start(list,format);
    vfprintf(stderr,format,list);
    va_end(list);
    fflush(stderr);
  }
}

void* alloc(size_t bytes) {
  void* p=malloc(bytes);
  /* printf("**%d\n",p); */
  return p;
}

void dealloc(void* f) {
  free(f);
}

char* q_(const char* orig) {
  char* ret= (char*)alloc(strlen(orig)+1);
  strcpy(ret,orig);
  return ret;
}

char* q_internal(const char* orig,char* file,int line) {
  char* ret= (char*)alloc(strlen(orig)+1);
  cond_printf(2,"q %lu %lu %s %d \"%s\"\n",(intptr_t)ret,(intptr_t)orig,file,line,orig);
  strcpy(ret,orig);
  return ret;
}

void s_internal(char* f,char* name,char* file,int line) {
  cond_printf(2,"s %lu %s %d \"%s\":",(intptr_t)f,file,line,f);
  dealloc(f);
  cond_printf(2,"done\n");
}
