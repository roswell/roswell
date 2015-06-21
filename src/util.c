#include "util.h"

void cond_printf(int v,char* format,...) {
  if((v&verbose) == v) {
    va_list list;
    va_start(list,format);
    vfprintf(stderr,format,list);
    va_end(list);
  }
}

void* alloc(size_t bytes) {
  void* p=malloc(bytes);
  //  printf("**%d\n",p);
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
  cond_printf(2,"%s %d q(%s) %lu \n",file,line,orig,(intptr_t)orig);
  char* ret= (char*)alloc(strlen(orig)+1);
  strcpy(ret,orig);
  return ret;
}

void s_internal(char* f,char* name,char* file,int line) {
  cond_printf(2,"%s %d s(%s) %lu \n",file,line,name,(intptr_t)f);
  dealloc(f);
}

#ifdef _WIN32
setenv(const char* name,const char* value,int overwrite) {
  char* s_=cat((char*)name,"=",(char*)value,NULL);
  _putenv(s_);
  s(s_);
}
unsetenv(const char* name) {
  char* s_=cat((char*)name,"=",NULL);
  _putenv(s_);
  s(s_);
}
#endif

int delete_file(char* pathspec) {
#ifndef HAVE_WINDOWS_H
  char* cmd;
  int ret;
  cmd=s_cat2(q("rm -f "),q(pathspec));
  ret=System(cmd);
  s(cmd);
  return ret==0;
#else
//  #error not implemented delete_file
#endif  
}

int rename_file(char* file,char* new_name) {
#ifndef HAVE_WINDOWS_H
  char* cmd;
  int ret;
  cmd=s_cat(q("mv "),q(file),q(" "),q(new_name),NULL);
  ret=System(cmd);
  s(cmd);
  return ret==0;
#else
  return MoveFileEx(file,new_name,MOVEFILE_REPLACE_EXISTING);
#endif
}

void touch(char* path) {
  int ret;
  cond_printf(1,"%s\n",path);
#ifndef HAVE_WINDOWS_H
  char* cmd=s_cat2(q("touch "),q(path));
  ret=System(cmd);
  s(cmd);
#else
#endif
}

char* s_decode(char* str) {
  int count,i,write,escape=0;
  char* ret;
  for(write=0;write<2;++write) {
    for(i=0,count=0;str[i]!='\0';++i,++count) {
      if(!escape && str[i]=='\\' && str[i+1]!='\0')
        escape=1,--count;
      else {
        if(write) {
          ret[count]=str[i];
          if(escape) {
            switch(str[i]) {
            case 'n':
              ret[count]='\n';
              break;
            case 'r':
              ret[count]='\r';
            }
          }
        }
        escape=0;
      }
    }
    if(!write) {
      ret=alloc(sizeof(char)*(count+1));
      ret[count]='\0';
    }
  }
  s(str);
  return ret;
}

char** parse_cmdline(char* cmdline,int *argc) {
  int i,write,mode=0;
  int count,last;
  char** ret;
  for(write=0;write<2;++write) {
    last=0,mode=0,count=0;
    for(i=0;cmdline[i]!='\0';++i) {
      if(cmdline[i]==' '||cmdline[i]=='\t') {
        if(i!=0) {
          if(write) {
            ret[count]=subseq(cmdline,last,i-mode);
            if(mode) {
              ret[count]=s_decode(ret[count]);
              mode=0;
            }
          }
          ++count;
        }
        for(;cmdline[i]==' '||cmdline[i]=='\t'||cmdline[i]=='\0';++i);
        last=i--;
      }else if(cmdline[i]=='"') {
        last=++i;
        mode=1;
        for(;cmdline[i]!='"'&&cmdline[i]!='\0';++i)
          if(cmdline[i]=='\\'&&cmdline[i+1]!='\0')
            ++i;
      }
    }
    if(last+1!=i) {
      if(write) {
        ret[count]=subseq(cmdline,last,i-mode);
        if(mode) {
          ret[count]=s_decode(ret[count]);
          mode=0;
        }
      }
      ++count;
    }
    if(!write)
      ret=alloc(sizeof(char**)*(count+1));
  }
  ret[count]=NULL;
  *argc=count;
  return ret;
}

int free_cmdline(char** argv) {
  char** p;
  for(p=argv;*p!=NULL;++p) {
    dealloc(*p);
  }
  dealloc(argv);
  return 1;
}

char* uname(void) {
#ifdef __CYGWIN__
  return q("windows");
#endif
#ifndef HAVE_WINDOWS_H
  char *p=system_("uname");
  char *p2;
  p2=remove_char("\r\n",p);
  s(p);
  return downcase(p2);
#else
  return q("windows");
#endif
}

char* uname_m(void) {
#if defined(_WIN64)
  return q("x86-64");
#endif
#ifdef __CYGWIN__
  return q("x86");
#endif
#ifndef HAVE_WINDOWS_H
  char *p=system_("uname -m");
  char *p2;
  p2=remove_char("\r\n",p);
  s(p);
  if(strcmp(p2,"i686")==0) {
    s(p2);
    return q("x86");
  }
  if(strcmp(p2,"amd64")==0) {
    s(p2);
    return q("x86-64");
  }
  if(strcmp(p2,"armv6l")==0 ||
     strcmp(p2,"rmv7l")==0) {
    char* result=system_("readelf -A /proc/self/exe |grep Tag_ABI_VFP_args|wc -l");
    char* result2=remove_char("\r\n",result);
    s(result);
    if(strcmp(result2,"0")!=0) {
      s(result2);
      return q("armel");
    }else {
      s(result2);
      return q("armhf");
    }
  }
  return substitute_char('-','_',p2);
#else
#if _WIN64
  return q("x86-64");
#elif _WIN32
  BOOL isWow64 = FALSE;
  LPFN_ISWOW64PROCESS fnIsWow64Process  = (LPFN_ISWOW64PROCESS)
    GetProcAddress(GetModuleHandle(TEXT("kernel32")),"IsWow64Process");
  if(fnIsWow64Process) {
    if (!fnIsWow64Process(GetCurrentProcess(), &isWow64))
      return q("x86");
    if(isWow64)
      return q("x86-64");
    else
      return q("x86");
  }
  else
    return q("x86");
#endif
#endif
}

char* which(char* cmd) {
#ifndef HAVE_WINDOWS_H
  char* which_cmd=cat("command -v \"",cmd,"\"",NULL);
#else
  if((cmd[0]=='.' && cmd[1]=='/')|| /* relative path */
     position_char("/:",cmd)!=-1) { /* have no path element */
    cmd=substitute_char('\\','/',q(cmd));
    return truename(cmd);
  }
  char* which_cmd=cat("cmd /c where ",cmd,"",NULL);
#endif
  char* p=system_(which_cmd);
  cond_printf(1,"system_ result:%s\n",p);
  p=substitute_char('\0','\r',substitute_char('\0','\n',p));
  char* p2=p?remove_char("\r\n",p):q("");
  s(p),s(which_cmd);
  return p2;
}

LVal directory(char* path) {
  LVal ret=0;
#ifndef HAVE_WINDOWS_H
  DIR* dir=opendir(path);
  struct dirent *dirent;

  if(dir==NULL)
    return 0;
  while((dirent=readdir(dir))!=0) {
    char* str=q(dirent->d_name);
    if(dirent->d_type&DT_DIR)
      str=s_cat2(str,q("/"));
    ret=conss(str,ret);
  }
  closedir(dir);
#else
  WIN32_FIND_DATA fd;
  char *p=cat(path,"*.*",NULL);
  HANDLE dir=FindFirstFile(p,&fd);
  if(dir==INVALID_HANDLE_VALUE)
    return 0;
  do {
    if(!(strcmp(fd.cFileName,".")==0 ||
         strcmp(fd.cFileName,"..")==0)) {
      char* str=q(fd.cFileName);
      if(fd.dwFileAttributes&FILE_ATTRIBUTE_DIRECTORY)
        str=s_cat2(str,q(SLASH));
      ret=conss(str,ret);
    }
  }while(FindNextFile(dir,&fd)!=0);
  s(p);
  FindClose(dir);
#endif
  return ret;
}

void signal_callback_handler(int signum) {
  printf("Caught signal %d\n",signum);
  exit(1);
}

char* atexit_delete=NULL;

void atexit_handler(void) {
  delete_file(atexit_delete);
  s(atexit_delete);
}

void setup_signal_handler (char* file_to_delete) {
#ifndef HAVE_WINDOWS_H
  atexit_delete=q(file_to_delete);
  signal(SIGHUP,  signal_callback_handler);
  signal(SIGINT,  signal_callback_handler);
  signal(SIGPIPE, signal_callback_handler);
  signal(SIGQUIT, signal_callback_handler);
  signal(SIGTERM, signal_callback_handler);
  atexit(atexit_handler);
#endif
}
