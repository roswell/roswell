#include "opt.h"

extern char* extract_command_str(int flags,const char *filename,int do_extract,const char* outputpath,char* type);

#ifndef HAVE_WINDOWS_H
char* extract_command_str(int flags,const char *filename,int do_extract,const char* outputpath,char* type) {
  char* str;
  char* bin=which("gtar");
  char* arc=(strcmp(bin,"")==0)?"tar":"gtar";
  s(bin);
  if(strcmp(type,"gzip")==0 || strcmp(type,"bzip2")==0 || strcmp(type,"xz")==0) {
    str=cat(type," -dc ",filename," | ",arc," -",do_extract?"x":"t",
            flags?"p":"","f - -C ",outputpath,NULL);
  }else if(strcmp(type,"7za")==0) {
    ensure_directories_exist((char*)outputpath);
    str=cat("7za ",do_extract?"x":"t"," -o",outputpath," ",filename,NULL);
  }
  return str;
}
#endif

int extract(const char *filename, int do_extract, int flags,const char* outputpath) {
  char* str=NULL;
  int len=strlen(filename),ret=-1;
  char* type="gzip"; /*for gz*/
  if(len>4) {
    int i,c;
    for(c=0,i=len;filename[i]!='.' && c<5;--i,++c) {
      if(filename[i]=='b'||filename[i]=='B') {
        type="bzip2";
        break;
      }else if(filename[i]=='x'||filename[i]=='X') {
        type="xz";
        break;
      }else if(filename[i]=='7') {
        type="7za";
        break;
      }
    }
  }
  cond_printf(1,"extracttype=%s\n",type);
  str=extract_command_str(flags,filename,do_extract,outputpath,type);
  cond_printf(1,"extractcmd=%s\n",str);
  if(str) {
    ret=System(str);
    s(str);
  }
  return ret;
}

DEF_SUBCMD(cmd_tar) {
  int argc=length(arg_);
  char** argv=stringlist_array(arg_);
  cond_printf(1,"cmd_tar:%d\n",argc);
  const char *filename = NULL;
  const char *outputpath = NULL;
  int flags=0, mode, opt;

  mode = 'x';
  /* Among other sins, getopt(3) pulls in printf(3). */
  while (*++argv != NULL && **argv == '-') {
    const char *p = *argv + 1;
    while ((opt = *p++) != '\0') {
      switch (opt) {
      case 'f':
        if(*p != '\0')
          filename = p;
        else
          filename = *++argv;
        p += strlen(p);
        break;
      case 'C':
        if(*p != '\0')
          outputpath = p;
        else
          outputpath = *++argv;
        p += strlen(p);
        break;
      case 'p':
        flags = 1;
        break;
      case 't':
        mode = opt;
        break;
      case 'v':
        verbose=1|verbose<<1;
        break;
      case 'x':
        mode = opt;
        break;
      }
    }
  }
  switch (mode) {
  case 't':
    extract(filename, 0, flags,outputpath);
    break;
  case 'x':
    extract(filename, 1, flags,outputpath);
    break;
  }
  return (0);
}
