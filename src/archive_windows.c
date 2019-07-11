#include "opt.h"

#ifdef HAVE_WINDOWS_H
char* extract_command_str(int flags,const char *filename,int do_extract,const char* outputpath,char* type) {
  char* str;
  char* _uname_m=uname_m();
  char* _uname=uname_s();
  char* _confdir=configdir();
  char* exe=s_escape_string(cat(_confdir,"impls",SLASH,_uname_m,SLASH,_uname,SLASH,"7za",SLASH,"9.20",SLASH,"7za.exe",NULL));
  char *outputpath2=q(outputpath);
  char *filename2=q(filename);
  substitute_char('\\','/',outputpath2);
  outputpath2=s_escape_string(outputpath2);
  filename2=s_escape_string(filename2);
  ensure_directories_exist(outputpath2);
  if(strcmp(type,"gzip")==0 || strcmp(type,"bzip2")==0 || strcmp(type,"xz")==0) {
    str=cat("cmd /c \"",exe," ",do_extract?"x ":"l ",filename2," -so |",exe," x -ttar -si -y -o",outputpath2,"\"",NULL);
  }else if(strcmp(type,"7za")==0) {
    ensure_directories_exist(outputpath2);
    str=cat(exe," ",do_extract?"x":"t"," -y -o",outputpath2," ",filename2,NULL);
  }
  s(outputpath2),s(filename2),s(_confdir),s(_uname),s(_uname_m);
  return str;
}
#endif
