#include "util.h"

int extract(const char *filename, int do_extract, int flags,const char* outputpath) {
  char* str;
#ifndef HAVE_WINDOWS_H
  int len=strlen(filename);
  char* type="gzip"; /*for gz*/
  if(len>4) {
    int i;
    for(i=len-4;filename[i]!='\0';++i)
      if(filename[i]=='b'||filename[i]=='B')
        type="bzip2"; /*bz*/
  }

  str=cat(type," -dc ",filename," | tar -",extract?"x":"t",
          flags?"p":"","f - -C ",outputpath,NULL);
#else
  char* _uname_m=uname_m();
  char* _uname=uname();
  char* _homedir=configdir();
  char* exe=s_escape_string(cat(_homedir,"impls",SLASH,_uname_m,SLASH,_uname,SLASH,"7za",SLASH,"9.20",SLASH,"7za.exe",NULL));
  char *outputpath2=q(outputpath);
  substitute_char('\\','/',outputpath2);
  outputpath2=s_escape_string(outputpath2);
  ensure_directories_exist(outputpath2);
  str=cat(exe," ",extract?"x ":"l ",filename," -so |",exe," x -ttar -si -y -o",outputpath2,NULL);
  s(outputpath2),s(_homedir),s(_uname),s(_uname_m);
#endif
  if(verbose>0)
    fprintf(stderr,"extractcmd=%s\n",str);
  int ret=system(str);
  s(str);
  return ret;
}

int cmd_tar(int argc, const char **argv) {
  const char *filename = NULL;
  const char *outputpath = NULL;
  int compress, flags=0, mode, opt;

  mode = 'x';
  compress = '\0';
  /* Among other sins, getopt(3) pulls in printf(3). */
  while (*++argv != NULL && **argv == '-') {
    const char *p = *argv + 1;
    while ((opt = *p++) != '\0') {
      switch (opt) {
      case 'f':
        if (*p != '\0')
          filename = p;
        else
          filename = *++argv;
        p += strlen(p);
        break;
      case 'C':
        if (*p != '\0')
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
        verbose++;
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

