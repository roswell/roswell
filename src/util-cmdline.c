/* -*- tab-width : 2 -*- */
#include "util.h"

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
  for(p=argv;*p!=NULL;++p)
    dealloc(*p);
  dealloc(argv);
  return 1;
}
