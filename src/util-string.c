/* -*- tab-width : 2 -*- */
#include "util.h"

char* qsprintf(int bufsize,char* format,...) {
  char* result=alloc(bufsize+1);
  va_list list;
  va_start(list,format);
  vsnprintf(result,bufsize+1,format,list);
  va_end(list);
  return result;
}

char* s_cat2(char* a,char* b) {
  char* ret= (char*)alloc(strlen(a)+strlen(b)+1);
  strcpy(ret,a);
  strcat(ret,b);
  dealloc(a);
  dealloc(b);
  return ret;
}

char* s_cat(char* first,...) {
  char* ret=first;
  char* i;
  va_list list;
  va_start(list,first);

  for(i=va_arg( list , char*);i!=NULL;i=va_arg( list , char*))
    ret=s_cat2(ret,i);
  va_end(list);
  return ret;
}

char* cat(char* first,...) {
  char* ret=q_(first);
  char* i;
  va_list list;
  va_start(list,first);

  for(i=va_arg( list , char*);i!=NULL;i=va_arg( list , char*))
    ret=s_cat2(ret,q_(i));
  va_end(list);
  return ret;
}

char* subseq(char* base,int beg,int end) {
  int len=-1;
  int i;
  char* ret;
  if(0>beg) {
    if(0>len)
      len=strlen(base);
    beg=len+beg;
  }
  if(0>=end) {
    if(0>len)
      len=strlen(base);
    end=len+end;
  }
  if(end<=beg)
    return NULL;
  ret=alloc(end-beg+1);
  for(i=0;i<end-beg;++i)
    ret[i]=base[i+beg];
  ret[i]='\0';
  return ret;
}

char* remove_char(char* items,char* orig) {
  int i,j,k;
  int found=0;
  char* ret;
  /* count removed*/
  for(j=0;orig[j]!='\0';++j) {
    for(i=0;items[i]!='\0';++i) {
      if(items[i]==orig[j]) {
        ++found;
        break;
      }
    }
  }
  ret=alloc(j+1-found);
  for(j=0,k=0;orig[j]!='\0';++j,++k) {
    for(i=0;items[i]!='\0';++i) {
      ret[k]=orig[j];
      if(items[i]==orig[j]) {
        --k;
        break;
      }
    }
  }
  ret[k]='\0';
  return ret;
}

int position_char(char* items,char* seq) {
  int i,j;
  for(i=0;seq[i]!='\0';++i) {
    for(j=0;items[j]!='\0';++j) {
      if(seq[i]==items[j])
        return i;
    }
  }
  return -1;
}

int position_char_not(char* items,char* seq) {
  int i,j,stop;
  for(i=0,stop=1;seq[i]!='\0';++i,stop=1) {
    for(j=0;items[j]!='\0';++j) {
      if(seq[i]==items[j]){
        stop=0;
        break;
      }
    }
    if(stop)
      return i;
  }
  return -1;
}

char* substitute_char(char new,char old,char* seq) {
  int i;
  for(i=0;seq[i]!='\0';++i) {
    if(seq[i]==old)
      seq[i]=new;
  }
  return seq;
}

char* upcase(char* orig) {
  int i;
  for(i=0;orig[i]!='\0';++i) {
    if('a'<=orig[i] && orig[i]<='z')
      orig[i]=orig[i]-'a'+'A';
  }
  return orig;
}

char* downcase(char* orig) {
  int i;
  for(i=0;orig[i]!='\0';++i) {
    if('A'<=orig[i] && orig[i]<='Z')
      orig[i]=orig[i]-'A'+'a';
  }
  return orig;
}

char* append_trail_slash(char* str) {
  return str[strlen(str)-1]!=SLASH[0]?s_cat2(str,q(SLASH)):str;
}

char* escape_string(char* str) {
  //character code might bi problem.
  char* ret;
  int i,j;
  for(i=0,j=0;str[i]!='\0';++i,++j) {
    if(str[i]=='\\' ||
       str[i]=='"') {
      ++j;
    }
  }
  ret=alloc(1+j);
  for(i=0,j=0;str[i]!='\0';++i,++j) {
    if(str[i]=='\\' ||
       str[i]=='"') {
      ret[j]='\\';
      ++j;
    }
    ret[j]=str[i];
  }
  ret[j]='\0';
  return ret;
}

char* s_escape_string(char* str) {
  char* r=escape_string(str);
  s(str);return r;
}

LVal split_string(char* string,char* by) {
  LVal ret;
  int pos,j,i;
  for(i=0,pos=-1,ret=0;string[i]!='\0';i++) {
    for(j=0;by[j]!='\0';++j) {
      if(string[i]==by[j]) {
        ret=conss(subseq(string,pos+1,i),V(ret));
        pos=i;
        break;
      }
    }
  }
  return nreverse((i!=pos+1)?conss(subseq(string,pos+1,i),V(ret)):conss(q(""),V(ret)));
}
