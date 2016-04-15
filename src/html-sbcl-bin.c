/* -*- tab-width : 2 -*- */
#include "opt.h"

LVal filter_sbcl_uri(LVal v) {
  char* str=subseq(firsts(v),-3,0);
  if(strcmp(str,"bz2")==0 ||
     strcmp(str,"msi")==0) {
    char* u=uname();
    char* m=uname_m();
    char *third,*fourth;
    char *m2;
    int i;
    char* tmp=file_namestring(q(firsts(v)));
    LVal ret= split_string(tmp,"-");
    s(tmp);
    third=firsts(nthcdr(2,ret));
    fourth=firsts(nthcdr(3,ret));
    if(strcmp(third,"x86")==0 &&
       strcmp(fourth,"64")==0) {
      m2=q("x86-64");
      i=4;
    }else {
      m2=q(third);
      i=3;
    }
    i=(strcmp(m2,m)==0 && strcmp(firsts(nthcdr(i,ret)),u)==0);

    s(m2),s(str),s(m),s(u),sL(ret);
    return i?toNumber(1):0;
  }
  s(str);
  return 0;
}

char* sbcl_bin(char* file) {
  char* str;
  LVal ret3,ret2,ret;
  cond_printf(1,"uname=%s uname-m=%s\n",uname(),uname_m());
  ret=atag_list(file);
  ret2=remove_if_not1(filter_sbcl_uri,ret);
  if(ret2==(LVal)NULL) {
    fprintf(stderr,"this architecture is not supported.stop\n");
    exit(1);
  }
  if(verbose&2)
    print_list(ret2);
  ret3= split_string(firsts(ret2),"-");
  str=q(firsts(nthcdr(1,ret3)));
  sL(ret),sL(ret2),sL(ret3);
  return str;
}

/*gcc html.c -DROSWELL_HTML_TEST util.c util_list.c util_dir.c util_string.c util_system.c*/
#ifdef ROSWELL_HTML_TEST
char* lispdir(void) {
  return NULL;
}
char** argv_orig;
int verbose=0;
int main(int argc,char** argv) {
  verbose=1;
  printf("version is %s\n",sbcl_bin(argv[1]));
}
#endif
