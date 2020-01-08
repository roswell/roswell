#include "opt.h"

LVal read_tsvline(FILE* fp) {
  char buf[2000];
  if(fgets(buf,2000,fp)==NULL)
    return 0;
  return split_string(buf,"\t\n");
}

LVal tsv_eq(LVal v1,LVal v2) {
  return strcmp((char*)v1,toString(v2))==0;
}

char* sbcl_bin(char* filename,int nth) {
  FILE* fp;
  char buf[2000];
  int os=-1,arch=-1,version=-1,variant=-1,uri=-1;
  char* uname=uname_s();
  char* unamem=uname_m();
  cond_printf(1,"uname=%s uname-m=%s\n",uname,unamem);
  cond_printf(1,"open %s\n",filename);
  if((fp=fp=fopen(filename,"r"))==NULL)
    return NULL;
  LVal l=read_tsvline(fp);
  int l_count=0;
  os=position((LVal)"os",l,tsv_eq);
  arch=position((LVal)"arch",l,tsv_eq);
  version=position((LVal)"version",l,tsv_eq);
  variant=position((LVal)"variant",l,tsv_eq);
  uri=position((LVal)"uri",l,tsv_eq);
  cond_printf(1,"header os=%d,arch=%d,version=%d,variant=%d,uri=%d\n",
              os,arch,version,variant,uri);
  sL(l);
  while(l=read_tsvline(fp)) {
    ++l_count;
    cond_printf(1,"%d os:%s ",l_count,firsts(nthcdr(os,l)));
    cond_printf(1,"arch:%s ",firsts(nthcdr(arch,l)));
    cond_printf(1,"variant:%s ",firsts(nthcdr(variant,l)));
    cond_printf(1,"version:%s\n",firsts(nthcdr(version,l)));
    if(strcmp(unamem,firsts(nthcdr(arch,l)))==0 &&
       strcmp(uname,firsts(nthcdr(os,l)))==0) {
      if(--nth==0) {
        char* ret=q(firsts(nthcdr(version,l)));
        sL(l);
        fclose(fp);
        return ret;
      }
    }
    sL(l);
  }
  fclose(fp);
  return NULL;
}

#ifdef ROSWELL_TSV_TEST
char* lispdir(void) {
  return NULL;
}
char** argv_orig;
int verbose=1;
int main(int argc,char** argv) {
  printf("version is %s\n",sbcl_bin(argv[1],1));
}
#endif
