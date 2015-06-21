#include "opt.h"

struct tag {
  int type; /* 0 for not tag
	       1 for open tag
	       2 for close tag*/
  char* name;
  struct Cons* attr;
};

struct Cons* attralloc(void) {
  struct opts* ret=alloc(sizeof(struct opts));
  ret->name=NULL;
  ret->value=NULL;
  return toPointer(cons(ret,(LVal)NULL));
}

void attrsfree(struct Cons* a) {
  struct Cons* next;
  for(;a;) {
    struct opts* p=(struct opts*)firstp((LVal)a);
    s((char*)p->name),s((char*)p->value);
    next=(struct Cons*)a->next;
    dealloc(a);
    a=next;
  }
}

LVal parse_attr(char* str) {
  int i,j;
  char *name,*val;
  struct Cons tmp,*ret=&tmp,*cur=ret;
  struct opts* p;
  tmp.next=(LVal)NULL;
  for(i=0;str[i]!='\0';++i) {
    name=NULL,val=NULL;
    /*skip space*/
    int pos=position_char_not(" \t\r\n",&str[i]);
    if(pos!=-1) {
      i+=pos;
    }else continue;
    /*attr name */
    pos=position_char("\"'>/= \t\r\n",&str[i]);
    if(pos!=-1)
      name=subseq(&str[i],0,pos),i+=pos;
    /*skip space*/
    pos=position_char_not(" \t\r\n",&str[i]);
    if(pos!=-1)
      i+=pos;
    if(!(name && str[i]=='='))
      continue;
    cur->next=(LVal)attralloc();;
    p=(struct opts*)firstp((LVal)cur->next);
    p->name=name;
    /*skip space*/
    pos=position_char_not(" \t\r\n",&str[++i]);
    if(pos!=-1) {
      i+=pos;
      switch(str[i]) {
      case '\'':
        j=position_char("'",&str[i+1]);
        if(j!=-1) {
          val=subseq(&str[i+1],0,j);
          i+=j+1;
        }
        break;
      case '"':
        for(j=1;str[i+j]!='"';++j)
          if(str[i+j]=='\\')
            ++j;
        val=subseq(&str[i],1,j);
        i+=j;
        break;
      default:
        pos=position_char(" \t\r\n",&str[i]);
        val=subseq(&str[i],0,pos);
        break;
      }
      p->value=val;
    }
    cur=(struct Cons *)cur->next;
  }
  return (LVal)ret->next;
}

LVal tagalloc(void) {
  struct tag* t=alloc(sizeof(struct tag));
  t->type=0;
  t->name=NULL;
  t->attr=NULL;
  return (LVal)toPointer(cons(t,(LVal)NULL));
}

void tagfree(LVal l) {
  struct tag* t=firstp(l);
  s(t->name);
  attrsfree(t->attr);
  dealloc(t);
  dealloc((void*)l);
}

void tagsfree(LVal tags) {
  struct Cons* t=(struct Cons*)tags;
  struct Cons* next;
  for(;t;t=next) {
    next=(struct Cons*)t->next;
    tagfree((LVal)t);
  }
}

LVal delete_not_open_tags(LVal t) {
  LVal tag=t;
  LVal ret=tag;
  LVal next;
  for(;((struct Cons*)tag)->next!=(LVal)NULL&&((struct tag*)firstp(tag))->type!=1;tag=next) {  /* find first*/
    next=Next(tag);
    tagfree(tag);
  }
  while(Next(tag)) {
    if(((struct tag*)firstp(Next(tag)))->type!=1) {
      next=Next(Next(tag));
      tagfree(Next(tag));
      ((struct Cons*)tag)->next=next;
    }else
      tag=Next(tag);
  }
  return ret;
}

LVal delete_not_tags(char* tags,LVal tag) {
  LVal ret;
  LVal next;
  for(;Next((LVal)tag)!=(LVal)NULL&&((struct tag*)firstp(tag))->name&&strcmp(((struct tag*)firstp(tag))->name,tags)!=0;tag=Next(tag));  /* find first*/
  ret=tag;
  while(Next(tag)) {
    if(((struct tag*)firstp(Next((LVal)tag)))->name&&
       strcmp(((struct tag*)firstp(Next((LVal)tag)))->name,tags)!=0) {
      next=Next(Next(tag));
      tagfree(Next(tag));
      ((struct Cons*)tag)->next=next;
    }else
      tag=Next(tag);
  }
  return (LVal)ret;
}

LVal filter_href(LVal t) {
  LVal tags=t;
  LVal ret=(LVal)NULL;
  for(;tags!=(LVal)NULL;tags=Next(tags)) {
    char* href=NULL;
    LVal a=(LVal)((struct tag*)firstp(tags))->attr;
    for(;a;a=Next(a)) {
      struct opts* v=(struct opts*)firstp(a);
      if(strcmp(v->name,"href")==0) {
	href=q(v->value);
	break;
      }
    }
    if(href)
      ret=(LVal)toPointer(conss(href,ret));
  }
  return ret;
}

LVal parse_tags(FILE* fp,LVal before,int mode) {
  LVal current=tagalloc();
  char str[2]={'\0','\0'};
  int c,i=0;
  char* buf=q("");
  switch(mode) {
  case 0: /* wait for '<' */
    ((struct tag*)firstp(current))->type=0;
    while((c=fgetc(fp))!=EOF) {
      if(c=='<') {
        if(strlen(buf)==0) {
          tagfree(current);
          s(buf);
          return parse_tags(fp,before,1);
        }else {
          ((struct Cons*)current)->next=parse_tags(fp,current,1);
          s(buf);
          return current;
        }
      }else
        str[0]=c,buf=s_cat2(buf,q(str));
    }
    break;
  case 1: /* wait for '>' */
    ((struct tag*)firstp(current))->type=0;
    while((c=fgetc(fp))!=EOF) {
      if(i==0) {
        if(c=='/')
          ((struct tag*)firstp(current))->type=2;
        else {
          ((struct tag*)firstp(current))->type=1;
          str[0]=c;
          buf=s_cat2(buf,q(str));
        }
        ++i;
        continue;
      }
      if(c=='>') {
        char *buf2;
        if(((struct tag*)firstp(current))->type==2) {
          int pos=position_char(" \t\r\n",buf);
          if(pos!=-1) {
            buf2=subseq(buf,0,pos);
            s(buf);
            buf=buf2;
            ((struct tag*)firstp(current))->name=q(buf);
          }else {
            ((struct tag*)firstp(current))->name=buf;
            buf=q("");
          }
        }else if(((struct tag*)firstp(current))->type==1) {
          int pos=position_char(" \t\r\n",buf);
          if(pos!=-1) {
            ((struct tag*)firstp(current))->name=subseq(buf,0,pos);
            buf2=subseq(buf,pos,0);
            ((struct tag*)firstp(current))->attr=(struct Cons*)parse_attr(buf2);
            s(buf);
            buf=buf2;
          }else {
            ((struct tag*)firstp(current))->name=buf;
            buf=q("");
          }
        }
        if(strcmp(((struct tag*)firstp(current))->name,"script")==0) {
          ((struct Cons*)current)->next=parse_tags(fp,current,2);
        }else{
          ((struct Cons*)current)->next=parse_tags(fp,current,0);
        }
        s(buf);
        return current;
      }else {
        str[0]=c;
        buf=s_cat2(buf,q(str));
      }
      ++i;
    }
    break;
  case 2: /* wait for '</' */
    ((struct tag*)firstp(current))->type=0;
    while((c=fgetc(fp))!=EOF) {
      if(c=='<') {
        if((c=fgetc(fp))!=EOF && c=='/') {
          ungetc('/',fp);
          if(strlen(buf)==0) {
            tagfree(current);
            s(buf);
            return parse_tags(fp,current,1);
          }else {
            ((struct Cons*)current)->next=parse_tags(fp,current,1);
            s(buf);
            return current;
          }
        }
        ungetc('/',fp);
      }else {
        str[0]=c;
        buf=s_cat2(buf,q(str));
      }
    }
    break;
  }
  s(buf);
  return current;
}

LVal atag_list(char* filename) {
  FILE* fp;
  LVal ret=(LVal)NULL;
  cond_printf(1,"open %s\n",filename);
  fp=fopen(filename,"r");
  if(fp!=NULL) {
    LVal tags=parse_tags(fp,(LVal)NULL,0);
    tags=delete_not_tags("a",delete_not_open_tags(tags));
    ret=nreverse(filter_href(tags));
    tagsfree(tags);
    fclose (fp);
  }
  return ret;
}

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

/*gcc html.c -DROSWELL_HTML_TEST util.c util_list.c util_dir.c util_string.c*/
#ifdef ROSWELL_HTML_TEST
char** argv_orig;
int verbose=0;
int main(int argc,char** argv) {
  verbose=1;
  sbcl_bin(argv[1]);
}
#endif
