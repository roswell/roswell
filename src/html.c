#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "util.h"

struct attr {
  char* name;
  char* value;
};

struct Cons* attralloc(void) {
  struct attr* ret=malloc(sizeof(struct attr));
  ret->name=NULL;
  ret->value=NULL;
  return toPointer(cons(ret,(LVal)NULL));
}

void print_attr(struct Cons* i) {
  for(;i;i=i->next) {
    struct attr* p=(struct attr*)firstp((LVal)i);
    printf("<%s=%s>",p->name,p->value);
  }
  printf("\n");
}

void attrsfree(struct Cons* a) {
  struct Cons* next;
  for(;a;) {
    struct attr* p=(struct attr*)firstp((LVal)a);
    s(p->name),s(p->value);
    next=a->next;
    free(a);
    a=next;
  }
}

struct Cons * parse_attr(char* str) {
  int i;
  int pos;
  char* name;
  char* val;
  struct Cons tmp;
  struct Cons *ret=&tmp,*cur=ret;
  tmp.next=NULL;
  for(i=0;str[i]!='\0';++i) {
    name=NULL;
    val=NULL;
    /*skip space*/
    pos=position_char_not(" \t\r\n",&str[i]);
    if(pos!=-1){
      i+=pos;
    }else{
      continue;
    }
    /*attr name */
    pos=position_char("\"'>/= \t\r\n",&str[i]);
    if(pos!=-1){
      name=subseq(&str[i],0,pos);
      i+=pos;
    }
    /*skip space*/
    pos=position_char_not(" \t\r\n",&str[i]);
    if(pos!=-1){
      i+=pos;
    }
    if(name) {
      struct attr* p;
      if(str[i]=='=') {
	int j=0;
	/*skip space*/
	pos=position_char_not(" \t\r\n",&str[++i]);
	if(pos!=-1){
	  i+=pos;
	}
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
	    for(j=1;str[i+j]!='"';++j) {
	      if(str[i+j]=='\\')
		++j;
	    }
	    val=subseq(&str[i],1,j);
	    i+=j;
	    break;
	  default:
	    pos=position_char(" \t\r\n",&str[i]);
	    val=subseq(&str[i],0,pos);
	    break;
	  }
	  cur->next=attralloc();;
	  p=(struct attr*)firstp((LVal)cur->next);
	  p->name=name;
	  p->value=val;
	  cur=cur->next;
	}else {
	  cur->next=attralloc();
	  p=(struct attr*)firstp((LVal)cur->next);
	  p->name=name;
	  cur=cur->next;
	}
      }
    }
  }
  return ret->next;
}

struct tag {
  int type; /* 0 for not tag 
	       1 for open tag 
	       2 for close tag*/ 
  char* name;
  struct Cons* attr;
};

struct Cons* tagalloc(void) {
  struct tag* t=malloc(sizeof(struct tag));
  t->type=0;
  t->name=NULL;
  t->attr=NULL;
  return toPointer(cons(t,(LVal)NULL));
}

void tagfree(LVal l) {
  struct tag* t=firstp(l);
  s(t->name);
  attrsfree(t->attr);
  free(t);
  free((void*)l);
}

void tagsfree(struct Cons* t) {
  struct Cons* next;
  for(;t;t=next) {
    next=t->next;
    tagfree((LVal)t);
  }
}

struct Cons* delete_not_open_tags(struct Cons* tag) {
  struct Cons* ret=tag;
  struct Cons* next;
  for(;tag->next!=NULL&&((struct tag*)firstp(tag))->type!=1;tag=next) {  /* find first*/
    next=tag->next;
    tagfree(tag);
  }
  while(tag->next) {
    if(((struct tag*)firstp(tag->next))->type!=1) {
      next=Next(Next(tag));//tag->next->next;
      tagfree(tag->next);
      tag->next=next;
    }else{
      tag=tag->next;
    }
  }
  return ret;
}

struct Cons* delete_not_tags(char* tags,struct Cons* tag) {
  struct Cons* ret;
  struct Cons* next;
  for(;Next((LVal)tag)/*->next*/!=NULL&&((struct tag*)firstp(tag))->name&&strcmp(((struct tag*)firstp(tag))->name,tags)!=0;tag=Next((LVal)tag)/*->next*/);  /* find first*/
  ret=tag;
  while(tag->next) {
    if(((struct tag*)firstp(Next((LVal)tag)/*->next*/))->name&&
       strcmp(((struct tag*)firstp(Next((LVal)tag)/*->next*/))->name,tags)!=0) {
      next=Next(Next(tag));//->next->next;
      tagfree(tag->next);
      tag->next=next;
    }else {
      tag=tag->next;
    }
  }
  return ret;
}

struct Cons* filter_href(struct Cons* tags)
{
  struct Cons* ret=NULL;
  char* href;
  for(;tags;tags=tags->next) {
    href=NULL;
    struct Cons* a=((struct tag*)firstp(tags))->attr;
    for(;a;a=a->next) {
      struct attr* v=(struct attr*)firstp(a);
      if(strcmp(v->name,"href")==0) {
	href=q(v->value);
	break;
      }
    }
    if(href) {
      ret=toPointer(conss(href,(LVal)ret));
    }
  }
  return ret;
}

void print_tags(struct Cons* tags) {
  for(;tags;tags=tags->next){
    if(((struct tag*)firstp(tags))->name)
      printf("%s",((struct tag*)firstp(tags))->name);
    print_attr(((struct tag*)firstp(tags))->attr);
  }
}

struct Cons* parse_tags(FILE* fp,struct Cons* before,int mode) {
  struct Cons* current=tagalloc();
  char str[2]={'\0','\0'};
  int c;
  char* buf=q("");
  if(mode==0) { /* wait for '<' */
    ((struct tag*)firstp(current))->type=0;
    while((c=fgetc(fp))!=EOF) {
      if(c=='<') {
	if(strlen(buf)==0) {
	  tagfree(current);
	  s(buf);
	  return parse_tags(fp,before,1);
	}else {
	  current->next=parse_tags(fp,current,1);
	  s(buf);
	  return current;
	}
      }else {
	str[0]=c;
	buf=s_cat2(buf,q(str));
      }
    }
  }else if (mode==1) { /* wait for '>' */
    int i=0;
    int token_count=0;
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
	    ((struct tag*)firstp(current))->attr=parse_attr(buf2);
	    s(buf);
	    buf=buf2;
	  }else {
	    ((struct tag*)firstp(current))->name=buf;
	    buf=q("");
	  }
	}
	if(strcmp(((struct tag*)firstp(current))->name,"script")==0) {
	  current->next=parse_tags(fp,current,2);
	}else{
	  current->next=parse_tags(fp,current,0);
	}
	s(buf);
	return current;
      }else {
	str[0]=c;
	buf=s_cat2(buf,q(str));
      }
      ++i;
    }
  }else if (mode==2) { /* wait for '</' */
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
	    current->next=parse_tags(fp,current,1);
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
  }
  s(buf);
  return current;
}

struct Cons* atag_list(char* filename)
{
  FILE* fp;
  struct Cons* ret=NULL;
  fp=fopen(filename,"r");
  if(fp!=NULL) {
    struct Cons* tags=parse_tags(fp,NULL,0);
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
    LVal tmp=file_namestring(q(firsts(v)));
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
    i=(strcmp(m2,m)==0&&strcmp(firsts(nthcdr(i,ret)),u)==0);
    
    s(m2),s(str),s(m),s(u),s(ret);
    if(i)
      return toNumber(1);
    else
      return 0;
  }
  s(str);
  return 0;
}

/*
int main(int argc,char** argv) 
{
  struct Cons *ret,*ret2;
  ret=atag_list(argv[1]);
  ret2=remove_if_not1(filter_sbcl_uri,ret);
  //ret3=mapcar1(separate_sbcl_uri,ret2);
  
  print_list(ret2);
  sL(ret),sL(ret2); 

}
*/
char* sbcl_bin(char* file)
{
  struct Cons *ret,*ret2;
  ret=atag_list(file);
  ret2=remove_if_not1(filter_sbcl_uri,ret);
  //ret3=mapcar1(separate_sbcl_uri,ret2);
  
  print_list(ret2);
  sL(ret),sL(ret2); 
}
