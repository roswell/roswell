/* -*- tab-width : 2 -*- */
#include "util.h"

LVal cons(void* v,LVal l) {
  struct Cons* ret=alloc(sizeof(struct Cons));
  ret->val=(LVal)v;
  ret->type=0;
  ret->next=l;
  return (LVal)toList(ret);
}

LVal consi(int v,LVal l) {
  return cons((void*)((LVal)toNumber(v)),l);
}

LVal conss(char* v,LVal l) {
  return cons((void*)((LVal)toString(v)|2),l);
}

LVal nreverse(LVal v) {
  LVal next;
  LVal before;
  for(before=0;v;v=next) {
    next=Next(v);
    ((struct Cons*)v)->next=before;
    before=v;
  }
  return (LVal)before;
}

LVal remove_if_not1(Function1 f,LVal v) {
  LVal ret;
  for(ret=0;v;v=Next(v)) {
    LVal fret=f(v);
    if(fret) {
      if(NumberP(first(v))) {
        ret=consi(firsti(v),ret);
      }else if(StringP(first(v))) {
        ret=conss(q(firsts(v)),ret);
      }
    }
    sL(fret);
  }
  return nreverse(ret);
}

LVal mapcar1(Function1 f,LVal v) {
  LVal ret;
  for(ret=0;v;v=Next(v)) {
    ret=cons((void*)f(first(v)),ret);
  }
  return nreverse(ret);
}

LVal find(LVal v,LVal l,Compare2 c) {
  for(;l;l=Next(l)) {
    if(c(v,first(l)))
      return first(l);
  }
  return 0;
}

int firsti(LVal v) {
  struct Cons* l=(struct Cons*)v;
  return (l->val>>2);
}

char* firsts(LVal v) {
  struct Cons* l=(struct Cons*)v;
  return (char*)(l->val&(~3));
}

void* firstp(LVal v) {
  struct Cons* l=(struct Cons*)v;
  return (void*)(l->val&(~3));
}

LVal first(LVal v) {
  struct Cons* l=(struct Cons*)v;
  return l->val;
}

LVal rest(LVal v) {
  struct Cons* l=(struct Cons*)v;
  return l->next;
 }

LVal nthcdr(int n,LVal v) {
  for(;n>0;--n) {
    v=rest(v);
  }
  return v;
}

int length(LVal l) {
  int c;
  for(c=0;l;++c,l=Next(l));
  return c;
}

void print_list(LVal v) {
  printf("(");
  for(;v;v=Next(v)) {
    switch(first(v)&3) {
    case 1:
      printf("%d",firsti(v));
      break;
    case 2:
      printf("\"%s\"",firsts(v));
      break;
    case 0:
      print_list(first(v));
      break;
    }
    if(Next(v))
      printf(" ");
  }
  printf(")\n");
}

LVal string_equal(LVal v1,LVal v2) {
  return strcmp(toString(v1),toString(v2))==0;
}

void sL(LVal v) {
  struct Cons* next;
  struct Cons* l;
  switch(v&3) {
  case 1: //number
    break;
  case 2: //string pointer
    s(toString(v));
    break;
  case 0: //builtin structure
    for(l=toList(v);l;l=next) {
      next=(struct Cons*)Next((LVal)l);
      sL(l->val);
      dealloc(l);
    }
    break;
  }
}

char** stringlist_array(LVal v) {
  LVal vi=v;
  int l=length(v);
  int c;
  char** arg=alloc(sizeof(char*)*(l+1));
  arg[l]=NULL;
  for(c=0;c<l;++c,v=Next(v))
    arg[c]=q(firsts(v));
  sL(vi);
  return arg;
}
