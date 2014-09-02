#ifndef __UTIL_H__
#define __UTIL_H__
#include <stdint.h>
#ifndef _WIN32
#include <dirent.h>
#endif
extern char** argv_orig;
extern int argc_orig;

typedef intptr_t LVal;
struct Cons {
  LVal val;
  int type;
  LVal next;
};

typedef LVal (*Function1)(LVal v);
typedef LVal (*Compare2)(LVal v1,LVal v2);

#define toPointer(v) ((void*)(((intptr_t)v)&~3))
#define NumberP(v) (((v)&3)==1)
#define toNumber(v) (((v)<<2)+1)
#define Number(v) (((v)>>2))
#define StringP(v) (((v)&3)==2)
#define toString(v) ((char*)(((intptr_t)v)&~3))
#define ListP(v) (((v)&3)==3)
#define toList(v) ((struct Cons*)(((intptr_t)v)&~3))
#define V(v) ((LVal)(v))
#define Next(v) ((LVal)(((struct Cons*)toPointer(v))->next))
LVal consi(int  v,LVal l);
LVal conss(char* v,LVal l);
LVal cons(void* v,LVal l);
LVal nreverse(LVal l);
LVal remove_if_not1(Function1 f,LVal v);
LVal mapcar1(Function1 f,LVal v);

LVal string_equal(LVal v1,LVal v2);
LVal find(LVal v,LVal l,Compare2 c);

int firsti(LVal l);
char* firsts(LVal l);
void* firstp(LVal l);
LVal first(LVal v);
LVal rest(LVal v);
LVal nthcdr(int n,LVal v);
LVal length(LVal l);

void print_list(LVal v);
LVal split_string(char* string,char* by);
void sL(LVal l);

void* alloc(size_t bytes);
void dealloc(void* f);
char* q(const char* orig);
void s(char* f);
char* s_cat2(char* a,char* b);
char* s_cat(char* first,...);
char* cat(char* first,...);
char* subseq(char* base,int beg,int end);
char* remove_char(char* items,char* orig);
int position_char(char* items,char* seq);
int position_char_not(char* items,char* seq);
char* upcase(char* orig);
char* homedir(void);
char* pathname_directory (char* path);
char* filname_namestring (char* path);
char* ensure_directories_exist (char* path);
int directory_exist_p (char* path);
int change_directory(const char* path);
int delete_directory(char* pathspec,int recursive);
int delete_file(char* pathspec);
void touch(char* path);
char* system_(char* cmd);
int system_redirect(const char* cmd,char* filename);
char* uname(void);
char* uname_m(void);
char* which(char* cmd);
LVal directory(char* path);

#endif
