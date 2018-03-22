/* -*- tab-width : 2 -*- */
#ifndef __UTIL_H__
#define __UTIL_H__
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#ifndef PACKAGE
#define PACKAGE "roswell"
#endif
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>
#ifndef HAVE_WINDOWS_H
#include <pwd.h>
#include <unistd.h>
#include <signal.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/wait.h>
#define SLASH "/"
#define PATHSEP ":"
#define EXE_EXTENTION ""
#define ROSRC "/etc/rosrc"
#else
#include <windows.h>
#include <shellapi.h>
#include <shlobj.h>
#include <wininet.h>
#include <direct.h>
typedef BOOL (WINAPI *LPFN_ISWOW64PROCESS) (HANDLE, PBOOL);
#define SLASH "\\"
#define PATHSEP ";"
#define EXE_EXTENTION ".exe"
#define ROSRC ""
int setenv(const char* name,const char* value,int overwrite);
int unsetenv(const char* name);
#endif
#ifdef HAVE_CURL_CURL_H
#include <curl/curl.h>
#endif

extern char** argv_orig;
extern int argc_orig;
extern int verbose;
extern int testing;

int download_simple (char* uri,char* path,int opt);

typedef intptr_t LVal;
struct Cons {
  LVal val;
  int type;
  LVal next;
};

#ifdef __APPLE__
#ifndef st_mtime
#define st_mtime st_mtimespec.tv_sec
#endif
#endif

typedef LVal (*Function1)(LVal v);
typedef LVal (*Function2)(LVal v1,LVal v2);
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
#define s(v) (s_internal(v,#v,__FILE__,__LINE__))
#define q(v) (q_internal(v,__FILE__,__LINE__))

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
LVal nrest(LVal v);
LVal nthcdr(int n,LVal v);
LVal nnthcdr(int n,LVal v);
int length(LVal l);

void print_list(LVal v);
LVal split_string(char* string,char* by);
void sL(LVal l);
LVal stringlist(char* first,...);
char** stringlist_array(LVal v);
LVal array_stringlist(int argc,char** argv);

void cond_printf (int v,char* format,...);
void* alloc(size_t bytes);
void dealloc(void* f);
char* q_(const char* orig);
char* q_internal(const char* orig,char* file,int line);
char* qsprintf(int bufsize,char* format,...);
void s_internal(char* f,char* name,char* file,int line);
char* s_cat2(char* a,char* b);
char* s_cat(char* first,...);
char* cat(char* first,...);
char* subseq(char* base,int beg,int end);
char* remove_char(char* items,char* orig);
int position_char(char* items,char* seq);
int position_char_not(char* items,char* seq);
char* substitute_char(char new,char old,char* seq);
char* upcase(char* orig);
char* downcase(char* orig);
char* append_trail_slash(char* str);
char* escape_string(char* str);
char* s_escape_string(char* str);
char* lispdir(void);
char* patchdir(void);
/*util_dir.c */
char* basedir(void);
char* configdir(void);
char* subcmddir(void);
char* truename(const char* path);
char* pathname_directory (char* path);
char* file_namestring (char* path);
int ensure_directories_exist (char* path);
int directory_exist_p (char* path);
int change_directory(const char* path);
int delete_directory(char* pathspec,int recursive);
char* impldir(char* arch,char* os,char* impl,char* version);
/*util_file.c */
int delete_file(char* pathspec);
int rename_file(char* file,char* new_name);
void touch(char* path);
int file_exist_p (char* path);
long file_mtime(char* path);
int file_newer_p(char * a,char* b);
/*util_system.c */
char* system_(char* cmd);
int system_redirect(const char* cmd,char* filename);
int System(const char* command);
void exec_arg(char** arg);

char* backslash_decode(char* str);
char** parse_cmdline(char* cmdline,int *argc);
int free_cmdline(char** argv);
char* determin_impl(char* impl);
char* uname_s(void);
char* uname_m(void);
char* which(char* cmd);
LVal directory(char* path);
void delete_at_exit(char* file_to_delete);
void setup_uid(int euid_or_uid);
int mklockdir(char* path);
int lock_apply(char* symbol,int remove);

char* sbcl_bin(char* file,int nth);
char* ccl_binname(char* bit);
#endif
