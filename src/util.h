#ifndef __UTIL_H__
#define __UTIL_H__
char* q(const char* orig);
void s(char* f);
char* s_cat2(char* a,char* b);
char* s_cat(char* first,...);
char* cat(char* first,...);
char* subseq(char* base,int beg,int end);
char* remove_char(char* items,char* orig);
int position_char(char* items,char* seq);
char* upcase(char* orig);
char* homedir(void);
char* pathname_directory (char* path);
char* ensure_directories_exist (char* path);
int directory_exist_p (char* path);
int change_directory(const char* path);
int delete_directory(char* pathspec,int recursive);
int delete_file(char* pathspec);
void touch(char* path);
char* system_(char* cmd);
char* uname(void);
char* uname_m(void);
char* which(char* cmd);
#endif
