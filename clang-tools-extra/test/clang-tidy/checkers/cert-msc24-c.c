// RUN: %check_clang_tidy %s cert-msc24-c %t

#define __STDC_LIB_EXT1__ 1
#define __STDC_WANT_LIB_EXT1__ 1

char *gets(char *s);

void f1(char *s) {
  (void)gets(s);
  // CHECK-MESSAGES: :[[@LINE-1]]:9: warning: function 'gets' is deprecated as of C99, removed from C11. [cert-msc24-c]

  char *(*f_ptr)(char *) = gets;
  // CHECK-MESSAGES: :[[@LINE-1]]:28: warning: function 'gets' is deprecated as of C99, removed from C11. [cert-msc24-c]
}

typedef void FILE;

FILE *fopen(const char *filename, const char *mode);
FILE *freopen(const char *filename, const char *mode, FILE *stream);
void rewind(FILE *stream);
void setbuf(FILE *stream, char *buf);
int fscanf(FILE *stream, const char *format, ...);

void f2(char *s, FILE *f) {
  (void)fopen(s, s);
  // CHECK-MESSAGES: :[[@LINE-1]]:9: warning: function 'fopen' has no exclusive access to file, 'fopen_s' should be used instead. [cert-msc24-c]

  FILE *(*f_ptr2)(const char *, const char *) = fopen;
  // CHECK-MESSAGES: :[[@LINE-1]]:49: warning: function 'fopen' has no exclusive access to file, 'fopen_s' should be used instead. [cert-msc24-c]

  (void)freopen(s, s, f);
  // CHECK-MESSAGES: :[[@LINE-1]]:9: warning: function 'freopen' has no exclusive access to file, 'freopen_s' should be used instead. [cert-msc24-c]

  FILE *(*f_ptr3)(const char *, const char *, FILE *) = freopen;
  // CHECK-MESSAGES: :[[@LINE-1]]:57: warning: function 'freopen' has no exclusive access to file, 'freopen_s' should be used instead. [cert-msc24-c]

  rewind(f);
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: function 'rewind' has no error detection, 'fseek' should be used instead. [cert-msc24-c]

  void (*f_ptr4)(FILE *) = rewind;
  // CHECK-MESSAGES: :[[@LINE-1]]:28: warning: function 'rewind' has no error detection, 'fseek' should be used instead. [cert-msc24-c]

  setbuf(f, s);
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: function 'setbuf' has no error detection, 'setvbuf' should be used instead. [cert-msc24-c]

  void (*f_ptr5)(FILE *, char *) = setbuf;
  // CHECK-MESSAGES: :[[@LINE-1]]:36: warning: function 'setbuf' has no error detection, 'setvbuf' should be used instead. [cert-msc24-c]

  int i;
  (void)fscanf(f, "%d", &i);
  // CHECK-MESSAGES: :[[@LINE-1]]:9: warning: function 'fscanf' is obsolescent, 'fscanf_s' should be used instead. [cert-msc24-c]

  int (*f_ptr6)(FILE *, const char *, ...) = fscanf;
  // CHECK-MESSAGES: :[[@LINE-1]]:46: warning: function 'fscanf' is obsolescent, 'fscanf_s' should be used instead. [cert-msc24-c]
}

struct tm;
char *asctime(const struct tm *timeptr);

void f3(const struct tm *timeptr) {
  (void)asctime(timeptr);
  // CHECK-MESSAGES: :[[@LINE-1]]:9: warning: function 'asctime' is non-reentrant, 'asctime_s' should be used instead. [cert-msc24-c]

  char *(*f_ptr)(const struct tm *timeptr) = asctime;
  // CHECK-MESSAGES: :[[@LINE-1]]:46: warning: function 'asctime' is non-reentrant, 'asctime_s' should be used instead. [cert-msc24-c]
}

typedef int time_t;
char *ctime(const time_t *timer);

void f4(const time_t *timer) {
  (void)ctime(timer);
  // CHECK-MESSAGES: :[[@LINE-1]]:9: warning: function 'ctime' is non-reentrant, 'ctime_s' should be used instead. [cert-msc24-c]

  char *(*f_ptr)(const time_t *timer) = ctime;
  // CHECK-MESSAGES: :[[@LINE-1]]:41: warning: function 'ctime' is non-reentrant, 'ctime_s' should be used instead. [cert-msc24-c]
}

typedef int errno_t;
typedef size_t rsize_t;
errno_t asctime_s(char *s, rsize_t maxsize, const struct tm *timeptr);

void fNoWarning(char *s, const struct tm *timeptr) {
  (void)asctime_s(s, 0, timeptr);
  //no-warning
}