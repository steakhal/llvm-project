// RUN: %check_clang_tidy %s cert-msc24-c %t

#define __STDC_LIB_EXT1__ 1

char *gets(char *s);

void f1(char *s) {
  (void)gets(s);
  // CHECK-MESSAGES: :[[@LINE-1]]:9: warning: function 'gets' is deprecated as of C99, removed from C11. [cert-msc24-c]

  char *(*f_ptr)(char *) = gets;
  // CHECK-MESSAGES: :[[@LINE-1]]:28: warning: function 'gets' is deprecated as of C99, removed from C11. [cert-msc24-c]
}

typedef void FILE;

void rewind(FILE *stream);
void setbuf(FILE *stream, char *buf);

void f2(char *s, FILE *f) {
  rewind(f);
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: function 'rewind' has no error detection, 'fseek' should be used instead. [cert-msc24-c]

  void (*f_ptr1)(FILE *) = rewind;
  // CHECK-MESSAGES: :[[@LINE-1]]:28: warning: function 'rewind' has no error detection, 'fseek' should be used instead. [cert-msc24-c]

  setbuf(f, s);
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: function 'setbuf' has no error detection, 'setvbuf' should be used instead. [cert-msc24-c]

  void (*f_ptr2)(FILE *, char *) = setbuf;
  // CHECK-MESSAGES: :[[@LINE-1]]:36: warning: function 'setbuf' has no error detection, 'setvbuf' should be used instead. [cert-msc24-c]
}

struct tm;
char *asctime(const struct tm *timeptr);

void f3(const struct tm *timeptr) {
  (void)asctime(timeptr);

  char *(*f_ptr)(const struct tm *timeptr) = asctime;
}
