#include "system_encoding.h"

char* get_system_encoding() {
  setlocale(LC_ALL,"");
  return nl_langinfo(CODESET);
}
