#include "system_encoding.h"

char* get_system_encoding() {
	return nl_langinfo(CODESET);
}
