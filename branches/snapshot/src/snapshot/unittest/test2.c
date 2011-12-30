#include <stdio.h>
#include <unistd.h>
#include <string.h>

int main(void) {
	char *offset = "jiawei";
	char buf[10];

	memset(buf, 0, 10);
	printf("len is %d\n", strlen(offset));
	g_strlcpy(buf, offset, strlen(offset));
	printf("buf is [%s]\n", buf);
	return 0;
}
