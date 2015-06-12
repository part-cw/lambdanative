// LambdaNative zip embedding functions

int minizip_unpack_to_disk(char *path, void *ptr, int len);
void *minizip_unpack_to_memory(void *ptr, int len, int *unpack_len);

// eof
