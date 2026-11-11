PKGURL="https://github.com/DaveGamble/cJSON/archive/refs/tags/v1.7.19.tar.gz"
PKGHASH=e66ddd2f99fd321ab53a694e6c74698eb987d056

package_download $PKGURL $PKGHASH

package_patch

echo "==> Building cJSON..."
$SYS_CC -c cJSON.c -I.
asserterror $? "compilation failed"
$SYS_AR ru $SYS_PREFIX/lib/libcjson.a cJSON.o 2> /dev/null
$SYS_RANLIB $SYS_PREFIX/lib/libcjson.a 2> /dev/null
mkdir -p $SYS_PREFIX/include/cjson
cp cJSON.h $SYS_PREFIX/include/cjson/

package_cleanup

#eof
