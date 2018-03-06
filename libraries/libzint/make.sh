PKGURL=https://github.comgit/woo-j/zint.git
PKGHASH=2e5fe31ebfba09e07c934434c3885dc40224a5bf

package_download $PKGURL $PKGHASH

package_patch

cd backend

rm *.o 2> /dev/null
libzint_version=`cat Makefile | grep DZINT_VERSION | cut -f 2- -d "="`
libzint_srcs=`ls -1 *.c | sed '/dllversion.c/d' | tr '\n' ' '`
veval "$SYS_CC -I$SYS_PREFIX/include $libzint_version -c $libzint_srcs"
asserterror $? "compilation failed"
veval "$SYS_AR ru $SYS_PREFIX/lib/libzint.a *.o"
asserterror $? "ar failed"
veval "$SYS_RANLIB $SYS_PREFIX/lib/libzint.a"
asserterror $? "ranlib failed"

cat zint.h | sed 's/^ZINT_EXTERN//g' > $SYS_PREFIX/include/zint.h

cd ..

package_cleanup

#eof
