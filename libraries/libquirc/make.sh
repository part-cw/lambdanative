
PKGURL=https://github.com/dlbeer/quirc.git
PKGHASH=42c79481e78bf5d13ed046b0722c6a1f78b29681

package_download $PKGURL $PKGHASH

package_patch

cd lib

rm *.o 2> /dev/null
veval "$SYS_CC -I$SYS_PREFIX/include -c *.c"
asserterror $? "compilation failed"
veval "$SYS_AR ru $SYS_PREFIX/lib/libquirc.a *.o"
asserterror $? "ar failed"
veval "$SYS_RANLIB $SYS_PREFIX/lib/libquirc.a"
asserterror $? "ranlib failed"

cp quirc.h $SYS_PREFIX/include

cd ..

package_cleanup

#eof
