PKGURL=https://github.com/codeplea/genann.git

package_download $PKGURL

rm *.o 2> /dev/null
veval "$SYS_CC -c genann.c genann.o"
asserterror $? "compilation failed"
veval "$SYS_AR rcs $SYS_PREFIX/lib/libgenann.a genann.o"
asserterror $? "ar failed"

cp genann.h $SYS_PREFIX/include
