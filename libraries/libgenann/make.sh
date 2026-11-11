PKGURL=https://github.com/codeplea/genann/archive/refs/tags/v1.1.1.tar.gz
PKGHASH=56a87b3f4b791ed3673df7d169847ce24f80943a

package_download $PKGURL $PKGHASH

rm *.o 2> /dev/null
veval "$SYS_CC -c genann.c -o genann.o"
asserterror $? "compilation failed"
veval "$SYS_AR rcs $SYS_PREFIX/lib/libgenann.a genann.o"
asserterror $? "ar failed"

cp genann.h $SYS_PREFIX/include
