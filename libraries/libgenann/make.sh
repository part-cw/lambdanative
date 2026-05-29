PKGURL=https://github.com/codeplea/genann/archive/refs/tags/v1.0.0.tar.gz
PKGHASH=d8547e43c135cb85a88b5de6208e352852232500

package_download $PKGURL $PKGHASH

rm *.o 2> /dev/null
veval "$SYS_CC -c genann.c -o genann.o"
asserterror $? "compilation failed"
veval "$SYS_AR rcs $SYS_PREFIX/lib/libgenann.a genann.o"
asserterror $? "ar failed"

cp genann.h $SYS_PREFIX/include
