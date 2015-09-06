
PKGURL=git://git.openldap.org/openldap.git
PKGBRANCH=mdb.master
PKGHASH=

package_download $PKGURL $PKGHASH $PKGBRANCH

cd libraries/liblmdb
rm *.o 2> /dev/null
veval "$SYS_CC -c mdb.c midl.c"
asserterror $? "compilation failed"
veval "$SYS_AR ru liblmdb.a *.o"
asserterror $? "compilation failed"
veval "$SYS_RANLIB liblmdb.a"
asserterror $? "compilation failed"
cp liblmdb.a $SYS_PREFIX/lib
cp *.h $SYS_PREFIX/include
cd ..

package_cleanup

#eof
