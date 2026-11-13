
PKGURL=https://www.openldap.org/software/download/OpenLDAP/openldap-release/openldap-2.6.16.tgz
PKGHASH=8d8452bc3f56aa7a392c62dd377733f83d1314eed6c52fe44028439a3bb4df18

package_download $PKGURL $PKGHASH

package_patch

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
