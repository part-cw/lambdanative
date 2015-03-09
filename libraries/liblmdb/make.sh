
PKGURL=https://gitorious.org/mdb/mdb.git
PKGHASH=3368d1f5e243225cba4d730fba19ff600798ebe3

package_download $PKGURL $PKGHASH

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
