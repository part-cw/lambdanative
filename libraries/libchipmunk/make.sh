
PKGURL=https://chipmunk-physics.net/release/ChipmunkLatest.tgz
PKGHASH=593a15a9032586e56b16d22d84f4f04c1f11a44e
package_download $PKGURL $PKGHASH

cd src
rm *.o 2> /dev/null
echo " => compiling source.."
veval "$SYS_CC -Wall -Wc,-std=gnu99 -DNDEBUG -DCP_USE_DOUBLES -I../include/chipmunk -c *.c"
asserterror $? "compilation failed"
veval "$SYS_AR ru libchipmunk.a *.o"
asserterror $? "linking failed"
veval "$SYS_RANLIB libchipmunk.a"
asserterror $? "linking failed"
cp libchipmunk.a $SYS_PREFIX/lib
cd ..
cp -R include/chipmunk $SYS_PREFIX/include

package_cleanup

# eof
