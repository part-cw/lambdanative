PKGURL=https://chipmunk-physics.net/release/Chipmunk-6.x/Chipmunk-6.2.2.tgz
PKGHASH=7b3c23953ec4157c7eb79bd883f23136cb9c5b3a
package_download $PKGURL $PKGHASH

case $SYS_PLATFORM in
bb10|playbook)
  EXTRACC=-Wc,-std=gnu99
;;
*)
  EXTRACC=-std=gnu99
;;
esac

cd src
rm *.o 2> /dev/null
echo " => compiling source.."
veval "$SYS_CC -Wall $EXTRACC -DNDEBUG -DCP_USE_DOUBLES -I../include/chipmunk -c *.c"
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
