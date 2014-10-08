PKGURL=http://www.fastcgi.com/dist/fcgi.tar.gz
PKGHASH=c751f4947696e4589d6d736d5ceef1f00c69544e
package_download $PKGURL $PKGHASH

EXTRACONF=
if [ ! "$SYS_PLATFORM" = "$SYS_HOSTPLATFORM" ]; then
  EXTRACONF=--host=`echo "${SYS_ARCH}-" | cut -f 1 -d "-"`
fi

# the shipping config.guess is out of date, fix that
cp ../../../../config.guess .
package_configure $EXTRACONF --disable-shared --enable-static

cd libfcgi

# the windows build assumes dynamic linking, fix that
fcgi_files=`ls -1 *.c ../include/*.h`
for file in $fcgi_files; do
  cp $file ${file}.tmp
  cat ${file}.tmp | sed 's/^DLLAPI //g' > $file
  rm ${file}.tmp
done

rm *.o 2> /dev/null
fcgi_libsrcs="fcgi_stdio.c fcgiapp.c"
if [ $SYS_PLATFORM = win32 ]; then
  fcgi_libsrcs="$fcgi_libsrcs os_win32.c"
else
  fcgi_libsrcs="$fcgi_libsrcs os_unix.c"
fi
veval "$SYS_CC -I.. -I../include -c $fcgi_libsrcs"
asserterror $? "compilation failed"
veval "$SYS_AR ru $SYS_PREFIX/lib/libfcgi.a *.o"
asserterror $? "compilation failed"
veval "$SYS_RANLIB $SYS_PREFIX/lib/libfcgi.a"
asserterror $? "compilation failed"
cd ..

cp include/fcgi*.h $SYS_PREFIX/include

package_cleanup

#eof
