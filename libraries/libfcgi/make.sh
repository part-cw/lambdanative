PKGURL=https://github.com/FastCGI-Archives/FastCGI.com/raw/master/original_snapshot/fcgi-2.4.1-SNAP-0910052249.tar.gz
PKGHASH=4360b48782d757bdad301aa1769540e3f5d0d5c5
package_download $PKGURL $PKGHASH

EXTRACONF=
if [ ! "$SYS_PLATFORM" = "$SYS_HOSTPLATFORM" ]; then
  EXTRACONF=--host=`echo "${SYS_ARCH}-" | cut -f 1 -d "-"`
fi

if [ "$SYS_PLATFORM" = "win32" ]; then
  if grep microsoft /proc/version -i -q; then
    EXTRACONF="$EXTRACONF --build=x86_64-unknown-linux-gnu"
  fi
fi

# the shipping config.guess is out of date, fix that
cp $SYS_ROOT/config.guess .
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
