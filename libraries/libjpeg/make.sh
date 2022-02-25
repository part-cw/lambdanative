PKGURL=http://downloads.sourceforge.net/project/libjpeg/libjpeg/6b/jpegsrc.v6b.tar.gz
PKGHASH=7079f0d6c42fad0cfba382cf6ad322add1ace8f9

package_download $PKGURL $PKGHASH

cp $SYS_ROOT/config.guess .
veval "wget -O config.sub 'http://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=config.sub;hb=HEAD'"

package_patch

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

LIBTOOL=`which libtool`
cp $LIBTOOL .

if [ "$SYS_PLATFORM" = "win32" ]; then
  cp configure configure.tmp
  cat configure.tmp | sed 's/test -s conftest/test -s conftest.exe/g' > configure
fi

package_configure $EXTRACONF --disable-shared --enable-static 
asserterror $? "configuration failed"

package_make libjpeg.a
asserterror $? "compilation failed"

package_make install-lib

package_cleanup

#eof
