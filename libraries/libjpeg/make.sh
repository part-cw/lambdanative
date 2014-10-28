PKGURL=http://downloads.sourceforge.net/project/libjpeg/libjpeg/6b/jpegsrc.v6b.tar.gz
PKGHASH=7079f0d6c42fad0cfba382cf6ad322add1ace8f9

package_download $PKGURL $PKGHASH

veval "wget -O config.guess 'http://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=config.guess;hb=HEAD'"
veval "wget -O config.sub 'http://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=config.sub;hb=HEAD'"

package_patch

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

package_configure $EXTRACONF --disable-shared --enable-static 
asserterror $? "configuration failed"

package_make libjpeg.a
asserterror $? "compilation failed"

package_make install-lib

package_cleanup

#eof
