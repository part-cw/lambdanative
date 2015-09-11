PKGURL=http://downloads.sourceforge.net/project/liboauth/liboauth-1.0.3.tar.gz 
PKGHASH=791dbb4166b5d2c843c8ff48ac17284cc0884af2

package_download $PKGURL $PKGHASH

package_patch

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

package_configure $EXTRACONF --disable-shared --enable-static 

package_make
asserterror $? "compilation failed"
package_make install
asserterror $? "compilation failed"

package_cleanup

#eof
