PKGURL=https://jpegclub.org/reference/wp-content/uploads/2026/01/jpegsrc.v10.tar.gz
PKGHASH=73eab5302331c845d3d4b02b83fc1d22c4b276c9

package_download $PKGURL $PKGHASH
package_patch

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

package_configure $EXTRACONF --disable-shared --enable-static 
asserterror $? "configuration failed"

package_make libjpeg.la
asserterror $? "compilation failed"

package_make install-libLTLIBRARIES

package_cleanup

#eof
