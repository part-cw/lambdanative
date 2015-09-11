PKGURL=http://curl.haxx.se/download/curl-7.44.0.tar.gz
PKGHASH=4dc36d2f3310585cc1f9211b5f5c71327c86fb61

package_download $PKGURL $PKGHASH

package_patch

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

package_configure $EXTRACONF --disable-ldap --disable-shared --enable-static 

package_make
asserterror $? "compilation failed"
package_make install
asserterror $? "compilation failed"

package_cleanup

#eof
