PKGURL=http://curl.haxx.se/download/curl-8.23.0.tar.gz
PKGHASH=dde79cd20f6b0d6dc7f4dcc3dd71c472fc2ebcee

package_download $PKGURL $PKGHASH

package_patch

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

package_configure $EXTRACONF --without-libpsl --with-openssl --disable-ldap --disable-shared --enable-static --without-librtmp --without-libidn --without-libssh2

package_make
asserterror $? "compilation failed"
package_make install
asserterror $? "compilation failed"

package_cleanup

#eof
