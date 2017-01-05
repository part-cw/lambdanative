
PKGURL=https://github.com/jedisct1/libsodium.git
PKGHASH=24daccad117311f308d7df4fae4b21d997770452

package_download $PKGURL $PKGHASH

veval "./autogen.sh"
asserterror $? "failed to generate configure script"

if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

package_configure $EXTRACONF --disable-shared --enable-static

package_make

package_make install

package_cleanup

#eof
