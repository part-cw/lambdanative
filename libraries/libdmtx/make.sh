PKGURL=https://github.com/dmtx/libdmtx.git
PKGHASH=a3e539a3e1c1076c7b96ad306f0883f9b363eb60

package_download $PKGURL $PKGHASH

package_patch

veval "./autogen.sh"
asserterror $? "failed to generate configure script"

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

if [ "$SYS_PLATFORM" = "win32" ]; then
  if grep microsoft /proc/version -i -q; then
    EXTRACONF="$EXTRACONF --build=x86_64-unknown-linux-gnu"
  fi
fi

package_configure $EXTRACONF --disable-shared --enable-static

package_make

package_make install

package_cleanup

#eof
