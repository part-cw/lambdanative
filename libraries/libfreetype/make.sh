PKGURL=https://download-mirror.savannah.gnu.org/releases/freetype/freetype-2.15.0.tar.gz
PKGHASH=9186627467a616d9a9e3e574748ac0c45fa77398e6364edef3041a8ba8d92f64

package_download $PKGURL $PKGHASH
package_patch

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  if [ "$SYS_PLATFORM" = "ios" ]; then
    if [ "$SYS_CPU" = "x86_64" ]; then
      EXTRACONF=--host=x86_64-apple-darwin
    else
      EXTRACONF=--host=arm-apple-darwin
    fi
  else
    EXTRACONF=--host=$SYS_ARCH
  fi
fi

if [ "$SYS_PLATFORM" = "win32" ]; then
  if grep microsoft /proc/version -i -q; then
    EXTRACONF="$EXTRACONF --build=x86_64-unknown-linux-gnu"
  fi
fi

if [ "$SYS_PLATFORM" = "macosx" ] || [ "$SYS_PLATFORM" = "ios" ]; then
  export ac_cv_header_hvf_Scaler_h=no
fi

package_configure $EXTRACONF --enable-static --disable-shared --without-png --without-harfbuzz --without-zlib --without-bzip2 --without-brotli --enable-freetype-config

if [ "$SYS_PLATFORM" = "macosx" ] || [ "$SYS_PLATFORM" = "ios" ]; then
  unset ac_cv_header_hvf_Scaler_h
fi

package_make
package_make install
package_cleanup

#eof
