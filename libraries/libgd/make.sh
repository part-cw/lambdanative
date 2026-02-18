PKGURL=https://github.com/libgd/libgd/releases/download/gd-2.3.3/libgd-2.3.3.tar.gz
PKGHASH=a275344fb56161df6a06679bf0bf29c30930d8eb
package_download $PKGURL $PKGHASH

package_patch

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi
if [ $SYS_PLATFORM = android ]; then
  EXTRACONF="$EXTRACONF --without-vpx"
fi

# prevent DLL hell on win32
cp src/gd.h tmp
cat tmp | sed 's/^#define BGD_DECLARE(rt) BGD_EXPORT_DATA_PROT rt BGD_STDCALL/#define BGD_DECLARE(rt) rt/g' > src/gd.h
rm tmp

if [ "$SYS_PLATFORM" = "win32" ]; then
  if grep microsoft /proc/version -i -q; then
    EXTRACONF="$EXTRACONF --build=x86_64-unknown-linux-gnu"
  fi
fi

package_configure $EXTRACONF --disable-shared --enable-static --with-png=$SYS_PREFIX --with-freetype=$SYS_PREFIX --with-jpeg=$SYS_PREFIX --without-avif --without-heif --without-tiff --without-webp --without-xpm --without-fontconfig --without-x

cd src
package_make libgd.la
asserterror $? "compilation failed"

assertfile ./.libs/libgd.a
cp ./.libs/libgd.a $SYS_PREFIX/lib
cp gd.h $SYS_PREFIX/include
cp gdfx.h $SYS_PREFIX/include
cp gd_io.h $SYS_PREFIX/include

cd ..

package_cleanup

#eof
