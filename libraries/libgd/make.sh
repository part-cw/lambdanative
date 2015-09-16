PKGURL=https://github.com/libgd/libgd/archive/gd-2.1.1.tar.gz
PKGHASH=b089ee7fc0b47b8aa4c2c70087506806637f03be
package_download $PKGURL $PKGHASH

# This version needs bootstrapping before configuration
./bootstrap.sh

package_patch

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

# prevent DLL hell on win32
cp src/gd.h tmp
cat tmp | sed 's/^#define BGD_DECLARE(rt) BGD_EXPORT_DATA_PROT rt BGD_STDCALL/#define BGD_DECLARE(rt) rt/g' > src/gd.h
rm tmp

package_configure $EXTRACONF --disable-shared --enable-static --with-png=$SYS_PREFIX --with-freetype=$SYS_PREFIX --with-jpeg=$SYS_PREFIX --without-tiff --without-xpm --without-fontconfig

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
