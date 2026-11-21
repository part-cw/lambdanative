PKGURL=https://github.com/libharu/libharu/archive/refs/tags/v2.4.6.tar.gz
PKGHASH=64c0f790c0cde24b69050a67b0337524fb2580b8

package_download $PKGURL $PKGHASH

EXTRACONF=
if [ ! "$SYS_PLATFORM" = "$SYS_HOSTPLATFORM" ]; then
  EXTRACONF=--host=`echo "${SYS_ARCH}-" | cut -f 1 -d "-"`
fi

asserttool cmake
package_cmake \
  -DBUILD_SHARED_LIBS=OFF \
  -DCMAKE_INSTALL_LIBDIR=lib \
  -DCMAKE_DISABLE_FIND_PACKAGE_ZLIB=TRUE \
  -DCMAKE_DISABLE_FIND_PACKAGE_PNG=TRUE
package_cmake_build
package_cmake_install
package_cleanup

#eof
