
PKGURL=https://github.com/nih-at/libzip/releases/download/v1.12.1/libzip-1.12.1.tar.gz
PKGHASH=3b66fe92cbc768d8d4d16e1427bfb7399e15d9c2795c2fc482e7b30a6332a422

package_download $PKGURL $PKGHASH

asserttool cmake
package_cmake \
  -DBUILD_DOC=OFF \
  -DBUILD_SHARED_LIBS=OFF \
  -DBUILD_TOOLS=OFF \
  -DBUILD_REGRESS=OFF \
  -DENABLE_COMMONCRYPTO=OFF \
  -DENABLE_GNUTLS=OFF \
  -DENABLE_OPENSSL=OFF \
  -DENABLE_WINDOWS_CRYPTO=OFF \
  -DENABLE_BZIP2=OFF \
  -DENABLE_LZMA=OFF \
  -DENABLE_ZSTD=OFF
package_cmake_build
package_cmake_install

package_cleanup

# eof
