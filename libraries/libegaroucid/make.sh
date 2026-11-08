PKGURL=https://github.com/Nyanyan/Egaroucid/archive/v7.8.4/egaroucid-7.8.4.tar.gz
PKGHASH=54ec6fb6639d894d4391f315049afe52f8ee4a7e

package_download $PKGURL $PKGHASH

package_patch
# 32 Bit OS is needed for Android builds, consider defining conditionally
package_cmake -DBUILD_ENGINE_LIB=ON -DBUILD_CONSOLE=OFF -DBUILD_GUI=OFF -DBUILD_SHARED_LIBS=OFF -DHAS_NO_AVX2=ON -DHAS_32_BIT_OS=ON -DCMAKE_ANDROID_STL_TYPE=c++_static

package_cmake_build

install include/egaroucid/egaroucid.h -D -t $SYS_PREFIX/include/
install build/libegaroucid.a -D -t $SYS_PREFIX/lib

package_cleanup
