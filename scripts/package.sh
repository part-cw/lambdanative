
# procedures to handle download and compilation of packages

package_valid()
{
  pkg="$1"
  hash=$2
  pkg_valid=no
  if [ -f $pkg ]; then
    if [ `shasum $pkg | cut -f 1 -d " "` = "$HASH" ]; then
      pkg_valid=yes
    fi
  fi
  echo $pkg_valid
}

package_unpack()
{
  pkg=$1
  extension=`echo ".${pkg}" | rev | cut -f 1 -d "." | rev`
  case $extension in
    zip) asserttool unzip
         unzip -qq $pkg
         ;;
    gz|tgz|tar)
         asserttool tar
         tar xf $1
         ;;
    bz)
         asserttool tar
         tar xf $1
         ;;
    xz)
         asserttool tar
         tar xf $1
         ;;
  esac
}

package_patch()
{
  echo "==> Patching source..."
  patches=`ls -1 ../../*.patch 2> /dev/null`
  for p in $patches; do
    if [ ! "X$p" = "X" ] && [ -f $p ]; then
      echo "==> applying patches from $p"
      patch -p0 < $p
    fi
  done
}

package_makequiet()
{
  pkg_makequiet= 
  if [ "X$SYS_VERBOSE" = "X" ] && [ "X$NOQUIET" = "X" ]; then
    pkg_makequiet="-s"
  fi
  echo $pkg_makequiet
}

package_confquiet()
{
  pkg_confquiet=
  if [ "X$SYS_VERBOSE" = "X" ] && [ "X$NOQUIET" = "X" ]; then
    pkg_confquiet="--quiet"
  fi
  echo $pkg_confquiet
}

package_download()
{
  URL=$1
  HASH=$2
  pkg=$SYS_PREFIXROOT/packages/`basename $URL`
  asserttool shasum
  if [ `package_valid "$pkg" $HASH` = no ]; then
    echo " => downloading ${URL}.."
    rmifexists $pkg
    asserttool wget
    wget $URL -O $pkg
    asserterror $?  "download of $URL failed"
    echo " === hash="`shasum $pkg | cut -f 1 -d " "`
    if [ `package_valid "$pkg" $HASH` = no ]; then
      rmifexists $pkg
      assert "Unable to proceed! package download failed (checksum error)"
    fi
  fi
  assertfile $pkg
  echo " => preparing to build ${pkg}.."
  if [ -d tmp_install ]; then
    rm -rf tmp_install
  fi
  mkdir tmp_install
  assertfile tmp_install
  pkg_here=`pwd`
  cd tmp_install
  package_unpack $pkg
  cd *
}

package_configure()
{
  echo " => configuring source.."
  OPT=$@
  CHOST=$SYS_ARCH \
  PKG_CONFIG_PATH=$SYS_PREFIX/lib/pkgconfig \
  PATH=$SYS_PREFIX/bin:$PATH \
  LDFLAGS=-L$SYS_PREFIX/lib \
  LD_LIBRARY_PATH=$SYS_PREFIX/lib \
  CPPFLAGS=-I$SYS_PREFIX/include \
  CC="$SYS_CC -I$SYS_PREFIX/include -L$SYS_PREFIX/lib" \
  AR="$SYS_AR" \
  RANLIB="$SYS_RANLIB" \
  ./configure `package_confquiet` --prefix=$SYS_PREFIX $OPT
  asserterror $? "configure failed [$pkg]"
}

package_make()
{
  pkg_make=make
  if [ ! "X"`which gmake 2> /dev/null` = "X" ]; then
    echo " == found gmake, using instead of make"
    pkg_make=gmake
  fi
  OPT=$@
  echo " => compiling source.."
  CHOST=$SYS_ARCH \
  PKG_CONFIG_PATH=$SYS_PREFIX/lib/pkgconfig \
  PATH=$SYS_PREFIX/bin:$PATH \
  LDFLAGS=-L$SYS_PREFIX/lib \
  LD_LIBRARY_PATH=$SYS_PREFIX/lib \
  CPPFLAGS=-I$SYS_PREFIX/include \
  CC="$SYS_CC -I$SYS_PREFIX/include -L$SYS_PREFIX/lib" \
  AR="$SYS_AR" \
  RANLIB="$SYS_RANLIB" \
  $pkg_make `package_makequiet` $OPT
  asserterror $? "$pkg_make failed [$pkg]"
}

package_cleanup()
{
  echo " => cleaning up.."
  cd $pkg_here
  if [ -d tmp_install ]; then
    rm -rf tmp_install
  fi
}

#eof
