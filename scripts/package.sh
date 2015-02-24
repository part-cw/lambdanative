
# procedures to handle download and compilation of packages

tmp_install=$SYS_TMPDIR/tmp_install

package_valid()
{
  pkg_valid_file="$1"
  pkg_valid_hash=$2
  pkg_valid=no
  if [ -f $pkg ]; then
    if [ $SYS_HOSTPLATFORM = openbsd ]; then
      if [ `sha1 -q $pkg_valid_file` = "$pkg_valid_hash" ]; then
        pkg_valid=yes
      fi
    else
      if [ `shasum $pkg_valid_file | cut -f 1 -d " "` = "$pkg_valid_hash" ]; then
        pkg_valid=yes
      fi
    fi
  fi
  echo $pkg_valid
}

package_unpack()
{
  pkg_unpack_file=$1
  echo " => extracting $pkg_unpack_file.."
  pkg_unpack_extension=`echo "${pkg_unpack_file}" | sed 's/.*\.//'`
  case $pkg_unpack_extension in
    zip) 
         asserttool unzip
         unzip -qq $pkg_unpack_file
         ;;
    tar)
         asserttool tar
         tar xf $pkg_unpack_file
         ;;
    gz|tgz)
         asserttool tar
         tar zxf $pkg_unpack_file
         ;;
    bz)
         asserttool tar
         tar jxf $pkg_unpack_file
         ;;
    xz|txz)
         asserttool tar
         tar Jxf $pkg_unpack_file
         ;;
  esac
}

package_patch()
{
  echo " => patching source..."
  pkg_patches=`ls -1 ../*.patch 2> /dev/null`
  for p in $pkg_patches; do
    if [ ! "X$p" = "X" ] && [ -f $p ]; then
      echo " => applying patches from $p"
      veval "patch -p0 < $p"
      asserterror $? "patching failed"
    fi
  done
}

package_download_git()
{
  if [ $SYS_HOSTPLATFORM = win32 ]; then
    pkg_git_url=`echo "$1" | sed 's/^git:/http:/'`
  else 
    pkg_git_url=$1
  fi
  pkg_git_hash=$2
  pkg_git_file="$SYS_PREFIXROOT/packages/"`basename $pkg_git_url`"-$pkg_git_hash.tgz"
  if [ -d $tmp_install ]; then
    rm -rf $tmp_install
  fi
  mkdir $tmp_install
  assertfile $tmp_install
  pkg_here=`pwd`
  cd $tmp_install
  if [ ! -f $pkg_git_file ]; then
    echo " => cloning ${pkg_git_url}.."
    veval "git clone $pkg_git_url"
    asserterror $? "repository cloning failed [$pkg_git_url]"
    cd *
    if [ ! "X$pkg_git_hash" = "X" ]; then
      veval "git checkout $pkg_git_hash"
      asserterror $? "repository checkout failed [$pkg_git_url]"
    else
      echo " == "`git log | head -n 1`
    fi
    cd ..
    veval "tar -zcvf $pkg_git_file *"
    assertfile "$pkg_git_file"
    cd *
  else
    assertfile "$pkg_git_file"
    package_unpack "$pkg_git_file"
    asserterror $? "package extraction failed [$pkg_git_file]"
    cd *
  fi
  cp $pkg_here/*.patch $tmp_install 2> /dev/null
}

package_download_ball()
{
  pkg_url=$1
  pkg_hash=$2
  pkg=$SYS_PREFIXROOT/packages/`basename $pkg_url`
  if [ $SYS_HOSTPLATFORM = openbsd ]; then
    asserttool sha1
  else 
    asserttool shasum
  fi
  if [ `package_valid "$pkg" $pkg_hash` = no ]; then
    echo " => downloading ${pkg_url}.."
    rmifexists $pkg
    asserttool wget
    wget_quiet=-q
    if [ ! "X$SYS_VERBOSE" = "X" ]; then
      wget_quiet=
    fi
    wget $pkg_url $wget_quiet -O $pkg
    if [ ! $? = 0 ]; then
       echo "WARNING: wget failed, retrying without certificate check.."
       rmifexists $pkg
       wget $pkg_url $wget_quiet --no-check-certificate -O $pkg
    fi
    asserterror $?  "download of $pkg_url failed"
    if [ $SYS_HOSTPLATFORM = openbsd ]; then
      echo " == hash "`sha1 -q $pkg`
    else
      echo " == hash "`shasum $pkg | cut -f 1 -d " "`
    fi
    if [ `package_valid "$pkg" $pkg_hash` = no ]; then
      rmifexists $pkg
      assert "Unable to proceed! package download failed (checksum error)"
    fi
  fi
  assertfile $pkg
  if [ -d $tmp_install ]; then
    rm -rf $tmp_install
  fi
  mkdir $tmp_install
  assertfile $tmp_install
  pkg_here=`pwd`
  cd $tmp_install
  package_unpack $pkg
  asserterror $? "package extraction failed [$pkg]"
  cd *
  cp $pkg_here/*.patch $tmp_install 2> /dev/null
}

package_download()
{
  pkg_dnl_hdr=`echo "$1" | cut -f 1 -d ":"`
  pkg_dnl_ext=`echo "$1" | sed 's/.*\.//'`
  case $pkg_dnl_ext in
    git)
      package_download_git $@
      ;;
    *)
      case $pkg_dnl_hdr in
        git) 
          package_download_git $@
        ;;
        *)
          package_download_ball $@
        ;;
      esac
      ;;
  esac
}

package_configure()
{
  echo " => configuring source.."
  pkg_conf_opt=$@
  veval "\
  CHOST=$SYS_ARCH \
  PKG_CONFIG_PATH=$SYS_PREFIX/lib/pkgconfig \
  PATH=\"$SYS_PREFIX/bin:$PATH\" \
  LDFLAGS=-L$SYS_PREFIX/lib \
  LD_LIBRARY_PATH=$SYS_PREFIX/lib \
  CPPFLAGS=-I$SYS_PREFIX/include \
  CC=\"$SYS_CC -I$SYS_PREFIX/include -L$SYS_PREFIX/lib\" \
  AR=$SYS_AR \
  RANLIB=$SYS_RANLIB \
  ./configure --prefix=$SYS_PREFIX $pkg_conf_opt "
  asserterror $? "configure failed"
}

package_make()
{
  pkg_make=make
  if [ ! "X"`which gmake 2> /dev/null` = "X" ]; then
    vecho " == found gmake, using instead of make"
    pkg_make="gmake -j 9"
  fi
  pkg_make_opt=$@
  echo " => compiling source.."
  veval "\
  CHOST=$SYS_ARCH \
  PKG_CONFIG_PATH=$SYS_PREFIX/lib/pkgconfig \
  PATH=\"$SYS_PREFIX/bin:$PATH\" \
  LDFLAGS=-L$SYS_PREFIX/lib \
  LD_LIBRARY_PATH=$SYS_PREFIX/lib \
  CPPFLAGS=-I$SYS_PREFIX/include \
  CC=\"$SYS_CC -I$SYS_PREFIX/include -L$SYS_PREFIX/lib\" \
  AR=$SYS_AR \
  RANLIB=$SYS_RANLIB \
  $pkg_make $pkg_make_opt"
  asserterror $? "$pkg_make failed"
}

package_cleanup()
{
  echo " => cleaning up.."
  cd $pkg_here
  if [ -d $tmp_install ]; then
    rm -rf $tmp_install
  fi
}

#eof
