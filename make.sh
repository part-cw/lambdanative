#!/bin/sh
# LambdaNative - a cross-platform Scheme framework
# Copyright (c) 2009-2013, University of British Columbia
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or
# without modification, are permitted provided that the
# following conditions are met:
#
# * Redistributions of source code must retain the above
# copyright notice, this list of conditions and the following
# disclaimer.
#
# * Redistributions in binary form must reproduce the above
# copyright notice, this list of conditions and the following
# disclaimer in the documentation and/or other materials
# provided with the distribution.
#
# * Neither the name of the University of British Columbia nor
# the names of its contributors may be used to endorse or
# promote products derived from this software without specific
# prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
# CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
# NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# application building, packaging and installation

. ./config.cache

. ./SETUP

evallog=`pwd`"/eval.log"

if [ -f $evallog ]; then
  rm $evallog
fi

#########################
# general functions

# since os x doesn't have md5sum
md5summer=md5sum
if [ "X"`which md5sum 2> /dev/null` = "X" ]; then
  md5summer="md5 -r"
fi

veval()
{
  if [ $SYS_VERBOSE ]; then 
    echo "$1"
    eval $1
  else 
    echo "$1" > $evallog
    eval $1 >> $evallog 2>&1
  fi
}

vecho()
{
  if [ $SYS_VERBOSE ]; then 
    echo "$1"
  fi
}

getparam()
{
  if [ "$1" = "1" ]; then echo "$2"; fi
  if [ "$1" = "2" ]; then echo "$3"; fi
  if [ "$1" = "3" ]; then echo "$4"; fi
  if [ "$1" = "4" ]; then echo "$5"; fi
  if [ "$1" = "5" ]; then echo "$6"; fi
}

rmifexists()
{
  if [ "$1" ]; then
    if [ -e "$1" ]; then
      vecho " => removing old $1.."
      rm -rf "$1"
    fi
  fi
}

string_contains() {
  string="$1"
  substring="$2"
  if test "${string#*$substring}" != "$string"; then
    echo yes
  else
    echo no
  fi
}

###############################
# keep track of state to reset partial builds

statefile=`pwd`"/make.state"

setstate()
{
  echo "state=$1" > $statefile
  rmifexists $evallog
}

resetstate()
{
  state=
  if [ -f $statefile ]; then
    . $statefile
    rm $statefile
  fi
  if [ ! "X$state" = "X" ]; then
    echo " ** WARNING: previous build aborted unexpectantly in state: $state"
    case $state in
      ARTWORK)
       file=`locatefile apps/$SYS_APPNAME/artwork.eps silent`
       if [ -f $file ]; then
         touch $file
       fi
      ;;
      STRINGS)
       if [ -d "$SYS_PREFIXROOT/build/$SYS_APPNAME/strings" ]; then
         rm "$SYS_PREFIXROOT/build/$SYS_APPNAME/strings/*.scm"
       fi
       ;;
      FONTS)
       if [ -d "$SYS_PREFIXROOT/build/$SYS_APPNAME/fonts" ]; then
         rm "$SYS_PREFIXROOT/build/$SYS_APPNAME/fonts/*.scm"
       fi
       ;;
      TEXTURES)
       if [ -d "$SYS_PREFIXROOT/build/$SYS_APPNAME/textures" ]; then
         rm "$SYS_PREFIXROOT/build/$SYS_APPNAME/textures/*.scm"
       fi
       ;;
    esac
  fi
} 

#################################
# abort on missing files and errors

assertfile()
{
  if [ ! -s "$1" ]; then
    if [ -f $evallog ]; then
      cat $evallog | sed '/^$/d'
    fi
    echo "ERROR: failed on file $1" 1>&2
    if [ ! "X$2" = "X" ]; then 
      echo ">> $2"
    fi
    if [ "X$SYS_VERBOSE" = "X" ]; then  
      echo "BUILD FAILED - configure with verbose option for more information" 
    else 
      echo "BUILD FAILED"
    fi
    exit 1
  fi
}

asserterror()
{
  if [ ! $1 = 0 ]; then
    if [ -f $evallog ]; then
      cat $evallog | sed '/^$/d'
    fi
    echo "ERROR: failed with exit code $1" 1>&2
    if [ ! "X$2" = "X" ]; then 
      echo ">> $2"
    fi
    if [ "X$SYS_VERBOSE" = "X" ]; then  
      echo "BUILD FAILED - configure with verbose option for more information" 
    else 
      echo "BUILD FAILED"
    fi
    exit 1
  fi
}

asserttool()
{
  for tool in $@; do
    if [ "X"`which $tool 2> /dev/null` = "X" ]; then 
      echo "ERROR: required tool $tool not found." 
      echo "Please install a package containing this tool before proceeding."
    if [ "X$SYS_VERBOSE" = "X" ]; then  
      echo "BUILD FAILED - configure with verbose option for more information" 
    else 
      echo "BUILD FAILED"
    fi
      exit 1
    else 
      vecho "  => $tool.. ok" 
    fi
  done
}

#################################
# misc file and directory support

stringhash()
{
  echo "$1" | $md5summer | cut -f 1 -d " "
}

isnewer()
{
  result=no
  if [ ! -e "$2" ]; then
    result=yes
  else
    if `test "$1" -nt "$2"`; then
      result=yes
    fi
  fi
  echo $result
}

newersourceindir()
{
  result="no"
  tgt="$2"
  dir=`dirname $1`
  srcfiles=`ls -1 $dir/*.scm` 
  for src in $srcfiles; do
    if `test "$src" -nt "$tgt"`; then
      result="yes"
    fi
  done
  echo $result
}

newerindir()
{
  result="no"
  if [ ! -e "$2" ]; then
    result=yes
  else
    tgt="$2"
    if [ -d $1 ]; then
      dir=$1
    else
      dir=`dirname $1`
    fi
    srcfiles=`ls -1 $dir/*`
    for src in $srcfiles; do
      if `test "$src" -nt "$tgt"`; then
        result="yes"
      fi
    done
  fi
  echo $result
}

locatetest()
{
  here=`pwd`
  file=
  dirs=$(echo "$SYS_PATH" | tr ":" "\n")
  for dir in $dirs; do
    tmp="$dir/$2"
    if [ ! "X$tmp" = "X" ]; then
      if [ $1 $tmp ]; then
        if [ "X$file" = "X" ]; then
          file=$tmp
        else
          if [ ! "X$3" = "Xsilent" ]; then
             echo "WARNING: $file shadows $tmp" 1>&2
          fi
        fi
      fi
    fi 
  done
  if [ "X$file" = "X" ]; then
    if [ ! "X$3" = "Xsilent" ]; then
      echo "WARNING: [$2] not found" 1>&2
    fi
  fi
  if [ ! -e $file ]; then
    if [ ! "X$3" = "Xsilent" ]; then
      echo "WARNING: [$2] not found" 1>&2
    fi
  fi
  cd "$here"
  echo $file
}

locatedir()
{
  locatetest "-d" $@
}

locatefile()
{
  locatetest "-f" $@
}

wildcard_dir()
{
  find $1 -maxdepth 0 -print |sort -r| head -n 1
}

#################################
# autoconf-like parameter substitution

ac_cache=`pwd`/tmp.subst

if [ -f $ac_cache ]; then
  rm $ac_cache
fi

ac_subst()
{
  ac_tool=$SYS_HOSTPREFIX/bin/subtool
  if [ "X$2" = "X" ]; then
    paramcmd="echo \"\$${1}\""
    substcmd="$ac_tool $ac_cache $1 \"`eval $paramcmd`\""
  else
    substcmd="$ac_tool $ac_cache $1 \"$2\""
  fi 
  eval $substcmd
}

ac_output()
{
  ac_tool=$SYS_HOSTPREFIX/bin/subtool
  infile=`echo "${1}" | sed 's/\.in$//'`.in
  if [ "X$2" = "X" ]; then
    outfile=$1
  else 
    outfile=$2
  fi
  assertfile $infile "substitution source file $infile not found"
  cat $infile | $ac_tool $ac_cache > $outfile
  asserterror $? "substitution failed on $infile"
}

#################################
# misc source code checks

has_module()
{
  res=no
  modfile=`locatefile apps/$SYS_APPNAME/MODULES`
  if [ -f "$modfile" ]; then
    if [ ! "X"`cat "$modfile" | grep "$1" | cut -c 1` = "X" ]; then
       res=yes
    fi
  fi 
  echo $res
}

is_gui_app()
{
  res=`has_module ln_glcore`
  if [ "$res" = "no" ]; then
    res=`has_module glcore`
  fi
  echo "$res"
}

is_standalone_app()
{
  neg=`has_module eventloop`
  if [ $neg = no ]; then
    neg=yes
  else
    neg=no
  fi
  echo $neg
}

###########################
# general compiler functions

GAMBIT_DEFS="-D___SINGLE_HOST -D___LIBRARY -D___PRIMAL"

compile()
{
  if [ $SYS_MODE = "debug" ]; then
    opts="(declare (block)(not safe)(debug)(debug-location))"
    optc="-g -Wall -Wno-unused-variable -Wno-unused-label -Wno-unused-parameter"
  else 
    opts="(declare (block)(not safe))"
    optc="-O2 -Wall -Wno-unused-variable -Wno-unused-label -Wno-unused-parameter"
  fi
  src="$1"
  ctgt="$2"
  otgt="$3"
  defs="$4"
  path=`dirname $src`
  here=`pwd`
  cd $path
  rmifexists "$ctgt"
  if [ `string_contains "$modules" "syntax-case"` = yes ]; then
    if [ ! -f ${SYS_HOSTPREFIX}/lib/gambcext.tmp ]; then
      echo " => compiling syntax-case dynamic library.." 
      veval "$SYS_GSC -dynamic -o  ${SYS_HOSTPREFIX}/lib/gambcext.o1 ${SYS_HOSTPREFIX}/lib/syntax-case.scm"
      mv ${SYS_HOSTPREFIX}/lib/gambcext.o1 ${SYS_HOSTPREFIX}/lib/gambcext.tmp
    fi
    assertfile ${SYS_HOSTPREFIX}/lib/gambcext.tmp
    cp ${SYS_HOSTPREFIX}/lib/gambcext.tmp ${SYS_HOSTPREFIX}/lib/gambcext.o1
  else
    rmifexists ${SYS_HOSTPREFIX}/lib/gambcext.o1
  fi
  echo " => $src.." 
  veval "$SYS_GSC -prelude \"$opts\" -c -o $ctgt $src"
  assertfile "$ctgt"
  rmifexists "$otgt"
  veval "$SYS_ENV $SYS_CC $defs -c -o $otgt $ctgt -I$SYS_PREFIX/include -I$path"
  assertfile "$otgt"
  cd $here
}

compile_payload()
{
  name=$1
  srcs="$2"
  csrcs=
  libs="$3"
  objs=
#  defs="-D___SINGLE_HOST -D___LIBRARY -D___PRIMAL"
  defs="$GAMBIT_DEFS"
  echo "==> creating payload needed for $SYS_APPNAME.."
  mkdir -p "$SYS_PREFIX/build"
  dirty=no
  for src in $srcs; do
    chsh=`stringhash "$src"`
    ctgt="$SYS_PREFIX/build/$chsh.c"
    csrcs="$csrcs $ctgt"
    otgt=`echo "$ctgt" | sed 's/c$/o/'`
    objs="$objs $otgt"
    flag=no
    if [ ! -f "$otgt" ]; then
      flag=yes
    else 
      topdir=`dirname $src`
      topdir=`basename $topdir`
      if [[ "X$topdir" = "Xtextures" || "X$topdir" = "Xstrings" || "X$topdir" = "Xfonts" ]]; then
        if [ `isnewer "$src" "$otgt"` = "yes" ]; then
          flag=yes
        fi
      else
        if [ `newersourceindir "$src" "$otgt"` = "yes" ]; then
          flag=yes
        fi
      fi
    fi
    if [ $flag = yes ]; then
      dirty=yes
      compile "$src" "$ctgt" "$otgt" "$defs"
    fi
  done
  lctgt=`echo "$ctgt" | sed 's/\.c$/\_\.c/'`
  lotgt=`echo "$lctgt" | sed 's/c$/o/'`
  if [ ! -f "$lotgt" ]; then
    dirty=yes
  fi
  if [ $dirty = yes ]; then
    echo " => creating gsc link.."
    vecho "$SYS_GSC -link $csrcs"
    $SYS_GSC -link $csrcs 
    assertfile $lctgt
    veval "$SYS_ENV $SYS_CC $defs -o $lotgt -c $lctgt -I$SYS_PREFIX/include"
    assertfile $lotgt
  fi
  objs="$objs $lotgt"
  hookhash=`stringhash "apps/$SYS_APPNAME/hook.c"`
  hctgt="$SYS_PREFIX/build/$hookhash.c"
  hotgt=`echo "$hctgt" | sed 's/c$/o/'`
  if [ ! -f "$hotgt" ]; then
    dirty=yes
  fi
  if [ $dirty = yes ]; then
    echo " => generating hook.."
    linker=`echo $chsh"__"`
cat > $hctgt  << _EOF
// automatically generated. Do not edit.
#include <stdlib.h>
#include <CONFIG.h>
#define ___VERSION 407000
#include <gambit.h>
#define LINKER ____20_$linker
___BEGIN_C_LINKAGE
extern ___mod_or_lnk LINKER (___global_state_struct*);
___END_C_LINKAGE
___setup_params_struct setup_params;
int debug_settings = ___DEBUG_SETTINGS_INITIAL;
#ifdef STANDALONE
char **cmd_argv;
int cmd_argc=0;
int main(int argc, char *argv[])
{
  cmd_argc=argc; cmd_argv=argv;
  ___setup_params_reset (&setup_params);
  setup_params.version = ___VERSION;
  setup_params.linker = LINKER;
  debug_settings = (debug_settings & ~___DEBUG_SETTINGS_REPL_MASK) | 
  (___DEBUG_SETTINGS_REPL_STDIO << ___DEBUG_SETTINGS_REPL_SHIFT);
  setup_params.debug_settings = debug_settings;
  ___setup(&setup_params);
  ___cleanup();
  return 0;
}
#else 
#include <stdio.h>
void ffi_event(int t, int x, int y)
{
  static int lock=0;
  static int gambitneedsinit=1;
  if (lock) { return; } else { lock=1; }
  if (gambitneedsinit) { 
      ___setup_params_reset (&setup_params);
      setup_params.version = ___VERSION;
      setup_params.linker = LINKER;
      debug_settings = (debug_settings & ~___DEBUG_SETTINGS_REPL_MASK) | 
        (___DEBUG_SETTINGS_REPL_STDIO << ___DEBUG_SETTINGS_REPL_SHIFT);
      setup_params.debug_settings = debug_settings;
      ___setup(&setup_params);
//      ___disable_heartbeat_interrupts(); //@@
      gambitneedsinit=0;
  }
  if (!gambitneedsinit) scm_event(t,x,y);
  if (t==EVENT_TERMINATE) { ___cleanup(); exit(0); }
  lock=0;
}
#endif
_EOF
    if [ `is_standalone_app` = "yes" ]; then
      defs="$defs -DSTANDALONE"
    fi
    veval "$SYS_ENV $SYS_CC $defs -c -o $hotgt $hctgt -I$SYS_PREFIX/include"
    assertfile $hotgt
  fi
  objs="$objs $hotgt"
  for lib in $libs; do
   objs=`ls -1 $SYS_PREFIX/build/$lib/*.o`" $objs"
  done
  tgtlib="$SYS_PREFIX/lib/libpayload.a"
  echo " => assembling payload.."
  sobjs=
  for obj in $objs; do
    sobjs="$sobjs $obj"
  done
  rmifexists "$tgtlib"
  vecho "$SYS_AR rc $tgtlib $sobjs"
  $SYS_AR rc $tgtlib $sobjs  2> /dev/null
  vecho "$SYS_RANLIB $tgtlib"
  $SYS_RANLIB $tgtlib 2> /dev/null
  assertfile "$tgtlib"
}

##################################
# extract object files from static library

explode_library()
{
  libname=$1
  libfile="$SYS_PREFIX/lib/$libname.a"
  libdir="$SYS_PREFIX/build/$libname"
  assertfile $libfile
  if [ `isnewer $libfile $libdir` = "yes" ]; then
    rmifexists "$libdir"
    echo " => exploding library $libname.."
    mkdir -p "$libdir"
    here=`pwd`
    cd "$libdir"
    $SYS_AR -x $libfile
    cd "$here"
  fi
}

###################################
# artwork & texture generation

make_artwork()
{
  setstate ARTWORK
  echo "==> creating artwork needed for $SYS_APPNAME.."
  objsrc=`locatefile "apps/$SYS_APPNAME/artwork.obj" silent`
  epssrc=`locatefile "apps/$SYS_APPNAME/artwork.eps" silent`
  if [ "X$epssrc" = "X" ]; then
    if [ "X$objsrc" = "X" ]; then
      echo "ERROR: artwork not found"
      exit 1
    fi
    assertfile $objsrc
    epssrc=`echo $objsrc | sed 's/obj$/eps/'`
  fi
  if [ `isnewer $objsrc $epssrc` = "yes" ]; then
    if [ ! "X$objsrc" = "X" ]; then
      assertfile $objsrc
      echo " => generating eps artwork.."
      tgif -print -stdout -eps -color $objsrc > $epssrc
    fi
  fi
  assertfile $epssrc
  mkdir -p "$SYS_PREFIX/build"
  mkdir -p "$SYS_PREFIXROOT/build/$SYS_APPNAME"
  pngsrc="$SYS_PREFIXROOT/build/$SYS_APPNAME/artwork.png"
  tmpfile="$SYS_PREFIXROOT/build/$SYS_APPNAME/tmp.png"
  if [ `isnewer $epssrc $pngsrc` = "yes" ]; then
    echo " => generating master pixmap.."
    veval "gs -r600 -dNOPAUSE -sDEVICE=png16m -dEPSCrop -sOutputFile=$tmpfile $epssrc quit.ps"
    assertfile $tmpfile
    veval "convert $tmpfile -trim -transparent \"#00ff00\" $pngsrc"
    rm $tmpfile
  fi
  assertfile $pngsrc
  if [ -s targets/$SYS_PLATFORM/icon-sizes ]; then
    . targets/$SYS_PLATFORM/icon-sizes
    for s in $sizes; do
      tgt="$SYS_PREFIXROOT/build/$SYS_APPNAME/artwork-$s.png"
      if [ `isnewer $epssrc $tgt` = "yes" ]; then
        echo " => generating "$s"x"$s" pixmap.."
        convert $pngsrc -resize $s"x"$s $tgt
        assertfile $tgt
      fi
    done
  fi
  setstate
}

texture_srcs=

make_texturedir()
{
  srcdir=$1
  prefix=$2
  if [ -d "$srcdir" ]; then
    images=`ls -1 "$srcdir"/*.png 2> /dev/null`
    for imgfile in $images; do
      scmfile=$SYS_PREFIXROOT/build/$SYS_APPNAME/textures/"$prefix"`basename $imgfile | sed 's/png$/scm/'`
      texture_srcs="$texture_srcs $scmfile"
      if [ `isnewer $imgfile $scmfile` = "yes" ]; then
        echo " => $imgfile.."
        $SYS_HOSTPREFIX/bin/png2scm "$imgfile" > "$scmfile"
      fi
    done
  fi
}

make_textures()
{
  setstate TEXTURES
  echo "==> creating textures needed for $SYS_APPNAME.."
  tgtdir=$SYS_PREFIXROOT/build/$SYS_APPNAME/textures
  mkdir -p $tgtdir
  srcdir="$appsrcdir/textures"
  if [ -d $srcdir ]; then 
    make_texturedir "$srcdir" "main-"
  fi
  for m in $modules; do
    srcdir=`locatedir modules/$m/textures silent`
    if [ ! "X$srcdir" = "X" ]; then
      make_texturedir "$srcdir" "${m}-"
    fi
  done
  setstate
}

font_srcs=

make_fontfile()
{
  srcfile=$1
  prefix=$2
  if [ `isnewer $srcfile $incfile` = "yes" ]; then
    while read line; do
      fline=`echo "$line" | sed '/^#/d'`
      if [ "$fline" ]; then 
        fontname=`echo "$fline" | cut -f 1 -d " "`
        font=`locatefile fonts/$fontname`
        assertfile $font 
        bits=`echo "$fline" | cut -f 2 -d " "`
        if [ "X$bits" = "X7" ]; then 
           bits=`locatefile fonts/ascii7.set`
        else
          if [ "X$bits" = "X8" ]; then 
            bits=`locatefile fonts/ascii8.set`
          else
            bits=`locatefile fonts/$bits`
          fi
        fi
        sizes=`echo "$fline" | cut -f 3 -d " "`
        name=`echo "$fline" | cut -f 4 -d " "`
        scmfile=$tgtdir/${prefix}${name}.scm
        font_srcs="$font_srcs $scmfile"
        if [ `isnewer $srcfile $scmfile` = "yes" ]; then
           echo " => $name using glyph set $bits.."
           $SYS_HOSTPREFIX/bin/ttffnt2scm $font "$bits" $sizes $name > $scmfile
           assertfile $scmfile
        fi
      fi
    done < $srcfile
  fi
}

make_fonts()
{
  setstate FONTS
  echo "==> creating fonts needed for $SYS_APPNAME.."
  tgtdir=$SYS_PREFIXROOT/build/$SYS_APPNAME/fonts
  mkdir -p $tgtdir
  srcfile="$appsrcdir/FONTS"
  if [ -f $srcfile ]; then 
    make_fontfile "$srcfile" "main-"
  fi
  for m in $modules; do
    srcdir=`locatefile modules/$m/FONTS silent`
    if [ ! "X$srcdir" = "X" ]; then
      make_fontfile "$srcdir" "${m}-"
    fi
  done
  setstate
}

make_string_aux()
{
  oldpath=`pwd`
  newpath=`mktemp -d tmp.XXXXXX`
  cd $newpath
  srcfont=$1
  assertfile $srcfont
  fontname=`$SYS_HOSTPREFIX/bin/ttfname $srcfont`
  tgtfont="$fontname".ttf
  cp "$srcfont" "$tgtfont"
  fontpath=`pwd`"/"
  size=$2
  string="$3"
  name=$4
  scmfile="$5"
  opt="$6"
cat > tmp.tex << __EOF
\batchmode
\documentclass{article}
\makeatletter 
\renewcommand{\Large}{\@setfontsize\Large{$size}{$size}} 
\makeatother 
\usepackage{fontspec}
\usepackage{xunicode}
%\fontspec [ Path = $fontpath ]{$fontname}
\setmainfont[$opt]{[$fontname.ttf]}
\usepackage[margin=0.1in, paperwidth=40in, paperheight=2in]{geometry}
\begin{document}
\thispagestyle{empty}
\noindent \Large
$string
\end{document}
__EOF
  veval "xelatex tmp.tex"
  assertfile tmp.pdf
  if [ "X"`which pdftops 2> /dev/null` = "X" ]; then
    veval "pdf2ps tmp.pdf"
  else
    veval "pdftops tmp.pdf"
  fi
  assertfile tmp.ps
  veval "ps2eps -B -C tmp.ps"
  assertfile tmp.eps
  veval "gs -r300 -dNOPAUSE -sDEVICE=pnggray -dEPSCrop -sOutputFile=$name.png tmp.eps quit.ps"
  assertfile $name.png
  veval "convert $name.png  -bordercolor White -border 5x5 -negate -scale 25% $name.png"
  $SYS_HOSTPREFIX/bin/png2scm $name.png > $scmfile
  cd $oldpath
  rm -rf $newpath
}

string_srcs=

make_stringfile()
{
  srcfile=$1
  prefix=$2
  cat $srcfile | sed '/^#/d' > tmp.STRINGS
  echo >> tmp.STRINGS
  while read -r fline; do
    if [ "$fline" ]; then 
      fontname=`eval "getparam 1 $fline"`
      font=`locatefile fonts/$fontname`
      assertfile $font
      size=`eval "getparam 2 $fline"`
      label=`eval "getparam 3 $fline" | sed 's:_/:@TMP@:g;s:/:\\\\:g;s:@TMP@:/:g'`
      name=`eval "getparam 4 $fline"`
      opt=`eval "getparam 5 $fline"`
      scmfile=$tgtdir/${prefix}${name}.scm
      string_srcs="$string_srcs $scmfile"
      if [ `isnewer $srcfile $scmfile` = "yes" ]; then
         echo " => $name.."
         make_string_aux $font $size "$label" $name $scmfile $opt
         assertfile $scmfile
      fi
    fi
  done < tmp.STRINGS
  rm tmp.STRINGS
}

make_strings()
{
  setstate STRINGS
  echo "==> creating strings needed for $SYS_APPNAME.."
  tgtdir=$SYS_PREFIXROOT/build/$SYS_APPNAME/strings
  mkdir -p $tgtdir
  srcfile="$appsrcdir/STRINGS"
  if [ -f $srcfile ]; then 
    make_stringfile "$srcfile" "main-"
  fi
  for m in $modules; do
    srcfile=`locatefile modules/$m/STRINGS silent`
    if [ ! "X$srcfile" = "X" ]; then
      make_stringfile "$srcdir" "$m-"
    fi
  done
  setstate
}

make_sounddir()
{
  snddir=$1
  make_soundfile=$2
  echo " => processing sounds from $snddir..."
  snds=`ls -1 $snddir/*.wav 2> /dev/null`
  for snd in $snds; do
    if [ -f "$snd" ]; then
      echo " => $snd.."
      $make_soundfile "$snd"
    fi
  done
  snds=`ls -1 $snddir/*.ogg 2> /dev/null`
  for snd in $snds; do
    if [ -f "$snd" ]; then
      vecho " => $snd.."
      $make_soundfile "$snd"
    fi
  done
}

make_sounds()
{
  make_soundfile=$1
  echo " => processing sounds needed for $SYS_APPNAME.."
  srcdir="$appsrcdir/sounds"
  if [ -d "$srcdir" ]; then 
    make_sounddir "$srcdir" $make_soundfile
  fi
  for m in $modules; do
    srcdir=`locatedir modules/$m/sounds silent`
    if [ ! "X$srcdir" = "X" ]; then
      make_sounddir "$srcdir" $make_soundfile
    fi
  done
}

###################################
# platform specific stuff

make_setup()
{
  setstate SETUP
  profile=`locatefile PROFILE`
  assertfile "$profile"
  . "$profile"
  versionfile=`locatefile apps/$SYS_APPNAME/VERSION`
  assertfile $versionfile
  SYS_APPVERSION=`cat $versionfile`
  SYS_APPVERSIONCODE=`echo $SYS_APPVERSION | sed 's/\.//'`
  echo "=== using profile $SYS_PROFILE [$profile].."
  echo "=== configured to build $SYS_APPNAME version $SYS_APPVERSION for $SYS_PLATFORM on $SYS_HOSTPLATFORM in $SYS_MODE mode"
  if [ "$SYS_MODE" = "release" ]; then
    SYS_IOSCERT=$SYS_IOSRELCERT
  else
    SYS_IOSCERT=$SYS_IOSDEVCERT
  fi
  SYS_PREFIXROOT=`pwd`"-cache"
  SYS_PREFIX="$SYS_PREFIXROOT/$SYS_PLATFORM"
  SYS_HOSTPREFIX="$SYS_PREFIXROOT/$SYS_HOSTPLATFORM"
  SYS_GSC=$SYS_HOSTPREFIX/bin/gsc
  SYS_DEBUGFLAG=
  if [ "X$SYS_MODE" = "Xdebug" ]; then
    SYS_DEBUGFLAG="-g"
  fi
  mkdir -p $SYS_PREFIX/bin
  mkdir -p $SYS_PREFIX/lib
  mkdir -p $SYS_PREFIX/include
  mkdir -p $SYS_PREFIX/build
  mkdir -p $SYS_HOSTPREFIX/bin
  mkdir -p $SYS_HOSTPREFIX/lib
  mkdir -p $SYS_HOSTPREFIX/include
  mkdir -p $SYS_PREFIXROOT/packages
  mkdir -p $SYS_PREFIXROOT/build
  buildsys=$SYS_PLATFORM"_"$SYS_HOSTPLATFORM
  if [ -s targets/$SYS_PLATFORM/host_$SYS_HOSTPLATFORM ]; then
    . targets/$SYS_PLATFORM/host_$SYS_HOSTPLATFORM
  else
     echo "ERROR: don't know how to setup a build for $SYS_PLATFORM on a $SYS_HOSTPLATFORM host"
     exit 1
  fi
  SYS_LOCASEAPPNAME=`echo $SYS_APPNAME | tr A-Z a-z`
  SYS_ANDROIDAPI=$ANDROIDAPI 
  # Add git path for overlay, additional paths, and the lambdanative path
  here=`pwd`
  SYS_BUILDHASH=
  for p in $(echo "$SYS_PATH" | tr ":" "\n"); do
    cd $p
    if [ -d "$p/.git" ]; then
      SYS_BUILDHASH="$SYS_BUILDHASH"`basename $p`": "`git log --pretty=format:"%h" -1`","
    fi
    cd $here
  done
  SYS_BUILDHASH=`echo "$SYS_BUILDHASH" | sed 's/,$//'`
  echo $SYS_BUILDHASH
  SYS_BUILDEPOCH=`date +"%s"`
  SYS_HOSTEXEFIX=
  if [ "$SYS_HOSTPLATFORM" = "win32" ]; then
    SYS_HOSTEXEFIX=".exe"
  fi
  SYS_ARCH=`$SYS_CC -dumpmachine`
  # Adding BUILD info requires rebuilding of config module
  touch modules/config/config.scm
  # build the subtool
  if ! `test -x  $SYS_HOSTPREFIX/bin/subtool` || 
       `test tools/subtool/subtool.c -nt $SYS_HOSTPREFIX/bin/subtool`; then
    gcc -o $SYS_HOSTPREFIX/bin/subtool tools/subtool/subtool.c 2> /dev/null
  fi
  ac_subst SYS_ORGTLD
  ac_subst SYS_ORGSLD
  ac_subst SYS_APPNAME
  ac_subst SYS_LOCASEAPPNAME
  ac_subst SYS_IOSCERT
  ac_subst SYS_IOSVERSION
  ac_subst SYS_IOSSDK
  ac_subst SYS_PLATFORM
  ac_subst SYS_HOSTPLATFORM
  ac_subst SYS_PREFIX
  ac_subst SYS_PREFIXROOT
  ac_subst SYS_HOSTPREFIX
  ac_subst SYS_GSC
  ac_subst SYS_CC
  ac_subst SYS_AR
  ac_subst SYS_RANLIB
  ac_subst SYS_STRIP
  ac_subst SYS_WINDRES
  ac_subst SYS_EXEFIX
  ac_subst SYS_APPFIX
  ac_subst SYS_APPVERSION
  ac_subst SYS_APPVERSIONCODE
  ac_subst SYS_ANDROIDAPI
  ac_subst ANDROIDSDK
  ac_subst SYS_BUILDHASH
  ac_subst SYS_BUILDEPOCH
  ac_subst SYS_PROFILE
  ac_subst SYS_VERBOSE
  ac_subst SYS_HOSTEXEFIX
  ac_subst SYS_OPENWRTTARGET
  ac_subst SYS_ARCH
  ac_output CONFIG.h $SYS_PREFIX/include/CONFIG.h
  name=$SYS_APPNAME
  here=`pwd`
  appsrcdir=`locatedir apps/$name`
  apptgtdir=$SYS_PREFIX/${SYS_APPNAME}${SYS_APPFIX}
  modules=
  if [ -f "$appsrcdir/MODULES" ]; then
    modules=`cat $appsrcdir/MODULES`
  fi
  plugins=
  if [ -f "$appsrcdir/PLUGINS" ]; then
    plugins=`cat $appsrcdir/PLUGINS`
  fi
  libraries=
  if [ -f "$appsrcdir/LIBRARIES" ]; then
    libraries=`cat $appsrcdir/LIBRARIES`
  fi
  setstate
}

make_loader()
{
  setstate LOADER
  locaseappname=`echo $SYS_APPNAME | tr A-Z a-z`
  here=`pwd`
  echo "==> creating $SYS_PLATFORM loader needed for $SYS_APPNAME.."
  if [ -s targets/$SYS_PLATFORM/build-binary ]; then
    . targets/$SYS_PLATFORM/build-binary
  else
    echo "ERROR: Don't know how to make the loader!"
    exit 1
  fi
  echo "=== $SYS_PREFIX/$SYS_APPNAME$SYS_APPFIX"
  setstate
}

###################################
# high level make functions

make_payload()
{
  setstate PAYLOAD
  coremodules=" syntax-case config eventloop ln_core ln_glcore "
  coresrcs=
  auxsrcs=
  for m in $modules; do
    if [ $m = "syntax-case" ]; then
      modsrc="$SYS_HOSTPREFIX/lib/syntax-case.scm"
    else
      modsrc=`locatefile modules/$m/$m.scm`
    fi
    if [ `string_contains "$coremodules" " $m "` = yes ]; then
      coresrcs="$coresrcs $modsrc"
    else
      auxsrcs="$auxsrcs $modsrc"
    fi
  done
  for p in $plugins; do
    plugsrc=`locatefile plugins/$p/$p.scm`
    auxsrcs="$auxsrcs $plugsrc"
  done
  # note: textures, fonts and strings can't go before glcore!
  srcs="$coresrcs $texture_srcs $font_srcs $string_srcs $auxsrcs $appsrcdir/main.scm"
  libs=
  for l in $libraries; do
    libname=`echo "$l!" | cut -f 1 -d "!"`
    excludes=`echo "$l!" | cut -f 2- -d "!"`
    excluded=`echo "$excludes" | grep $SYS_PLATFORM`
    if [ "X$excluded" = "X" ]; then
      libs="$libs $libname"
    fi
  done
  compile_payload $name "$srcs" "$libs"
  setstate
}

make_clean()
{
  echo "==> cleaning up build files.."
  rmifexists $SYS_PREFIX/lib/libpayload.a
# these files are platform independent
#  rmifexists $SYS_PREFIXROOT/build
  rmifexists $SYS_PREFIX/build
}

make_scrub()
{
  echo "==> removing entire build cache.."
  platforms=`ls -1 targets`
  for platform in $platforms; do
    rmifexists $SYS_PREFIXROOT/$platform
  done
  rmifexists $SYS_PREFIXROOT/build
}

make_install()
{
  setstate INSTALL
  if [ -s targets/$SYS_PLATFORM/install ]; then
    . targets/$SYS_PLATFORM/install
  else
    echo "==> no install setup for this platform ($SYS_PLATFORM)"
  fi
  setstate
}

update_packfile()
{
  setstate PACKTOOL
  embedfile=`locatefile apps/$SYS_APPNAME/EMBED silent`
  if [ ! "X$embedfile" = "X" ]; then
    echo "==> Updating packfile for $SYS_APPNAME.."
    here=`pwd`
    cd `locatedir apps/$SYS_APPNAME`
    $SYS_HOSTPREFIX/bin/packtool
    if [ -f embed.scm ]; then
      if `test "embed.scm" -nt "main.scm"`; then
        touch main.scm
      fi
    fi
    cd $here
    mainfile=`locatefile apps/$SYS_APPNAME/main.scm`
    if [ "X"`cat "$mainfile" | grep "(include \"embed.scm\")" | cut -c 1` = "X" ]; then
      echo "ERROR: $SYS_APPNAME/main.scm is missing include for embed.scm" 
      echo "Please add  (include \"embed.scm\")  to the top of it."
      exit 1
    fi
  fi
  setstate
}

make_executable()
{
  if [ ! "$SYS_MODULES" ]; then
    make_loader
  else
    echo "==> making standalone executable for $SYS_APPNAME.."
    veval "$SYS_CC -o $SYS_PREFIX/bin/$SYS_APPNAME$SYS_EXEFIX $SYS_PREFIX/lib/libpayload.a"
    assertfile $SYS_PREFIX/bin/$SYS_APPNAME$SYS_EXEFIX
  fi
}

make_package()
{
  setstate PACKAGE
  here=`pwd`
  echo "==> making package.."
  if [ -s targets/$SYS_PLATFORM/package ]; then
    . targets/$SYS_PLATFORM/package
  else
    echo "=== no packaging setup for this platform ($SYS_PLATFORM)"
  fi
  setstate
}

make_libraries()
{
  setstate LIBRARIES
  echo "==> creating libraries needed for $SYS_APPNAME.."
  libfile=`locatefile apps/$SYS_APPNAME/LIBRARIES` 
  assertfile $libfile
  libraries=
  if [ -f "$libfile" ]; then
    libraries=`cat $libfile`
  fi
  here=`pwd`
  for lib in $libraries; do
    libname=`echo "$lib!" | cut -f 1 -d "!"`
    excludes=`echo "$lib!" | cut -f 2- -d "!"`
    excluded=`echo "$excludes" | grep $SYS_PLATFORM`
    if [ "X$excluded" = "X" ]; then    
      libfile="$SYS_PREFIX/lib/$libname.a"
      libdir=`locatedir libraries/$libname`
      assertfile $libdir
      if [ `newerindir $libdir $libfile` = "yes" ]; then
        echo " => $libname.."
        cd $libdir
        ac_output build.sh
        eval "$SYS_ENV sh build.sh"
        rm build.sh
        cd $here
      fi
      explode_library $libname
    fi
  done
  setstate
}

make_tools()
{
  setstate TOOLS
  echo "==> creating tools needed for $SYS_APPNAME.."
  tools=`ls -1 tools`
  here=`pwd`
  for tool in $tools; do
    tooldir=tools/$tool
    assertfile $tooldir
    cd $tooldir
    ac_output build.sh
    veval "sh build.sh"
    asserterror $?
    rm build.sh
    cd $here
  done
  setstate
}

make_info()
{
  echo " => Application	: $SYS_APPNAME"
  echo " => Version	: $SYS_APPVERSION"
  echo " => Platform	: $SYS_PLATFORM"
  echo " => Org. ID	: $SYS_ORGSLD.$SYS_ORGTLD"
  exit 1
}

######################
# system sanity checks

make_xelatexcheck()
{
  vecho " => checking for working xelatex.."
  chkdir=check.xelatex
  mkdir -p $chkdir
  cd $chkdir
cat > check.tex << END
\batchmode
\documentclass{article}
\usepackage{fontspec}
\usepackage{geometry}
\begin{document}
check
\end{document}
END
  veval "xelatex check.tex"
  assertfile check.pdf "xelatex environment is not complete. Please install necessary XeTeX packages." 
  cd ..
  rm -rf $chkdir
}

make_libarycheck(){
  setstate LIBRARYCHECK
  echo "==> checking for required libraries.."
  if [ -s targets/$SYS_PLATFORM/check-libraries ]; then
    . targets/$SYS_PLATFORM/check-libraries
  fi
  setstate
}

make_toolcheck()
{
  setstate TOOLCHECK
  echo "==> checking for required tools.."
  # basic 
  asserttool grep wget zip tar sed tr cut tail head find
  # language 
  asserttool autoconf make gcc patch
  # graphics 
  asserttool gs convert xelatex ps2eps freetype-config
  # verify that xelatex works
  make_xelatexcheck
  # platform specific tools
  if [ -s targets/$SYS_PLATFORM/check-tools ]; then
    . targets/$SYS_PLATFORM/check-tools
  fi
  setstate
}

make_lntoolcheck()
{
  echo "==> checking for lambdanative tools.."
  if [ ! -x $SYS_HOSTPREFIX/bin/gsc ]; then
    if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
      echo " => not found, commence building lambdanative tools.."
      cp config.cache tmp.config.cache
      SYS_PATH=$SYS_PATH ./configure $SYS_APPNAME
      . ./config.cache
      rmifexists tmp.subst
      make_setup
      make_libraries
      make_tools 
      mv tmp.config.cache config.cache
      . ./config.cache
      rmifexists tmp.subst
      make_setup
      echo " => lambdanative tools build complete"
    fi
  fi
}

##############################

make_gcc()
{
  echo "==> building gcc compiler (this will take a while).."
  asserttool flex bison
  gcc_version="4.8.1"
  gcc_ball="gcc-${gcc_version}.tar.gz"
  gcc_prefix=$SYS_PREFIXROOT/gcc/$SYS_HOSTPLATFORM/gcc-${gcc_version}
  tgt=$SYS_PREFIXROOT/packages/$gcc_ball
  if [ ! -f "$tgt" ]; then  
    echo " => downloading $gcc_ball.."
    veval "wget ftp://ftp.gnu.org/gnu/gcc/gcc-${gcc_version}/$gcc_ball -O $tgt"
  fi
  assertfile $tgt
  oldpath=`pwd`
  newpath=`mktemp -d tmp.XXXXXX`
  cd $newpath
  tar -zxf $tgt
  cd *
  veval "./contrib/download_prerequisites"
  mkdir build
  cd build
  veval "../configure \
    --prefix=$gcc_prefix \
    --enable-languages=c \
    --disable-nls"
  veval "make -j 4 bootstrap"
  mkdir -p $gcc_prefix
  veval "make install"
  echo " => gcc compilation complete"
  cd $oldpath
  rm -rf $newpath
}

##############################

usage()
{
  echo "usage: make.sh <clean|tools|resources|libraries|payload|executable|all|install|package|info>"
  exit 0;
}

##############################
# main dispatcher

make_setup
case "$1" in
  clean|scrub|info|package|install)
  ;;
  *)
    make_toolcheck
    make_libarycheck
    make_lntoolcheck
  ;;
esac

# try to prevent a failed build from contaminating next make
# this has to be called after make_setup
resetstate

# check if framework version has changed since last configure
cfg_version=$SYS_VERSION
cur_version=`cat ./VERSION | sed '/^#/d'`
if [ "X$cfg_version" = "X" ]; then
  cfg_version=$cur_version
fi
if [ ! "X$cfg_version" = "X$cur_version" ]; then
  echo " ** NEW FRAMEWORK VERSION DETECTED - scrubbing cache"
  make_scrub
  rm config.cache
  echo " ** FRAMEWORK VERSION CHANGE - please rerun configure for the local host"
  exit 1
fi

case "$1" in
clean) 
  rm -rf tmp.?????? 2> /dev/null
  make_clean
;;
scrub)
  rm -rf tmp.?????? 2> /dev/null
  make_scrub
;;
tools)
  make_tools
;;
resources)
  make_artwork
  make_textures
  make_fonts
  make_strings
;;
libraries)
  make_libraries
;;
explode)
  explode_library $2
;;
payload)
  make_payload
;;
executable)
  make_executable
;;
all)
  rm -rf tmp.?????? 2> /dev/null
  make_libraries
  make_tools
if [ `is_gui_app` = "yes" ]; then
  make_artwork
  make_textures
  make_fonts
  make_strings
fi
  update_packfile
  make_payload
  make_executable
  make_package
;;
package)
  make_package
;;
install)
  make_install
;;
info)
  make_info
;;
gcc)
  make_gcc
;;
*)
  usage
;;
esac

if [ -f $evallog ]; then
  rm $evallog
fi

if [ -f $ac_cache ]; then
  rm $ac_cache
fi

# all is well if we got here, so clear the state cache
resetstate

#eof
