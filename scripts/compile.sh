# compilation scripts to convert sourcecode to object files

language_extensions="scm c lua" 
link_stage_languages="scm lua"

compile_supported_language()
{
  language=`echo "$1" | sed 's/.*\.//'`
  match=`echo " $language_extensions " | grep " $language "`
  if [ "X$match" = "X" ]; then
    echo ""
  else
    echo "$language"
  fi
}

gambit_link_stage()
{
  language=$1
  match=`echo " $link_stage_languages " | grep " $language "`
  if [ "X$match" = "X" ]; then
    echo "no"
  else
    echo "yes"
  fi
}

# --------------
# scheme

compile_scm()
{
  if [ $SYS_MODE = "debug" ]; then
    opts="(declare (block)(not safe)(standard-bindings)(extended-bindings)(debug)(debug-location))"
    optc="-g -Wall -Wno-unused-variable -Wno-unused-label -Wno-unused-parameter"
  else
    opts="(declare (block)(not safe)(standard-bindings)(extended-bindings))"
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
  veval "$SYS_ENV $SYS_CC $defs -c -o $otgt $ctgt -I$SYS_PREFIX/include -I$SYS_PREFIX/include/freetype2 -I$path"
  assertfile "$otgt"
  cd $here
}

# ---------------
# C

compile_c()
{
  if [ $SYS_MODE = "debug" ]; then
    optc="-g -Wall -Wno-unused-variable -Wno-unused-label -Wno-unused-parameter"
  else
    optc="-O2 -Wall -Wno-unused-variable -Wno-unused-label -Wno-unused-parameter"
  fi
  src="$1"
  ctgt="$2"
  otgt="$3"
  defs="$4"
  path=`dirname $src`
  here=`pwd`
  cd "$path"
  rmifexists "$ctgt"
  echo " => $src.."
  cp "$src" "$ctgt"
  rmifexists "$otgt"
  veval "$SYS_ENV $SYS_CC $defs -c -o $otgt $ctgt -I$SYS_PREFIX/include -I$SYS_PREFIX/include/freetype2 -I$path"
  assertfile "$otgt"
  cd $here
}

# ------------------
# lua

# embed compressed source to eval at runtime
compile_lua()
{
  src=${1}.scm
  $SYS_HOSTPREFIX/bin/lngtool "string->lua" "$1" > $src
  compile_scm "$src" "$2" "$3" "$4"
  rmifexists "$src"
}

#eof
