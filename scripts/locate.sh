# locate files and directories

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

#eof
