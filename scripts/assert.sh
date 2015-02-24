
assert()
{
  echo "** ERROR: $1"
  exit 1
}

assertfile()
{
  if [ ! -e "$1" ]; then
    if [ -f "$evallog" ]; then
      cat "$evallog" | sed '/^$/d'
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

havetool()
{
  havetool_result=yes
  for tool in $@; do
    tool_location=`which $tool 2> /dev/null`
    if [ "X$tool_location" = "X" ]; then
      havetool_result=no
    fi
  done
  echo $havetool_result
}
  
asserttool()
{
  for tool in $@; do
    tool_location=`which $tool 2> /dev/null`
    if [ "X$tool_location" = "X" ]; then
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

#eof
