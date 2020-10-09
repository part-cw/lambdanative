
veval()
{
  if [ $SYS_VERBOSE ]; then
    echo "$1"
    eval $1
    veval_result=$?
  else
    echo "$1" > $evallog
    eval $1 >> $evallog 2>&1
    veval_result=$?
  fi
  return $veval_result
}

vecho()
{
  if [ $SYS_VERBOSE ]; then
    echo "$@"
  fi
}

