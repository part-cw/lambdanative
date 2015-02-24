# location of temporary lambanative state

SYS_TMPDIR="$HOME/.lambdanative"

if [ ! -d $SYS_TMPDIR ]; then
  mkdir -p $SYS_TMPDIR
fi

#eof
