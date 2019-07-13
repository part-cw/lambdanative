# compilation scripts to convert sourcecode to object files

languages=

compile_init()
{
  languages=
  compile_init_dirs=`echo "$SYS_PATH" | tr ':' '\n'`
  # vecho "searching supported languages in $compile_init_dirs"
  for compile_init_dir in $compile_init_dirs; do
    if [ -d $compile_init_dir/languages ]; then
      compile_init_lngs=`ls -1 $compile_init_dir/languages/*.sh`
      for compile_init_lng in $compile_init_lngs; do 
        compile_lng=`basename $compile_init_lng | sed 's/\.sh$//'`
        if [ `string_contains "$languages" "$compile_lng"` = no ]; then
          vecho "loading support for $compile_lng.."
          . $compile_init_lng
          languages="$languages $compile_lng"
        fi
      done
    fi
  done
  vecho "supported languages = $languages"
}

#eof
