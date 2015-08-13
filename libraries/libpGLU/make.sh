# portable GLU library (only tesselation and matrix manipulation)
# this is a patched version of https://code.google.com/p/iphone-glu

PKGURL=https://iphone-glu.googlecode.com/files/iGLU-1.0.0.tar.gz
PKGHASH=73215cc384876444e4937ba11937f998a8894bff

package_download $PKGURL $PKGHASH

package_patch

pGLU_tgt=libpGLU.a

pGLU_srcs="libutil/error.c libutil/glue.c libutil/project.c libutil/registry.c libtess/dict.c libtess/geom.c libtess/memalloc.c libtess/mesh.c libtess/normal.c libtess/priorityq.c libtess/render.c libtess/sweep.c libtess/tess.c libtess/tessmono.c"

rm -f *.o 2> /dev/null
for src in $pGLU_srcs; do
  veval "$SYS_CC -c ${src} -I./include $SYS_CFLAGS -DNDEBUG -DLIBRARYBUILD"
done

rm -f $pGLU_tgt 2> /dev/null
veval "$SYS_AR -ru ${pGLU_tgt} *.o"
veval "$SYS_RANLIB ${pGLU_tgt}"
assertfile ${pGLU_tgt}

mkdir -p ${SYS_PREFIX}/include
cp ./include/glu.h ${SYS_PREFIX}/include/pGLU.h
mkdir -p ${SYS_PREFIX}/lib
cp ${pGLU_tgt} ${SYS_PREFIX}/lib

#eof
