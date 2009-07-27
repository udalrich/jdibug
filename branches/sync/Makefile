VERSION=0.2
PROJECT=jdibug

DIST_EL_FILES=tree-mode.el \
	          elog.el \
			  jdwp.el \
	          jdi.el \
		 	  jdibug.el 

DIST_DOC_FILES=jdibug.texi jdibug.html

all: doc dist

dist: 
	mkdir ${PROJECT}-${VERSION}
	cp ${DIST_EL_FILES} ${DIST_DOC_FILES} ${PROJECT}-${VERSION}
	tar jcvf ${PROJECT}-${VERSION}.tar.bz2 ${PROJECT}-${VERSION}
	rm -rf ${PROJECT}-${VERSION}

doc: 
	texi2html jdibug.texi