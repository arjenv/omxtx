CC=gcc
LD=gcc

CFLAGS=-Wall -Wno-format -g -I/opt/vc/include/IL -I/opt/vc/include -I/opt/vc/include/interface/vcos/pthreads -I/opt/vc/src/hello_pi/libs/ilclient -DSTANDALONE -D__STDC_CONSTANT_MACROS -D__STDC_LIMIT_MACROS -DTARGET_POSIX -D_LINUX -D_REENTRANT -D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64 -U_FORTIFY_SOURCE -DHAVE_LIBOPENMAX=2 -DOMX -DOMX_SKIP64BIT -ftree-vectorize -pipe -DUSE_EXTERNAL_OMX -DHAVE_LIBBCM_HOST -DUSE_EXTERNAL_LIBBCM_HOST -DUSE_VCHIQ_ARM
LDFLAGS= -L/opt/vc/lib/ -L/opt/vc/src/hello_pi/libs/ilclient/ # -Xlinker --verbose 
LIBS=-lavformat -lopenmaxil -lbcm_host -lvcos -lvchiq_arm -lpthread -lrt -lilclient -lGLESv2 -lEGL
OFILES=omxtxres.o

.PHONY: all clean install dist

all: omxtxres

.c.o:
	$(CC) $(CFLAGS) -c $<

omxtxres: omxtxres.o
	$(CC) $(LDFLAGS) -o omxtxres $(OFILES) $(LIBS)

remuxer: remuxer.o
	$(CC) $(LDFLAGS) $(LIBS) -o remuxer remuxer.o

clean:
	rm -f *.o omxtxres
	rm -rf dist

dist: clean
	mkdir dist
	cp omxtxres.c Makefile dist
	FILE=omxtxres-`date +%Y%m%dT%H%M%S`.tar.bz2 && tar cvf - --exclude='.*.sw[ponml]' dist | bzip2 > $$FILE && echo && echo $$FILE
