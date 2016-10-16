ALL= libboX11.so main.exe boxlaunch.exe mainw.exe

all: $(ALL)

libboX11.dll.so: boX11.c boX11.h
	# winegcc -shared -fPIC boX11.c /usr/lib/wine/user32.dll.so -o libboX11.dll.so
	winegcc -shared -fPIC boX11.c -o libboX11.dll.so

libboX11.so: libboX11.dll.so
	cp libboX11.dll.so libboX11.so
	
main.exe: main.c
	winegcc $^ ./libboX11.dll.so -o $@

# libHSBoX11.so: BoX11.hs
	# ghc -shared -fPIC -dynamic -threaded $^ -o $@

boxlaunch.exe: boxlaunch.c
	winegcc -ldl -Wl,-rpath=. $^ -o $@

testbox.exe: testbox.c libboX11.so
	winegcc testbox.c -o testbox.exe -L. -lboX11

# main.so: boX11.c BoX11.hs Main.hs export.hs
	# ghc -shared -fPIC -dynamic -threaded export.hs -lboX11 /usr/lib/ghc-8.0.1/rts/libHSrts-ghc8.0.1.so -o $@

mainw.exe: mainw.c boX11.c boX11.h
	winegcc mainw.c -o mainw.exe -lboX11 -L.

test: $(ALL)
	./main.exe

clean:
	rm $(wildcard *.o *.hi)

force:
	@echo $(wildcard *.o *.hi)

install:
	ln -s `readlink -f libboX11.dll.so` /usr/lib/libboX11.so
