ALL= libboX11.so main.exe libHSBoX11.so loader.exe main.so mainw.exe

all: $(ALL)

libboX11.dll.so: boX11.c boX11.h
	winegcc -shared -fPIC boX11.c /usr/lib/wine/user32.dll.so -o libboX11.dll.so

libboX11.so: libboX11.dll.so
	cp libboX11.dll.so libboX11.so
	
main.exe: main.c
	winegcc $^ ./libboX11.dll.so -o $@

libHSBoX11.so: BoX11.hs
	ghc -shared -fPIC -dynamic $^ -o $@

loader.exe: loader.c
	winegcc -ldl $^ -o $@

main.so: boX11.c BoX11.hs Main.hs export.hs
	ghc -shared -fPIC -dynamic export.hs -optl-lboX11 /usr/lib/ghc-8.0.1/rts/libHSrts-ghc8.0.1.so -o $@

mainw.exe: mainw.c boX11.c boX11.h
	winegcc -shared -fPIC mainw.c -o mainw.exe -lboX11

test: $(ALL)
	./main.exe
