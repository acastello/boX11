ALL= libboX11.dll.so main.exe libHSBoX11.so loader.exe

all: $(ALL)

libboX11.dll.so: boX11.c boX11.h
	winegcc -shared -fPIC boX11.c -o $@
	
main.exe: main.c
	winegcc $^ ./libboX11.dll.so -o $@

libHSBoX11.so: BoX11.hs
	ghc -shared -fPIC -dynamic $^ -o $@

loader.exe: loader.c
	winegcc -ldl $^ -o $@

test: $(ALL)
	./main.exe
