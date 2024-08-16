st.exe: ast.c
	gcc -Werror -Wswitch -Wimplicit-fallthrough ast.c -o ast.exe

clean:
	rm -f *.o *.exe *.s a.out

test.s: ast.exe
	./ast.exe > test.s

test.exe: test.s
	gcc test.s -masm=intel -o test.exe

.PHONY: test
test: test.exe
	./test.exe