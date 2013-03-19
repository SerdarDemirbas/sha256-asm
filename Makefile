sha256: sha256.o
	ld -melf_i386 -o $@ $<

sha256.o: sha256.asm
	as --32 -g3 -o $@ $<

clean:
	rm -f sha256.o sha256

.PHONY: clean
