all:
	-@cd src; \
	./compile; \
	rm ../*.native 2>/dev/null; \
	mv *.native ../. 2>/dev/null

test: all
	-@./runTests
clean:
	-@rm -rf src/_build 2>/dev/null; \
	rm *.native 2>/dev/null; \
	echo "Project is now clean"
