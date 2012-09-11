PSL=~/cs173/pt/debian-dist/bin/assignment1-debian
PSL=../osx-dist/bin/assignment1-osx
TESTS=$(shell find . -type f -name '*.psl')
EXPECTED=$(addsuffix .expected,$(TESTS))

%.psl.expected: %.psl
	$(PSL) --interp < "$<" > "$@" 2> "$(@:.psl.expected=.psl.error)"

.PHONY: tests clean errs

all: tests
tests: $(EXPECTED)
	-$(PSL) --brief .
clean:
	-find . -name '*.psl.error' -delete -o -name '*.psl.expected' -delete
errs:
	@-find * -name '*.psl.error' -a -! -empty

long:
	-$(PSL) --test-interp .
