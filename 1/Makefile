TESTS=$(basename $(wildcard test*.p))

.PHONY: check $(TESTS)

check: $(TESTS)

$(TESTS): %: %.p
	../int $< `cat $@.input` > $@.log && diff $@.log $@.answer

clean:
	rm -f *.log *~ 
