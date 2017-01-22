TESTS=$(basename $(wildcard test*.p))

.PHONY: check $(TESTS)

check: $(TESTS)

$(TESTS): %: %.p
	../analyze $< > $@.log && diff $@.log $@.answer

clean:
	rm -f *.log *~ 
