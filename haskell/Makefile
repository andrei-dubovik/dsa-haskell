targets := \
  refactor0-first-steps \
  refactor1-historical-data \
  refactor2-complete-model \
  refactor3-memoization

.PHONY: all clean

all: $(targets)

%:: %.hs
	ghc -dynamic -O2 -no-keep-hi-files -no-keep-o-files $<

clean:
	rm -f $(targets)
