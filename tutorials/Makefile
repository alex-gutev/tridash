SUBDIRS = 1a-hello-node \
	1b-hello-node \
	2a-budget-app \
	2b-inline-functors \
	3a-conditional-bindings \
	4a-functions \
	5a-error-handling \
	5b-initial-values \
	6a-failures \
	7a-outer-nodes \
	8a-modules \
	9a-failure-types

ALLFILES = $(SUBDIRS:%=%.all)
CLEANFILES = $(SUBDIRS:%=%.clean)

all: $(ALLFILES)

clean: $(CLEANFILES)

%.all: %
	$(MAKE) -C $< all

%.clean: %
	$(MAKE) -C $< clean

.PHONY: all clean
