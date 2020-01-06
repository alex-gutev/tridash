SUBDIRS = 1a-hello-node \
	1b-inline-nodes \
	1c-two-way-bindings \
	2a-functor-nodes \
	2b-binding-functors \
	3a-maximum \
	3b-sum-limit \
	3c-color \
	4a-add-function \
	4b-optional-arguments \
	4c-optional-argument-node-value \
	4d-recursive-meta-nodes \
	4e-tail-recursion \
	4f-nested-meta-nodes \
	4g-local-nodes \
	4h-outer-nodes \
	4i-simple-meter \
	5a-subnodes \
	6a-handling-failures \
	6b-cleaning-up \
	6c-initial-values \
	6d-exercise \
	7a-failure-types \
	7b-positive-numbers \
	7c-proper-failure-type \
	8a-target-node-transform \
	9a-contexts \
	9b-explicit-contexts \
	9c-explicit-contexts-failure-type \
	9d-improved-error-message \
	9e-concise-syntax \
	10a-counter \
	10b-state-transition \
	10c-alternating-buttons \
	11a-higher-order-meta-nodes \
	11b-message-list \
	12a-multiple-source-files \
	12b-modularity \
	12c-infix-add-operator

ALLFILES = $(SUBDIRS:%=%.all)
CLEANFILES = $(SUBDIRS:%=%.clean)

all: $(ALLFILES)

clean: $(CLEANFILES)

%.all: %
	$(MAKE) -C $< all

%.clean: %
	$(MAKE) -C $< clean

.PHONY: all clean
