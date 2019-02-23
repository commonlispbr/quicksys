LISP_FLAGS = --noinform --disable-debugger --load
LISP := sbcl $(LISP_FLAGS)
IMAGE = commonlispbr/emacs:sbcl

check:
	@$(LISP) run-tests.lisp

ci-check:
	@docker run -t --entrypoint=/usr/bin/sbcl \
			   -v $(shell pwd):/workspace \
			   --security-opt seccomp=unconfined \
			   $(IMAGE) \
		       $(LISP_FLAGS) \
		       run-tests.lisp
	@docker run --entrypoint=/bin/rm -t \
	 	   	   -v $(shell pwd):/workspace \
               $(IMAGE) \
		       -rf system-index.txt

.PHONY: check
