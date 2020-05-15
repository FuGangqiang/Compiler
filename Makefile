default: test

.DEFAULT:
	cd src && $(MAKE) $@
