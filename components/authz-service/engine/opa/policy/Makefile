.PHONY: opa-cmd
HAVE_OPA_CMD:=$(shell command -v opa 2>/dev/null)
opa-cmd:
ifndef HAVE_OPA_CMD
	go get -u github.com/open-policy-agent/opa
endif

static: opa-cmd  # if there's no output, this will not fail; if there is (NR!=0), fail
	opa fmt -d *.rego | awk '{ print } END { if (NR!=0) { print "run make fix-format to fix this"; exit 1 } }'

fix-format: opa-cmd
	opa fmt -w *.rego

unit: opa-cmd
	opa test -v .
