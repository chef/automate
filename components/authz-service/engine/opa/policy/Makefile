static: # if there's no output, this will not fail; if there is (NR!=0), fail
	opa fmt -d *.rego | awk '{ print } END { if (NR!=0) { print "run make fix-format to fix this"; exit 1 } }'

fix-format:
	opa fmt -w *.rego

unit:
	opa test -v .
