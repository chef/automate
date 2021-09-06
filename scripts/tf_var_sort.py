#!/usr/bin/env python
# sort terraform variables

import sys
import re

# this regex matches terraform variable definitions
# we capture the variable name so we can sort on it
pattern = r'(variable\s+")([^"]+)("\s+{[^{]*})'


def process(content):
    # sort the content (a list of tuples) on the second item of the tuple
    # (which is the variable name)
    matches = sorted(re.findall(pattern, content), key=lambda x: x[1])

    # iterate over the sorted list and output them
    for match in matches:
        print(''.join(map(str, match)))

        # don't print the newline on the last item
        if match != matches[-1]:
            print('')


# check if we're reading from stdin
if not sys.stdin.isatty():
    stdin = sys.stdin.read()
    if stdin:
        process(stdin)

# process any filenames on the command line
for filename in sys.argv[1:]:
    with open(filename) as f:
        process(f.read())
