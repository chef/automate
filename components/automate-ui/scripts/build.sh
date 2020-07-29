#!/bin/bash
# The build machine is using sh; need to use bash to be able to use PIPESTATUS.

# Purpose:
# Treat warnings as errors for build commands that do not provide innate support to do that.

CMD_STRING=$1
# Examples:
#    "ng lint"
#    "ng lint | grep -v 'deprecated'"

WARNING_LABEL=$2
# Examples:
#    "WARN:"         (as `ng test` uses)
#    "WARNING:"      (as `ng lint` uses)

# Setup an unused stream to redirect to stdout in the next command.
exec 5>&1
# We want to capture the target command's output to reuse it (rather than have to re-run the command),
# so sending to an unused stream and redirecting that to stdout (stream 1) where it can be captured into the `out` variable.
# In order to check the exit code of the target command itself, need to use PIPESTATUS
# to grab the exit code of the first pipe component inside CMD_STRING rather than the final one.
out=$(eval "$CMD_STRING | tee /dev/fd/5; exit \${PIPESTATUS[0]}")

# If the unit test run reported non-zero--an actual unit test error--then exit with non-zero return code.
# shellcheck disable=SC2181
if [ $? -ne 0 ]; then
  exit 1
fi

# If no errors, next check for any warnings.
grep -q "$WARNING_LABEL" <<< "$out"

# Return a non-zero exit code if any warnings present.
# $? will be 0 if warnings were found, so `test` inverts that result.
test $? -ne 0
