BEGIN {
  FS = "\""
}

END {
  if (collecting) {
    for (i = 1; i < collecting; i++) print buffer[i]
  }
}

collecting && (($0 ~ /chef.automate.api.iam.policy\).action[[:space:]]*=[[:space:]]*".*"/) || (optionFound && $0 ~ /^[[:space:]]*action:[[:space:]]*".*"/)) {
  print ""
  print "  Authorization Action:"
  print ""
  print "  ```"
  print "  "$2
  print "  ```"

  for (i = 1; i < collecting; i++) print buffer[i]
  collecting = 0
  optionFound = 0
}

collecting && $0 ~ /chef.automate.api.iam.policy\)[[:space:]]*=/ {
  print "found it" > "/dev/stderr"
  optionFound = 1
}

/^[[:space:]]*\*\// {
  if (collecting) {
    for (i = 1; i < collecting; i++) print buffer[i]
  }
  collecting = 1
}

collecting {
  buffer[collecting++] = $0
}

!collecting {
  print
}

{
  lastline = $0
}