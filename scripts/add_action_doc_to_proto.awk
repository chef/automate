BEGIN {
  FS = "\""
}

END {
  if (collecting) {
    for (i = 1; i < collecting; i++) print buffer[i]
  }
}

collecting && $0 ~ /chef.automate.api.iam.policy\).action *= *".*"/ {
  print ""
  print "  Authorization Action:"
  print ""
  print "  ```"
  print "  "$2
  print "  ```"

  for (i = 1; i < collecting; i++) print buffer[i]
  collecting = 0
}

/^ *\*\// {
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