BEGIN {
  FS = "\""
}

END {
  if (collecting) emptyBuffer()
}

collecting && (($0 ~ /chef.automate.api.iam.policy\).action[[:space:]]*=[[:space:]]*".*"/) || (optionFound && $0 ~ /^[[:space:]]*action:[[:space:]]*".*"/)) {
  # add annotation...
  whitespace = useTab ? "\t" : "  "
  print ""
  print whitespace"Authorization Action:"
  print ""
  print whitespace"```"
  print whitespace$2
  print whitespace"```"
  emptyBuffer()

  # reset for next pass
  collecting = 0
  optionFound = 0
}

# Handle `option` across multiple lines (e.g. reporting.proto)
collecting && $0 ~ /chef.automate.api.iam.policy\)[[:space:]]*=/ {
  optionFound = 1
}

# A closing comment is the trigger to start collecting
/^[[:space:]]*\*\// {
  if (collecting) emptyBuffer()
  useTab = $0 ~ /^\t/
  collecting = 1
}

collecting {
  buffer[collecting++] = $0
}

!collecting {
  print
}

function emptyBuffer(){
  for (i = 1; i < collecting; i++) print buffer[i]
}
