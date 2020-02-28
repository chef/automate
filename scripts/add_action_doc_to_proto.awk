BEGIN {
  # Changing field separator to be able to easily grab action as 2nd field ($2 below)
  FS = "\""
}

END {
  if (collecting) replayBuffer()
}

/Authorization Action:/ {
  previouslyAnnotated = 1
}

# The V2 action triggers documenting the action then backfilling what has been cached.
collecting && (($0 ~ /chef.automate.api.iam.policy\).action[[:space:]]*=[[:space:]]*".*"/) || (optionFound && $0 ~ /^[[:space:]]*action:[[:space:]]*".*"/)) {
  # add annotation...
  whitespace = useTab ? "\t" : "  "
  print ""
  print whitespace"Authorization Action:"
  print whitespace"```"
  print whitespace$2
  print whitespace"```"

  # ... and now backfill
  replayBuffer()

  # reset for next pass
  collecting = 0
  optionFound = 0
}

# Handle `option` across multiple lines (e.g. reporting.proto), accommodating different developer styles
collecting && $0 ~ /chef.automate.api.iam.policy\)[[:space:]]*=/ {
  optionFound = 1
}

# A closing comment is the trigger to start collecting
/^[[:space:]]*\*\// {
  if (collecting) replayBuffer()
  useTab = $0 ~ /^\t/ # allows matching different developer styles
  if (!previouslyAnnotated) collecting = 1 # Only move to collecting state if not done previously
  previouslyAnnotated = 0
}

collecting {
  buffer[collecting++] = $0
}

!collecting {
  print
}

function replayBuffer(){
  for (i = 1; i < collecting; i++) print buffer[i]
}

# Define shell function to run this awk script for one file, sitting in automate root directory:
#   doc_action() { awk -f ./scripts/add_action_doc_to_proto.awk $1 > data.tmp && mv data.tmp $1 ; }
#   export -f doc_action
#
# Run for all proto files that have both endpoints and doc-comments:
#   grep --include=\*.proto -rwl rpc . | xargs grep -l '\*\/' | xargs -t -n1 -P1 bash -c 'doc_action "$@"' _
#
# Next, need to update *.pb.go, *_swagger.json, and *.pb.swagger.go (in hab studio):
#   with `compile_go_protobuf_component automate-gateway` or `compile_go_protobuf_component api`
#
# Finally, update *.swagger.json in automate-chef-io/data/docs/api_chef_automate (in automate-chef-io dir):
#    make sync_swagger_files
