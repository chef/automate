#!/bin/bash

readme=${1:-needs README.md path as argument}
echo -e '## SQL migrations\n' > "$readme"
for file in "$(dirname "$readme")"/*.up.sql; do
  f=$(basename "$file")
  echo "- [\`$f\`]($f)" >> "$readme"
done
if ! git diff --exit-code "$readme"; then
 echo "$readme index not committed; please commit $readme"
 exit 1
fi
