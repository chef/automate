#!/bin/bash

echo -n "" > event.proto

echo "// Local Copy of github.com/habitat-sh/habitat/master/components/sup/protocols/event.proto" >> event.proto
echo "// to update, use update-event-proto.sh" >> event.proto
echo "// DO NOT EDIT" >> event.proto

echo "// local modification: must specify proto version at the very top" >> event.proto
echo "syntax = \"proto3\";" >> event.proto
echo "// local modification: set go package to something we can using in automate" >> event.proto
echo "option go_package = \"github.com/chef/automate/api/external/habitat\";" >> event.proto
# redundant `syntax` declarations in the proto file would be an error so we remove it from the upstream:
curl https://raw.githubusercontent.com/habitat-sh/habitat/master/components/sup/protocols/event.proto | grep -v 'syntax = "proto3"\;' >> event.proto

