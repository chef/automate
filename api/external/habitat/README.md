# Habitat Protobufs

Files in this directory are synced in from the habitat project. In the
synchronization process we make the modifications necessary to compile the
protos to golang code we can use in Automate. Aside from that, the copy here
should be verbatim.

The files here should never be edited directly, except under exceptional
circumstances. To make changes, modify the script that synchronizes from the
upstream.

## How to Update:

From your host system or the hab studio, cd to this directory, then run
`./update-event-proto.sh`. In the studio you may need to install curl first,
that can be done with `install_if_missing core/curl curl`. After that you can
compile the protos as normal, e.g., with `compile_all_protobuf_components`.
