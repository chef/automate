curl --header "Authorization: token $GITHUB_TOKEN" \
     --header 'Accept: application/vnd.github.v3.raw' \
     --remote-name \
     --location 'https://raw.githubusercontent.com/habitat-sh/habitat/master/components/sup-protocol/protocols/types.proto'

mv event.proto api/habitat/
