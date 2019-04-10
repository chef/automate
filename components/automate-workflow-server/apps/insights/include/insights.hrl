-include_lib("delivery/include/deliv_types.hrl").

-record(insights_event, {
    source :: atom(),
    source_fqdn :: binary(),
    type :: atom(),
    action :: atom(),
    ejson :: json()
}).
