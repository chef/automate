%% TODO: are those necessary??
-record(enterprise, {id :: binary(),
                     name :: binary()}).

-record(organization, {id :: binary(),
                       enterprise_id :: binary(),
                       name :: binary()}).
