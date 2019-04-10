-module(deliv_enterprise_default_search).

-include("deliv_types.hrl").

-compile({parse_transform, sqerl_gobot}).

-export([
         fetch/1,
         upsert/2,
         delete/1
        ]).

%% sqerl callbacks
-export([
        '#insert_fields'/0,
        '#update_fields'/0,
        '#statements'/0,
        '#table_name'/0
       ]).

'#insert_fields'() -> [ent_id, search].
'#update_fields'() -> [id, ent_id, search].
'#table_name'() -> "enterprise_default_searches".

'#statements'() ->
    [
     default,
     {fetch_by_ent_id, sqerl_rec:gen_fetch(?MODULE, ent_id)},
     {delete_by_ent_id, sqerl_rec:gen_delete(?MODULE, ent_id)}
    ].

insert(EntId, Search) ->
    deliv_db:insert(#deliv_enterprise_default_search{ent_id = EntId, search = Search}).

fetch(EntId) ->
    deliv_db:qfetch(?MODULE, fetch_by_ent_id, [EntId]).

upsert(EntId,Search) ->
    case fetch(EntId) of
      [] ->
          insert(EntId, Search);
      [DefaultSearch] ->
          deliv_db:update(DefaultSearch#deliv_enterprise_default_search{search = Search})
    end.

delete(EntId) ->
    deliv_db:delete(fetch(EntId)).
