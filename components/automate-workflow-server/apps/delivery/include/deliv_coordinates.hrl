%% Coordinates refer to a collection of values that together are used to
%% describe a unique object.

%% Project
%% A project is scoped to a single organization, which in turn is scoped to a single
%% enterprise. We can describe a specific project using the names of the enterprise,
%% organization and project name.
-record(proj_coordinates, {
          ent_name :: binary(),
          org_name :: binary(),
          proj_name :: binary()
         }).
