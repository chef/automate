# IAM V2 Data Migrations

In this service and `teams-service`, we've added `storage/postgres/datamigrations`
that contain IAM V2 specific data migrations. These migrations only run when
V2 mode is enabled. They run both when the service starts in V2 mode as well
as when `chef-automate iam upgrade-to-v2` is run. If the service is not in V2
mode, these migrations will not be applied. For that reason, **THESE MIGRATIONS
MUST NOT CONTAIN SCHEMA CHANGES!** Otherwise, we will end up with a split schema
between V1 and V2 and be in a bad place for a user upgrading to V2.

## AuthZ Service

In this service, since all the V2 data is dropped on `reset-to-v1`, we've updated
that reset to also reset the `data_migrations` schema back to its initial state.
This is done so that the V2 data migrations are applied every time on upgrade, which
is necessary since the data gets reset every time.

## Teams Service

When `upgrade-to-v2` is run, it creates the teams a single time. `reset-to-v1` does not
clean up those teams. Therefore, the `teams-service` migrations are only applied once.
