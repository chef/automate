Purpose: In order to complete A1 to A2 upgrades, each of the teams responsible for storing data in the Automate infrastructure is responsible for writing migrations to move the data from A1 to A2. To support those efforts, this document describes the end-to-end A1->A2 data migration process from the point of view of the deployment service.

### Design Constraints:
These design constraints are based on conversations with our field teams and reflect their experience with the realities of our customers IT processes.

* in-place upgrade is a must-have for some customers who have to deal with (slow) enterprise provisioning processes. Other strategies, such as upgrade from backup will be required by other customers (OpsWorks in particular) so we're making decisions that will accommodate alternative upgrade strategies but we're delivering those later
* customers won't have enough free disk to handle two copies of the compliance and converge data stored in elasticsearch

### Inputs to services:
1. A running A2 postgres loaded with A1 postgres data.
1. A running A2 elasticsearch loaded with A1 elasticsearch data.
1. Creds needed to connect to A2 postgres and elasticsearch. (If any.)

### The workflow we envision:
1. Deployment service loads A1 postgres and elasticsearch data into A2 postgres and elasticsearch.
1. Deployment service tells services to start, providing locations and creds for A2 postgres and elasticsearch.
1. Each service starts up:
   1. Service detects whether it needs to do a Stage 1 migration. Stage 1 migrations are those that are expected to be fast, synchronous, and idempotent; this can and should include any necessary schema migrations. Stage 1 migrations could happen in the `init` phase if necessary. In the (future) multinode case where more than one instance of the service is running, the deployment service will supply config to elect an instance of the service being deployed to be the migration runner.
      1. During Stage 1 migrations, services should respond to health checks with a Critical response. Exit code 2.
      1. Once Stage 1 migrations are complete, the service should be able to start normally. It should be able to function and receive data at this point.
   1. Once the service has successfully completed Stage 1 migration, it coordinates Stage 2 migrations. (Stage 2 = large/slow, e.g. elasticsearch reindexing)
      1. Service will have exposed an API endpoint for migration and upgrade status.
      1. Stage 2 migrations will be run in the background.
      1. Stage 2 migrations will migrate data that is high-value to the user first (e.g. reverse chronological order).
      1. If a Stage 2 migration fails, service provides the data on which the migration failed in a form that the user could send on Chef Support for further debugging/improvement.

### What the deployment service needs from other services
1. Ability to start up with A1 data.
1. Ability to detect whether it needs to do a Stage 1 and/or Stage 2 migration.
1. An API endpoint for migration and upgrade status. Status of failed migrations should include the data on which the migration failed.
