# External ES Test

The purpose of this compose repo is to build a test case for customers that
are using external elasticsearch by configuring automate-elasticsearch to be
a non-data/non-master node that peers with an existing cluster.

## Requirements
* The test case should be modeled to as close an approximation as we can make
  to customer deployments
* Backup and restore in this configuration must continue to work

## Challenges
This test requires two machines (or containers) to approximate the configuration
we're trying to test. The existing testing framework only supports single node
tests. We do have prior art of using docker-compose to run test cases, but they
predate the test framework and don't integrate well with the build job.

Options are:
  * Update the test framework to support deploying and testing using docker-compose
    as a driver. Upsides are that it fits nicely into existing things. Downside
    is that it's the most work.
  * Write the test as a nightly. Downside is that it only runs after merge and once
    a day. Upside is that it's the easiest.
  * Write a hybrid test: a shell script that uses the build job artifacts but
    bypasses the test framework and deploys and tests outside of it.

## Configuration

### A2 Node
The test nodes running A2 and Elasticsearch should have a shared storage directory.
On the A2 node, that storage directory will be used as both the backups directory
and be included in the elasticsearch repo.

The automate-elasticsearch service should be configured in non-data/non-master node
and should peer with the Elasticsearch node and join it's cluster.

### Elasticsearch Node
On the elasticsearch node, only the elasticsearch repo will need to be configured
to use the shared storage directory as the repo path.
