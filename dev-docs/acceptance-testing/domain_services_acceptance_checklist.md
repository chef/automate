# domain services acceptance checklist

_note: create, edit, and delete of credential, manual node, profile, and scan job are completed in cypress testing_

## where to go and what to expect
go to: https://a2-local-inplace-upgrade-acceptance.cd.chef.co/dashboards/event-feed 
or https://a2-perf-test-single-local-inplace-upgrade-acceptance.cd.chef.co/dashboards/event-feed 

dashboards: (event feed) empty\
applications page: some services data loaded\
infrastructure: (client runs) some older missing nodes, (chef-servers) a chef server and org with some data\
compliance: (reports) a few nodes reporting in, (scan jobs) a few old scan jobs, (profiles) a few installed profiles\
settings: (node integrations) a few integrations\ already created, (node credentials) a few credentials already created

## what to test

### ensure latest audit cookbook + chef-client is compatible with this automate version
1) Generate a token:

    * Either ssh to acceptance and get token:

    ```
    ssh your-ad-user@a2-local-inplace-upgrade-acceptance.cd.chef.co
    sudo chef-automate iam token create admin-token-1 --admin
    ```

    * Or [Use the Automate UI](https://github.com/chef/automate/blob/master/components/compliance-service/smokin/README.md#generating-a-token)

2) run test-kitchen with latest audit cookbook + chef infra

`cd your-automate-repo/components/compliance-service/smokin && kitchen destroy && COLLECTOR_URL='https://a2-local-inplace-upgrade-acceptance.cd.chef.co/data-collector/v0' COLLECTOR_TOKEN='TOKEN_VALUE' kitchen converge`

_note: if you find yourself missing tools, please see [the readme](https://github.com/chef/automate/blob/master/components/compliance-service/smokin/README.md)_ 


### test cloud integration, delete and create
1) navigate to acceptance, settings, node integrations
2) find "aws-api integration"
3) go into the nodes for the integration, delete the node, delete the integration
4) create new integration for aws: api. select "Read my credentials from my EC2 environment" and name it "aws-api integration"
5) once created, click into details of integration and make sure there's a node there


### create some scan jobs to get some fresh data
1) navigate to acceptance, compliance, scan jobs
2) select create a new job, and go through the steps to create three scan jobs
 - select the aws-api integration you created and use the CIS Amazon Web Services Foundation Benchmark for the first one
 - select the automate integration, node name filter of `inspec*`, and use a rhel and ssh profile
 - select the aws-ec2 integration (or create one if none exists) and use some linux and ssh profiles


### delete and install profiles
1) navigate to acceptance, compliance, profiles
2) select one of the profiles, and delete it
3) choose a profile from the "available" store and click "get"


### check on apps page
1) navigate to acceptance, applications
2) take a look at the data on the page, click on some filters and see data change
3) select a new row and see sidebar data change


### check on event feed
1) navigate to acceptance, dashboards
2) notice some scan jobs and profiles data
3) select a filter and expect data (if any) to match filter


### check on client runs
1) navigate to acceptance, infrastructure
2) see a node with data (from the kitchen/audit run)
3) click into the details for the node, ensure there's data
4) go back to the list view, apply a filter and expect data (if any) to match filter
5) download json report


### check on desktops
1) navigate to acceptance, /desktop
2) see a desktop with data (from the kitchen/audit run)
3) click on a filter to open the list view
4) play with the filters
5) download report from list view
6) view the details for a desktop
7) download report from detail view


### check on chef servers
1) navigate to acceptance, infrastructure, chef servers
2) delete and recreate the chef server/org; instructions can be found in `automate/dev-docs/adding-data/adding_test_data.md` 
3) click into the details for the chef server and org
4) browse around


### check on compliance reports
1) navigate to acceptance, compliance, reports
2) navigate to nodes list, view scan results
3) click into node details on a node, view node report/history
4) go back to list view
5) view reporting profiles list, check scan results
6) navigate to controls list. apply a control filter. if the control status was different from overall status, expect overall status to change
7) download json report


## release notes
read through the list of changes posted in #a2-release-coordinate\
find the commits from our team\
pick out which ones seem to be most relevant/important for our users. if you're unsure, ask in #a2-release-coordinate or ping Natalie directly\
write a simple, short sentence in the [pending release notes](https://github.com/chef/automate/wiki/Pending-Release-Notes) to let the customers know about this improvement/bug fix/new feature


## let people know you're done
if you found a bug during acceptance testing and you're not sure if it's blocking, ping Natalie/Jon from #a2-release-coordinate to ask them their opinion\
if you found no blocking/big bugs, let ppl know you've completed acceptance and release notes with a quick message in #a2-release-coordinate
