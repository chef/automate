# External Dependecies
## Below list is for the automate deps 

1. Elastic search
   * automate-elasticsearch
     * Version: 6.8.14

2. ngnix
   * automate-es-gateway 
   * automate-load-balancer
     * Version: 1.19.2 
  
3. Open JDK(Java) binaries
   * automate-openjdk
     * Version: 11.0.10+9

4. Memecached
   * automate-builder-memcached
     * Version: 1.5.19

5. bookshelf
   * automate-cs-bookshelf
     * Version: 14.4.4/20210520120637

6. builder-api
   * automate-builder-api
     * Version:9034/20200827185135 

7. builder-api-proxy
   * automate-builder-api-proxy
     * Version: 8997/20200812161534

8. curl
   * automate-cs-nginx
     * Version: 7.68.0/20200601114640

9. ruby
   * automate-cs-nginx
     * Version: 2.6.5/20200404043345

10. oc_bifrost
    * automate-cs-oc-bifrost
      * Version: 14.4.4/20210520120641

11. oc_erchef
    * automate-cs-oc-erchef
      * Version: 14.4.4/20210520120641

12. Dex
    * automate-dex
      * Version: 2.27.0

13. postgresql
    * automate-postgresql
      * Version: 9.6.11

    * pg-sidecar-service
      * Version: 9.6.11/20190305213451

14. go
    * automate-scaffolding-go
      * Version: 1.14


15. openresty-noroot
    * automate-workflow-nginx
      * Version: 1.13.6.2/20210520120637

16. inspec
    * compliance-service
      * Version: 4.24.32/20210112215549

17. elixir
    * notifications-service
      * Version: 1.8.0

18. postgresql-client
    * pg-sidecar-service
      * Version: 9.6.11/20190305213520 

19. sqitch_pg
    * pg-sidecar-service
      * Version: 3.7.4/20190305225225

20.  Third party go module dependencies: [Go mod link](https://github.com/chef/automate/blob/master/go.mod)

21. Third party UI dependencies: [Package json link](https://github.com/chef/automate/blob/master/components/automate-ui/package.json)
    

## Below deps is for A2HA backend 

1. postgresql
    * automate-backend-postgresql
      * Version: core/postgresql11/11.2/20210813070814

2. haproxy
    * automate-backend-haproxy
      * Version: core/haproxy/2.2.2/20200923113708

3. elasticsearch
    * automate-backend-elasticsearch
      * Version: chef/elasticsearch-odfe/0.10.1.2/20210812193038

4. journalbeat
    * automate-backend-journalbeat
      * Version: chef/journalbeat/6.8.6/20210812193038

5. metricbeat
    * automate-backend-metricbeat
      * Version: chef/metricbeat/6.8.6/20210812193038

6. kibana
    * automate-backend-kibana
      * Version: chef/kibana-odfe/0.10.0.4/20210812193038

7. terraform
    * automate-backend-deployment
      * Version: core/terraform/0.12.26/20200922233119      

8. aws-cli
    * automate-backend-deployment
      * Version: core/aws-cli/1.18.145/20200924113212

9. ruby
    * automate-cluster-ctl
      * Version: core/ruby26
