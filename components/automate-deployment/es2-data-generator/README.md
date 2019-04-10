# Elasticsearch v2 Data Generator

Stuff in this directory is here to help create Elasticsearch indices for
daily data (insights, compliance) that is in Elasticsearch 2.x data
format.

## Why?

We upgraded Elasticsearch from 2.x to 5.x in a1 0.8->1.0. Elasticsearch
supports read compatibility for N+1 major versions (and 5 is the next
version after 2), so a1 can read indices created with Automate 0.8 and
before. In a2, we're upgrading Elasticsearch to version 6, which is not
compatible with Es 2 data. Therefore, we have to reindex the data in
Elasticsearch 5 to make it usable in Automate 2.

| Automate Version | Es Major Version | Oldest Supported Es Data Version |
|------------------|------------------|----------------------------------|
| 0.8 and lower    | 2                | 1                                |
| 1.0 to 1.x       | 5                | 2                                |
| 2.x              | 6                | 5                                |

## How To Create Es2 Data

The `reconfigure` cookbooks in a1 already did some data migrations where
necessary, and we only support upgrading to a2 from a relatively new
(1.8+) version of a1. Therefore, the basic process for creating a
dataset to test with is:

1. install a1 version 0.8
2. generate data
3. upgrade a1 to 1.8
4. save dataset

To follow the above process using the scripts/etc. here, do the
following:

1. `vagrant up` from `components/automate-deployment`
2. `vagrant ssh` into the box, `sudo -i` to root, `cd /a2/components/automate-deployment/es2-data-generator`
3. Run `scripts/setup` to install a1 0.8 and `chef-load`
4. Edit `chef-load.toml` as necessary, then run `chef-load start -c chef-load.toml`
   and/or `chef-load generate -c chef-load.toml`
5. When you have the data you want, run `scripts/upgrade-a1` to upgrade
   a1 and run a1's data migrations.
6. Run `scripts/capture` to make a tarball of the Elasticsearch data. It
   will show up as `a1-es2-sample-data-DATESTAMP.tgz` in the datasets
   directory.
