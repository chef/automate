#!/bin/bash -e

out_file="run_apis.out"
url_prefix="http://localhost:10121"
node_id="460167a1-c193-4fb8-92ca-ebdc2b7d580e"
profile_id="f42d2f48c9acd48f52324d52ec575ca9028e405eb303f69cb34d79eb0e588b5c"
utc_13=`date -u +%Y-%m-%dT%TZ`

function call_api()
{
  local url=${1}
  echo "* Calling $url" >> $out_file
  curl -Ss "$url" | jq . >> $out_file
  echo -e "\n" >> $out_file
}

function call_non_trends()
{
  local filters=${1}
  call_api "$url_prefix/stats/summary?filters=$filters"
  call_api "$url_prefix/stats/failures?types=platform+environment+profile+control&filters=$filters"
  call_api "$url_prefix/stats/summary/nodes?filters=$filters"
  call_api "$url_prefix/stats/summary/controls?filters=$filters"
  call_api "$url_prefix/nodes?filters=$filters&per_page=2&page=1&sort=environment&order=asc"
  call_api "$url_prefix/reports?filters=node_id:$node_id&per_page=2&page=1"
}

function call_profiles()
{
  call_api "$url_prefix/search/profiles?filters=end_time:$utc_13&per_page=1&page=1"
}

function call_suggestions()
{
  call_api "$url_prefix/suggestions?type=node&text=aquamarine%20BURNING"
  call_api "$url_prefix/suggestions?type=profile&text=dev%20linux"
}

function call_trends()
{
  local params=${1}
  call_api "$url_prefix/stats/trend/nodes?$params"
  call_api "$url_prefix/stats/trend/controls?$params"
}

date > $out_file

call_non_trends "end_time:$utc_13"
call_non_trends "platform:centos"
call_non_trends "end_time:2017-11-21T23%3A59%3A59Z"
call_non_trends "end_time:2017-11-21T23%3A59%3A59Z+environment:DevSec%20Prod%20Zeta"
call_non_trends "end_time:2017-11-21T23%3A59%3A59Z+node_id:fc827270-3e68-404d-8ddf-339d942f5f64"
call_non_trends "control:sshd-39"
call_non_trends "role:apache_deb"
call_non_trends "profile_id:$profile_id"

call_trends "interval=259200&filters=start_time:2017-10-23T00%3A00%3A00Z+end_time:2017-11-23T23%3A59%3A59Z"
call_trends "interval=859200&filters=start_time:2017-10-23T00%3A00%3A00Z+end_time:2017-11-20T23%3A59%3A59Z+platform:centos"
call_trends "interval=859200&filters=start_time:2017-10-23T00%3A00%3A00Z+end_time:2017-11-20T23%3A59%3A59Z+environment:DevSec%20Prod%20Zeta"

call_profiles
call_suggestions

date >> $out_file
