#!/bin/bash

#shellcheck disable=SC2034
test_name="proxy"
test_proxy="true"

source integration/services/common.sh
_ssh_node_container_name="$(service_container_name "ssh_node")"

do_setup() {
    do_setup_default
    local previous_umask
    previous_umask=$(umask)
    umask 022

    echo "Installing docker"
    hab pkg install --binlink core/docker
    echo "Installed docker"

    hab pkg install core/jq-static

    umask "$previous_umask"
}

jq() {
    hab pkg exec core/jq-static jq "$@"
}

do_deploy() {
    #shellcheck disable=SC2154
    chef-automate deploy config.toml \
        --product automate \
        --product builder \
        --hartifacts "$test_hartifacts_path" \
        --override-origin "$HAB_ORIGIN" \
        --manifest-dir "$test_manifest_path" \
        --admin-password chefautomate \
        --accept-terms-and-mlsa

    log_info "applying dev license"
    chef-automate license apply "$A2_LICENSE"

    echo "127.0.0.1 ${CONTAINER_HOSTNAME}" >> /etc/hosts

    timestamp=$(date +"%m-%d-%y-%H-%M")

    log_info "generating admin token"
    if ! token=$(chef-automate iam token create "$timestamp-tok" --admin); then
        log_error "Non-zero exit code, output:"
        log_error "$token"
        return 1
    fi

    export CYPRESS_ADMIN_TOKEN=$token
    export CYPRESS_RUN_FLAKY=$FLAKY

    log_info "fixing dns resolution for '${CONTAINER_HOSTNAME}'"
    echo "127.0.0.1 ${CONTAINER_HOSTNAME}" >> /etc/hosts

    yum install openssh -y
    yum install openssh-server -y
    systemctl start sshd
    chpasswd <<< 'root:123456'
}

do_test_deploy() {
    #shellcheck disable=SC2154
    #shellcheck source=integration/helpers/bldr_tests.sh
    source "${source_dir}/helpers/bldr_tests.sh"

    do_test_deploy_default
    # bldr_smoke_test

    # start_ssh_node
    
    log_info "running cypress in e2e"
    cd "${A2_ROOT_DIR}/e2e" || return 1
    export CYPRESS_SKIP_SSO=true
    export CYPRESS_BASE_URL="https://$CONTAINER_HOSTNAME"

    npm install # get dependencies defined in e2e/package.json
    if ! npm run cypress:run:proxy; then
        buildkite-agent artifact upload "cypress/videos/*;cypress/videos/**/*;cypress/screenshots/*;cypress/screenshots/**/*"
        return 1
    fi

#     # get api token
#     token=$(chef-automate iam token create admin --admin)
#     # TZoBEkR8RQg7r3KpFXbyINcbBo0=
#     echo "${token}"
#     if [[  -z "$token" ]] ; then
#       echo "Failed to create admin token"
#       exit 1
#     fi

#     # create credentials
#     id=$(curl 'https://localhost/api/v0/secrets'   \
#     -H "api-token: ${token}"   -d '{"id":"test","name":"test","type":"ssh","data":[{"key":"username","value":"abla"},{"key":"password","value":"dable"},{"key":"key","value":null}],"tags":[]}' -k | jq -r '.id')
#     echo "${id}"
#     if [[ -z "$id" ]] ; then
#       echo "Failed to fetch id"
#       exit 1
#     fi
#     # {"id":"065f2e62-b395-4cf1-97ef-e18fb31a6bce"}

#     # create node
#     node=$(curl 'https://localhost/api/v0/nodes/bulk-create' \
#         -H "api-token: ${token}" \
#         -d '{"nodes":[{"name":"aws test-3.138.179.244","manager":"automate","target_config":{"backend":"ssh","secrets":['${id}'],"port":22,"sudo":false,"hosts":["3.138.179.244"]},"tags":[]}]}' \
#         -k | jq -r '.ids[0]')
#     echo "$node"
#     if [[ -z "$node" ]] ; then
#       echo "Failed to create node"
#       exit 1
#     fi
#     # resp: {"ids":["01dd9c27-9504-45e0-a665-7273ff516187"]}

#     # upload profile
#     # curl 'https://localhost/api/v0/compliance/profiles?contentType=application/x-gzip&owner=admin' \
#     #   -H "api-token: ${token}"
#     #   --data-raw $'------WebKitFormBoundaryQrwmi71c5IFquAl3\r\nContent-Disposition: form-data; name="file"; filename="testproxy-0.1.0.tar.gz"\r\nContent-Type: application/x-gzip\r\n\r\n\r\n------WebKitFormBoundaryQrwmi71c5IFquAl3--\r\n' \
#     #   -k

#     upload=$(curl  -v -F file=@testproxy-0.1.0.tar.gz 'https://localhost/api/v0/compliance/profiles?contentType=application/x-gzip&owner=admin' -H "api-token: ${token}" -k | jq -r '.summary.valid')
#     echo "$upload"
#     if [[ -z "$upload" ]] ; then
#       echo "Failed to upload"
#       exit 1
#     fi

#     echo "valid ${upload}"
#     # {"summary":{"valid":true,"timestamp":"2021-01-22T13:54:36+00:00","location":"/hab/svc/compliance-service/var/tmp/inspec-upload528214652.tar.gz","controls":3}}

#     # get automate node manager
#     managerId=$(curl 'https://localhost/api/v0/nodemanagers/search' \
#       -H "api-token: ${token}" \
#       -d '{ "filter_map": [ { "key": "manager_type", "values": [ "automate" ] } ], "sort": "date_added" }' \
#       -k | jq -r '.managers[0].id')


#     # schedule scan job
#     scanId=$(curl 'https://localhost/api/v0/compliance/scanner/jobs' \
#       -H "api-token: ${token}" \
#       -d '{"type":"exec","tags":[],"name":"test","profiles":["compliance://admin/testProxy#0.1.0"],"node_selectors":[{"manager_id":'${managerId}',"filters":[{"key":"name","values":["aws test-3.138.179.244"],"exclude":false}]}],"recurrence":""}' \
#       -k | jq -r '.id')
#     echo "${scanId}"
#     # {"id":"e55e6047-072d-47d5-8d98-d9adc9db2630","name":"test"}

#     # # check if scan is complete
#     # curl 'https://localhost/api/v0/compliance/scanner/jobs/search' \
#     #   -H 'authority: localhost' \
#     #   -H 'sec-ch-ua: "Chromium";v="88", "Google Chrome";v="88", ";Not A Brand";v="99"' \
#     #   -H 'accept: application/json, text/plain, */*' \
#     #   -H 'authorization: Bearer eyJhbGciOiJSUzI1NiIsImtpZCI6ImIwOTU2NjI5YzQxMTc1NGU0NWZkMDI0MDZhZjJkMWQyOTVkNDdmMTIifQ.eyJpc3MiOiJodHRwczovL2EyLWRldi50ZXN0L2RleCIsInN1YiI6IkNpUmtZekV5TldFeE5pMWtZVFkxTFRReE5HTXRPREUyWmkwM05qYzNNR1UwWkRWbU5qSVNCV3h2WTJGcyIsImF1ZCI6ImF1dG9tYXRlLXNlc3Npb24iLCJleHAiOjE2MTE0MDc3NTUsImlhdCI6MTYxMTMyMTM1NSwiYXRfaGFzaCI6IkJBRzRoMzBuOUprNTY3Qy0tM01CT2ciLCJlbWFpbCI6ImFkbWluIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsIm5hbWUiOiJMb2NhbCBBZG1pbmlzdHJhdG9yIiwiZmVkZXJhdGVkX2NsYWltcyI6eyJjb25uZWN0b3JfaWQiOiJsb2NhbCIsInVzZXJfaWQiOiJkYzEyNWExNi1kYTY1LTQxNGMtODE2Zi03Njc3MGU0ZDVmNjIifX0.LBwBdLT_EZmL3qKVwX--dIyth1EOVmz-58F56LibBWpNEqLpTDCwOx4sRRKFjEJASdsambeGKCpKHUIr7BBaI1HedVcAMdEov3exlatbSsESzn69HgkHIy3zVz-_IDH0-SMWuT0qn8D4dIxVctk9wAqysdqQMuMiB1UE6xXYlaYVDBrMVw8_opezOT06rGHe5HwHV6b0b6Etp1K8Wh1WxEdu-zROKkwmXqiwhQq-FWLrPn7_x4f0yytkR3lcpnR9ai5UrReqzvpUqaX2D3rvxaFtzudhI-G1QM9dfl4kUMbpB3u8-tB5CwVNx0uDhXqGH0ycBt2qZCJxzFEV9C4RwQ' \
#     #   -H 'sec-ch-ua-mobile: ?0' \
#     #   -H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/88.0.4324.96 Safari/537.36' \
#     #   -H 'content-type: application/json+lax' \
#     #   -H 'origin: https://localhost' \
#     #   -H 'sec-fetch-site: same-origin' \
#     #   -H 'sec-fetch-mode: cors' \
#     #   -H 'sec-fetch-dest: empty' \
#     #   -H 'referer: https://localhost/compliance/scan-jobs/jobs' \
#     #   -H 'accept-language: en-US,en;q=0.9,hi;q=0.8' \
#     #   -H 'cookie: session=H99C7KbIZnC3HrDY3qvpvnIgbEMPAD3FZulsamwFoKg' \
#     #   --data-raw '{"filters":[{"key":"job_type","values":["exec"]},{"key":"parent_job","values":[""]}],"page":1,"per_page":100,"sort":"end_time","order":"DESC"}' \
#     #   --compressed \
#     #   --insecure

#     # {"jobs":[{"id":"e55e6047-072d-47d5-8d98-d9adc9db2630","name":"test","type":"exec","timeout":0,"tags":[],"start_time":"2021-01-22T13:57:40Z","end_time":"2021-01-22T13:57:50Z","status":"completed","retries":0,"retries_left":0,"results":[],"nodes":[],"profiles":[],"node_count":1,"profile_count":1,"node_selectors":[],"scheduled_time":"0001-01-01T00:00:00Z","recurrence":"","parent_id":"","job_count":0,"deleted":false}],"total":1}

#     # check the report failed or success
#     sleep 1m
#     status=$(curl 'https://localhost/api/v0/compliance/reporting/stats/summary' \
#     -H "api-token: ${token}" \
#     -d '{"filters":[{"type":"job_id","values":['${scanId}']},{"type":"start_time","values":["2021-01-12T00:00:00Z"]},{"type":"end_time","values":["2021-01-22T23:59:59Z"]}]}' \
#     -k | jq -r '.report_summary.stats.status')

# # {"controls_summary":null,"node_summary":null,"report_summary":{"stats":{"nodes":"1","platforms":1,"environments":1,"profiles":1,"nodes_cnt":1,"controls":3},"status":"failed","duration":0,"start_date":""}}
#     echo "${status}"
#     if [[ "$status" -eq "failed" ]] ; then
#       echo "OMG SCANNED===================::::::::::::::::::::---------"
#       exit 1
#     fi
}

# start_ssh_node() {
#     docker_run "${_ssh_node_container_name}" chefes/ssh-target-ubuntu1804:latest
# }