#!/bin/bash

function getHeader() {
  local api_header="api-token: $1"
  printf '%s' "$api_header"
}

function callingAPI() {
  local url="https://$1$2"
  local response=$(curl -k --location --request POST "https://$1$2" --header "$3" --header 'Content-Type: application/json' --data-raw "$4")
  printf '%s' "$response"
}

function summaryAPIRequest() {
  local data_raw="{ \"type\": \"controls\", \"filters\": [ { \"type\": \"start_time\", \"values\": [ \"$1\" ] }, { \"type\": \"end_time\", \"values\": [ \"$2\" ] } ] }"
  printf '%s' "$data_raw"
}

function controlListAPIRequest() {
  local data_raw="{ \"size\": \"100\", \"filters\": [ { \"type\": \"start_time\", \"values\": [ \"$1\" ] }, { \"type\": \"end_time\", \"values\": [ \"$2\" ] } ] }"
  printf '%s' "$data_raw"
}

function summaryCSVHeader() {
  echo 'Count of Failed,Count of Passed, Count of SKipped, Count of Waived, Count of Failed Major Controls, Count of Failed Minor Controls, Count of Failed Critical Controls'
}

function summaryCSVData() {
  local response=$1
  echo "$response" | jq -r '.controls_summary | {Failed: .failures, Passed: .passed, Skipped: .skipped,Waived: .waived,Major: .majors,Minor: .minors,Crtical: .criticals,} | join(",")'
}

function controlListCSVHeader() {
  echo 'Control ID, Control Title, Profile Name, Impact, Last Report Time, Total Failed'
}

function controlListCSVData() {
  local response=$1
  echo "$response" | jq -r '.control_items[] | {id: .id, title: .title, profile_name:.profile.title,impact: .impact,end_time: .end_time,total_failed: .control_summary.failed.total,} | join(",")'
}

function nodeSearchAPIRequest() {
  echo "{ \"filters\": [ { \"type\": \"start_time\", \"values\": [ \"$1\" ] }, { \"type\": \"end_time\", \"values\": [ \"$2\" ] } ], \"page\": 1, \"per_page\": 100, \"sort\": \"latest_report.end_time\", \"order\": \"DESC\" } }"
}

function nodeSearchCSVHeader() {
  echo 'Node ID, Node Name'
}

function nodeSearchCSVData() {
  local response=$1
  echo "$response" | jq -r '.nodes[] | {id: .id, name: .nameact} | join(",")'
}

function createCSV() {
  local env="${1:-default}"
  local csvType="$2"
  local url="$3"
  local csvHeader="$4"
  local csvData="$5"
  rm -f "$env-$csvType.csv"
  printf 'Creating CSV\n========================\n'
  {
  printf 'API: https://%s\n' "$hostname$url"
  printf 'Start Time: %s\n' "$startTime"
  printf 'End Time: %s\n' "$endTime"
  printf 'Environment: %s\n' "$env"

  echo "$csvHeader"
  printf '%s' "$csvData"
  } >> "$env-$csvType.csv"
  printf 'Created CSV %s\n========================\n' "$env-$csvType.csv"

}

while getopts f:t:d:h:e: flag; do
  case "${flag}" in
  f) hostname=${OPTARG} ;;
  t) token=${OPTARG} ;;
  d) duration=${OPTARG} ;;
  e) environment=${OPTARG} ;;
  esac
done

startTime=$(date -j -v-"${duration}"d +"%Y-%m-%dT%TZ")
endTime=$(date +"%Y-%m-%dT%TZ")

summaryAPIUrl="/api/v0/compliance/reporting/stats/summary"
controlsAPIUrl="/api/v0/compliance/reporting/controls"
nodeSearchUrl="/api/v0/compliance/reporting/nodes/search"

header=$(getHeader "$token")

printf '%s\n' 'Calling Controls Stats Summary API'
requestBody=$(summaryAPIRequest "$startTime" "$endTime")
response=$(callingAPI "$hostname" "$summaryAPIUrl" "$header" "$requestBody")
createCSV "$environment" "stats" "$summaryAPIUrl" "$(summaryCSVHeader)" "$(summaryCSVData "$response")"
printf '%s\n' 'Completed Controls Stats Summary API'

printf '%s\n' 'Calling Control Lists API'
requestBody=$(controlListAPIRequest "$startTime" "$endTime")
response=$(callingAPI "$hostname" "$controlsAPIUrl" "$header" "$requestBody")
createCSV "$environment" "controllist" "$controlsAPIUrl" "$(controlListCSVHeader)" "$(controlListCSVData "$response")"
printf '%s\n' 'Completed Controls Lists API'

printf '%s\n' 'Calling Nodes Lists API'
requestBody=$(nodeSearchAPIRequest "$startTime" "$endTime")
echo "$requestBody"
response=$(callingAPI "$hostname" "$nodeSearchUrl" "$header" "$requestBody")
echo "$response"
createCSV "$environment" "nodesearch" "$nodeSearchUrl" "$(nodeSearchCSVHeader)" "$(nodeSearchCSVData "$response")"
printf '%s\n' 'Completed Nodes Lists API'

exit
