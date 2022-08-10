#!/bin/bash

file="./demo.properties"


while IFS='=' read -r key value
do
    key=$(echo $key | tr '.' '_')
    eval ${key}=\${value}
done < "$file"

days_back=$1

startTime=$(date -j -v-${days_back}d +"%Y-%m-%dT%TZ")
endTime=$(date +"%Y-%m-%dT%TZ")

url="${hostname}/api/v0/compliance/reporting/stats/summary"
token=${token}'='

api_header="api-token: $token"
data_raw="{ \"type\": \"controls\", \"filters\": [ { \"type\": \"start_time\", \"values\": [ \"$startTime\" ] }, { \"type\": \"end_time\", \"values\": [ \"$endTime\" ] } ] }"

##Control Stats API
curl -k --location --request POST "$url" --header "$api_header" --header 'Content-Type: application/json' --data-raw "$data_raw" > ./controlStats.json
touch stats.csv && echo "Count of Failed,Count of Passed, Count of SKipped, Count of Waived, Count of Failed Major Controls, Count of Failed Minor Controls, Count of Failed Critical Controls" > stats.csv

cat ./controlStats.json | jq -r '.controls_summary | {Failed: .failures, Passed: .passed, Skipped: .skipped,Waived: .waived,Major: .majors,Minor: .minors,Crtical: .criticals,} | join(",")' > stats.csv


##Control List API
url="${hostname}/api/v0/compliance/reporting/controls"
data_raw="{ \"size\": \"100\", \"filters\": [ { \"type\": \"start_time\", \"values\": [ \"$startTime\" ] }, { \"type\": \"end_time\", \"values\": [ \"$endTime\" ] } ] }"
curl -k --location --request POST "$url" --header "$api_header" --header 'Content-Type: application/json' --data-raw "$data_raw" > ./control.json

echo "Control ID, Control Title, Profile Name, Impact, Last Report Time, Total Failed" > controllist.csv
cat control.json | jq -r '.control_items[] | {id: .id, title: .title, profile_name:.profile.title,impact: .impact,end_time: .end_time,total_failed: .control_summary.failed.total,} | join(",")' >> controllist.csv

