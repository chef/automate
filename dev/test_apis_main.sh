#!/bin/bash

domain="ec2-3-137-206-93.us-east-2.compute.amazonaws.com"
token="8Vve_boBz4qfRhugMU3slm6fvbo="
newline=$'\n'

overview() {
	echo "${newline}****************** overview API ******************"

	curl -k --location --request POST 'https://'$domain'/api/v0/compliance/reporting/stats/summary' \
	--header 'accept: application/json, text/plain, */*' \
	--header 'api-token: '$token'' \
	--header 'Content-Type: text/plain' \
	--data-raw '{"filters":[]}'

	wait
}

list_nodes() {
	echo "${newline}****************** List Nodes API ******************"

	curl -k 'https://'$domain'/api/v0/compliance/reporting/nodes/search' \
	--header 'accept: application/json, text/plain, */*' \
	--header 'api-token: '$token'' \
	--header 'Content-Type: text/plain' \
  	--data-raw '{"filters":[],"page":1,"per_page":100,"sort":"latest_report.end_time","order":"DESC"}' 

	wait
}

get_report() {
	echo "${newline}****************** Get Reports API ******************"

	curl -k 'https://'$domain'/api/v0/compliance/reporting/reports' \
  	--header 'accept: application/json, text/plain, */*' \
	--header 'api-token: '$token'' \
	--header 'Content-Type: text/plain' \
	--data-raw '{"filters":[{"type":"node_id","values":["660d0ba0-92bb-43fc-a62b-747e28ba2390"]}],"page":1,"per_page":10,"sort":"latest_report.end_time","order":"DESC"}' \
	--compressed 

	wait
}

suggestions() {
	echo "${newline}****************** Suggestion API ******************"

	curl -k 'https://'$domain'/api/v0/compliance/reporting/suggestions' \
	--header 'accept: application/json, text/plain, */*' \
	--header 'api-token: '$token'' \
	--header 'Content-Type: text/plain' \
	--data-raw '{"type":"environment","text":"","filters":[{"type":"start_time","values":["2022-10-08T00:00:00Z"]},{"type":"end_time","values":["2022-10-18T23:59:59Z"]}]}' \
	--compressed 

	wait
}

profiles() {
	echo "${newline}****************** List Profiles API ******************"

	curl -k 'https://'$domain'/api/v0/compliance/reporting/profiles' \
  	--header 'accept: application/json, text/plain, */*' \
	--header 'api-token: '$token'' \
	--header 'Content-Type: text/plain' \
  	--data-raw '{"filters":[],"page":1,"per_page":100}' \
  	--compressed 

	wait
}

get_profile() {
	echo -k "${newline}****************** Get profile API ******************"

	curl -k 'https://'$domain'/api/v0/compliance/reporting/stats/profiles' \
   	--header 'accept: application/json, text/plain, */*' \
	--header 'api-token: '$token'' \
	--header 'Content-Type: text/plain' \
	--data-raw '{"type":"summary","id":"b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015","filters":[]}' \
	--compressed 

	wait
}

list_control_report() {
	echo "${newline}****************** List Control Report API ******************"

	curl -k 'https://'$domain'/api/v0/compliance/reporting/reportcontrols/id/a4a396f1-a0b3-42ba-be92-74a081f7b421' \
	--header 'accept: application/json, text/plain, */*' \
	--header 'api-token: '$token'' \
	--header 'Content-Type: text/plain' \
	--data-raw '{"filters":[{"type":"from","values":["0"]},{"type":"size","values":["100"]},{"type":"status","values":[""]}]}' \
	--compressed 

	wait
}

list_control() {
	echo "${newline}****************** Control API ******************"

	curl -k 'https://'$domain'/api/v0/compliance/reporting/controls' \
	--header 'accept: application/json, text/plain, */*' \
	--header 'api-token: '$token'' \
	--header 'Content-Type: text/plain' \
	--data-raw '{"filters":[]}' \
	--compressed 

	wait
}

for i in {1..100} ; do
	overview &
	list_nodes &
	get_report &
	suggestions &
	profiles &
	get_profile &
	list_control_report &
	list_control &

	sleep 10
done

echo "${newline}Done executions!"
