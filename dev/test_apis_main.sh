#!/bin/bash

domain="ec2-18-116-164-107.us-east-2.compute.amazonaws.com"
token="Y59ukGax4oVBlkkcVlS6sgkAafA="

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
	--data-raw '{"filters":[{"type":"node_id","values":["524c135f-973b-41f2-9214-7cd24a90f88a"]}],"page":1,"per_page":10,"sort":"latest_report.end_time","order":"DESC"}' \
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
	--data-raw '{"type":"summary","id":"416342174df2ecfef3b0c648ca6bd5f830f0166947f01e6f43e4c8HHURK","filters":[]}' \
	--compressed 

	wait
}

list_control_report() {
	echo "${newline}****************** List Control Report API ******************"

	curl -k 'https://'$domain'/api/v0/compliance/reporting/reportcontrols/id/48327ef1-6860-4385-95c6-cbbd26969c29' \
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


