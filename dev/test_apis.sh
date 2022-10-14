#!/bin/bash

domain="'$domain'"
token="1c92BssTcIGG3ClopuwooFXedQ4="
newline=$'\n'

overview() {
	curl -k --location --request POST 'https://'$domain'/api/v0/compliance/reporting/stats/summary' \
--header 'accept: application/json, text/plain, */*' \
--header 'api-token: '$token'' \
--header 'Content-Type: text/plain' \
--data-raw '{"filters":[]}'
}

list_nodes() {
	curl -k 'https://'$domain'/api/v0/compliance/reporting/nodes/search' \
	--header 'accept: application/json, text/plain, */*' \
	--header 'api-token: '$token'' \
	--header 'Content-Type: text/plain' \
  	--data-raw '{"filters":[],"page":1,"per_page":100,"sort":"latest_report.end_time","order":"DESC"}' \

}

get_report() {
	curl 'https://'$domain'/api/v0/compliance/reporting/reports' \
  	--header 'accept: application/json, text/plain, */*' \
	--header 'api-token: '$token'' \
	--header 'Content-Type: text/plain' \
  --data-raw '{"filters":[{"type":"node_id","values":["9b9f4e51-b049-4b10-9555-10578916e666"]}],"page":1,"per_page":10,"sort":"latest_report.end_time","order":"DESC"}' \
  --compressed \
  --insecure
}

suggestions() {
	curl 'https://'$domain'/api/v0/compliance/reporting/suggestions' \
	--header 'accept: application/json, text/plain, */*' \
	--header 'api-token: '$token'' \
	--header 'Content-Type: text/plain' \
	--data-raw '{"type":"control_tag_value","text":"","type_key":"control_tag:gtitle","filters":[{"type":"start_time","values":["2022-10-02T00:00:00Z"]},{"type":"end_time","values":["2022-10-12T23:59:59Z"]}]}' \
	--compressed \
	--insecure
}

profiles() {
	curl 'https://'$domain'/api/v0/compliance/reporting/profiles' \
  	--header 'accept: application/json, text/plain, */*' \
	--header 'api-token: '$token'' \
	--header 'Content-Type: text/plain' \
  	--data-raw '{"filters":[],"page":1,"per_page":100}' \
  	--compressed \
 	--insecure
}

get_profile() {
	curl 'https://'$domain'/api/v0/compliance/reporting/stats/profiles' \
   	--header 'accept: application/json, text/plain, */*' \
	--header 'api-token: '$token'' \
	--header 'Content-Type: text/plain' \
  --data-raw '{"type":"summary","id":"b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015","filters":[]}' \
  --compressed \
  --insecure
}

list_control_report() {
	curl 'https://'$domain'/api/v0/compliance/reporting/reportcontrols/id/44024b50-2e0d-42fa-a57c-dddddddddddd' \
	--header 'accept: application/json, text/plain, */*' \
	--header 'api-token: '$token'' \
	--header 'Content-Type: text/plain' \
	--data-raw '{"filters":[{"type":"from","values":["0"]},{"type":"size","values":["100"]},{"type":"status","values":[""]}]}' \
	--compressed \
	--insecure
}

list_control() {
	curl 'https://'$domain'/api/v0/compliance/reporting/controls' \
	--header 'accept: application/json, text/plain, */*' \
	--header 'api-token: '$token'' \
	--header 'Content-Type: text/plain' \
	--data-raw '{"filters":[]}' \
	--compressed \
	--insecure
}

for i in {1..1000} ; do
    echo "${newline}****************** overview ******************"
	overview
	echo "${newline}****************** List node ******************"
	list_nodes
	echo "${newline}****************** Get Reports ******************"
	get_report
	echo "${newline}****************** Suggestion ******************"
	suggestions
	echo "${newline}****************** List profiles ******************"
	profiles
	echo "${newline}****************** Get profile ******************"
	get_profile
	echo "${newline}****************** List Control Report ******************"
	list_control_report
	echo "${newline}****************** Control ******************"
	list_control
	echo "${newline}Done!"
   
done