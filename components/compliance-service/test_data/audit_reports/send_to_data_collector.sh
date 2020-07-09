#!/bin/bash -e
#
# This script can be used to send the compliance reports found in
# this directory to the ingestion endpoint of a Chef Automate server
#

# Automate URL, e.g. https://ec2-18-207-92-199.us-east-2.compute.amazonaws.com
AUTOMATE_URL="$1/data-collector/v0/"

# Automate API Token, e.g. hJueQcP-6RXYD0ocrGDBJ7EoROY=
AUTOMATE_API_TOKEN=$2

# Only tested on Linux and Darwin
if [[ ! "$OSTYPE" == "linux-gnu" ]] && [[ ! "$OSTYPE" == "darwin"* ]]; then
  echo "Unsupported OS type '$OSTYPE'"
  exit 1
fi

if ! grep -q "^https"<<<"$AUTOMATE_URL"; then
  echo "AUTOMATE_URL parameter missing or is invalid"
  echo "Usage:   ./$(basename $0) AUTOMATE_URL AUTOMATE_API_TOKEN"
  echo "Example: ./$(basename $0) https://my-automate.example.com hJueQcP-6RXYD0ocrGDBJ7EoROY="
  exit 2
fi

if [ ! `wc -m <<< "$AUTOMATE_API_TOKEN"` -eq 29 ]; then
  echo "AUTOMATE_API_TOKEN parameter missing or is invalid"
  echo "Usage:   ./$(basename $0) AUTOMATE_URL AUTOMATE_API_TOKEN"
  echo "Example: ./$(basename $0) https://my-automate.example.com hJueQcP-6RXYD0ocrGDBJ7EoROY="
  exit 3
fi

# Function to return a date in the past based on the DAYS parameter sent
# Send parameter 0 for today, 1 for yesterday and so on
function past_date()
{
  local DAYS=${1}
  if [[ "$OSTYPE" == "linux-gnu" ]]; then
    date -d "$DAYS day ago" +'%Y-%m-%d'
  elif [[ "$OSTYPE" == "darwin"* ]]; then
    date -v -${DAYS}d -u +%Y-%m-%d
  fi
}

# This function takes each file prefixed with $CURRENT_DATE in the current directory,
# changes the end_time to use NEW_DATE and sends it to the data collector url of Automate
function send_to_automate()
{
  local CURRENT_DATE=${1}
  local NEW_DATE=${2}
  for file in $(ls -1 ${CURRENT_DATE}*); do
    echo " * Sending \"$file\" with updated date of \"$NEW_DATE\""
    sed "s/$CURRENT_DATE/$NEW_DATE/" "$file" | curl --insecure -H "api-token: $AUTOMATE_API_TOKEN" -X POST "$AUTOMATE_URL" -H "Content-Type: application/json" -d @-
  done
}

echo "Sending the following compliance reports to Automate ( $AUTOMATE_URL )"

# Send all report files prefixed with '2018-03-07' with today's date
send_to_automate '2018-03-07' $(past_date 0)
send_to_automate '2018-02-09' $(past_date 0)

# Send all report files prefixed with '2018-04' with yesterday's date
send_to_automate '2018-04-01' $(past_date 1)
send_to_automate '2018-04-02' $(past_date 1)

# Send all report files prefixed with '2018-03-05' dated two days ago
send_to_automate '2018-03-05' $(past_date 2)

send_to_automate '2018-03-04' $(past_date 3)
send_to_automate '2018-04-01' $(past_date 4)
send_to_automate '2018-02-09' $(past_date 9)
