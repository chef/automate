#!/usr/bin/env python
import datetime
import json
import sys
import os.path
import difflib

index = sys.argv[1]
time  = sys.argv[2]
value = sys.argv[3]
history_file = "/opt/collect_metrics/index_records.json"

if os.path.isfile(history_file):
  file = open(history_file,'r+')
  contents = file.read()
  data = json.loads(contents)

  #the file exists and has a previous record for this index
  if index in data:
    seconds_elapsed = int(time) - int(data[index]["time"])
    documents_indexed = int(value) - int(data[index]["documents"])
    documents_per_second = float(documents_indexed) / seconds_elapsed
    documents_per_minute = documents_per_second * 60

  #the file exists, but we haven't seen this index
  else:
    documents_per_second = 0.0
    documents_per_minute = 0.0

  print documents_per_minute

  data[index] = { "time": time, "documents": value }
  file.seek(0)
  file.write(json.dumps(data))
  file.truncate()
  file.close()

#the file doesn't exist, so this is our first run
else:
  data = { index: { "time": time, "documents": value } }
  file = open(history_file, 'wt')
  file.write(json.dumps(data))
  file.close()
  print "0.0"
