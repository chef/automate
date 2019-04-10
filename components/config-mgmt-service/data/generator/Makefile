generate: Gemfile.lock
	bundle exec ruby ./generator.rb

Gemfile.lock: Gemfile
	bundle install

clean:
	curl -XDELETE -k "${ELASTICSEARCH_URL}/node-state"
	curl -XDELETE -k "${ELASTICSEARCH_URL}/converge-history-*"

# Initialize mappings for loading the data manually
init:
	curl -XPUT -H "Content-Type: application/json" --upload-file ../mappings/converge-history.json http://${ELASTICSEARCH_URL}/converge-history
	curl -XPUT -H "Content-Type: application/json" --upload-file ../mappings/node-state.json http://${ELASTICSEARCH_URL}/node-state




