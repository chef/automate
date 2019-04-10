generate:
	bundle exec ./generator.rb generate

# Same as generate but also saves the ElasticSearch documents in ./dump/
# The files in ./dump/ are used for testing the API calls hitting ElasticSearch. See ./dump/README.md for more details
generate-w-dump:
	bundle exec ./generator.rb generate --dump --matrix matrix4dump.yml
	cd .. && make rebuild-latest-clean

sample:
	rm -f Gemfile.lock
	bundle install
	bundle exec ./generator.rb sample

stats:
	bundle exec ./generator.rb stats

download-sample-profiles:
	mkdir -p .tmp/profiles/
	git clone --depth 1 git@github.com:chef/compliance-profiles.git .tmp/profiles/
	cd .tmp/profiles/dist/supported/; \
	curl -k -O https://s3-eu-west-1.amazonaws.com/apop-bucket/profiles/mylinux-failure-minor-5.2.0.tar.gz; \
	curl -k -O https://s3-eu-west-1.amazonaws.com/apop-bucket/profiles/mylinux-success-1.8.9.tar.gz; \
	curl -k -O https://s3-eu-west-1.amazonaws.com/apop-bucket/profiles/mylinux-failure-major-5.4.4.tar.gz

clean:
	rm -rf ./report
	rm -rf .tmp/profiles/

start-client-containers:
	$(MAKE) build-client-containers
	@$(MAKE) run-client-containers

build-client-containers:
	cd targets/client_containers && ./run build

run-client-containers:
	cd targets/client_containers && ./run start

stop-client-containers:
	cd targets/client_containers && ./run stop
