.DEFAULT_GOAL=all
all: test
setup:
	(bundle --version || gem install bundler) && \
	(bundle check || bundle install --jobs 5 --retry 5)
test: setup
	bundle exec rspec
