# NOTE(ssd) 2019-11-13: The mockgen tag is used to exclude files in
# the gateway package that require the mocked client package to
# build. According to the documenation of mockgen, we should be in
# "source" mode which doesn't require compiling the module; however,
# in practice it appears to be compiling the module anyway, leading to
# a chicken-and-egg problem.

export GOFLAGS = -tags=mockgen

all: mock_gateway/clients_mock.go mock_notifier/notifier_mock.go

mock_gateway/clients_mock.go: ../gateway/clients.go
	mockgen -source $< -destination $@

mock_notifier/notifier_mock.go: ../../notifications-client/notifier/notifier.go
	mockgen -source $< -destination $@

clean:
	rm mock_gateway/clients_mock.go mock_notifier/notifier_mock.go
