all: mock_authn/authenticate_mock.go \
	mock_authz/authz_mock.go \
	mock_cfgmgmt/cfgmgmt_mock.go \
	mock_compliance_events/compliance_events_mock.go \
	mock_compliance_ingest/compliance_ingest_mock.go \
	mock_feed/feed_mock.go \
	mock_gateway/clients_mock.go \
	mock_ingest/chef_ingest_mock.go \
	mock_notifier/notifier_mock.go

mock_cfgmgmt/cfgmgmt_mock.go: ../../../api/interservice/cfgmgmt/service/cfgmgmt.pb.go
	mockgen -source $< -destination $@ -package mock_cfgmgmt

mock_compliance_events/compliance_events_mock.go: ../../compliance-service/ingest/events/compliance/compliance.pb.go
	mockgen -source $< -destination $@ -package mock_compliance_events

mock_compliance_ingest/compliance_ingest_mock.go: ../../compliance-service/ingest/ingest/compliance.pb.go
	mockgen -source $< -destination $@ -package mock_compliance_ingest

mock_feed/feed_mock.go: ../../compliance-service/api/automate-feed/feed.pb.go
	mockgen -source $< -destination $@

mock_gateway/clients_mock.go: ../gateway/clients.go
	mockgen -source $< -destination $@

mock_ingest/chef_ingest_mock.go: ../../../api/interservice/ingest/chef.pb.go
	mockgen -source $< -destination $@

mock_notifier/notifier_mock.go: ../../notifications-client/notifier/notifier.go
	mockgen -source $< -destination $@
