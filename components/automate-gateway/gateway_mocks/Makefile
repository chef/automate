all: mock_compliance_events/compliance_events_mock.go \
	mock_compliance_ingest/compliance_ingest_mock.go \
	mock_feed/feed_mock.go \
	mock_gateway/clients_mock.go \
	mock_notifier/notifier_mock.go

mock_compliance_events/compliance_events_mock.go: ../../compliance-service/ingest/events/compliance/compliance.pb.go
	mockgen -source $< -destination $@ -package mock_compliance_events

mock_compliance_ingest/compliance_ingest_mock.go: ../../compliance-service/ingest/ingest/compliance.pb.go
	mockgen -source $< -destination $@ -package mock_compliance_ingest

mock_feed/feed_mock.go: ../../compliance-service/api/automate-feed/feed.pb.go
	mockgen -source $< -destination $@

mock_gateway/clients_mock.go: ../gateway/clients.go
	mockgen -source $< -destination $@

mock_notifier/notifier_mock.go: ../../notifications-client/notifier/notifier.go
	mockgen -source $< -destination $@
