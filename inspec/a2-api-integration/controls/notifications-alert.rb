# encoding: utf-8
# copyright: 2017, Chef Software, Inc.
# license: All rights reserved

title 'notifications-service alert integration tests'

control 'notifications-alert' do
  title 'Notifications Alert operations'
  desc 'Verifies alerts notifications rules api'

  # To Run these test locally run 'go run integration/helpers/requestbin/requestbin.go' in the background
  describe "when creating an alert" do
    after(:context) do
      all_rules = automate_api_request(
        "/api/v0/notifications/rules",
        http_method: 'GET'
      )
      all_rules.parsed_response_body[:rules].each do |rule|
        if rule[:name].start_with?(TEST_RULE_NAME_PREFIX)
          automate_api_request(
            "/api/v0/notifications/rules/#{rule[:id]}",
            http_method: 'DELETE'
          ).http_status
        end
      end
    end

    let(:failed_ccr_message) do
      return @failed_ccr_json ||= begin
        content = inspec.profile.file("fixtures/converge/converge-failure-report_run_converge.json")
        json = JSON.parse(content, :symbolize_names => true)
        json[:id] = SecureRandom.uuid
        request = JSON.generate(json)
      end
    end

    let(:send_failed_ccr) do
      automate_api_request(
        '/data-collector/v0',
        http_method: 'POST',
        request_body: failed_ccr_message
      )
    end

    let(:send_successful_ccr) do
      automate_api_request(
        '/data-collector/v0',
        http_method: 'POST',
        request_body: inspec.profile.file("fixtures/converge/chefdk-debian-7-tester-2d206b_run_converge.json")
      )
    end

    let(:send_failed_inspec_report) do
      automate_api_request(
        '/data-collector/v0',
        http_method: 'POST',
        request_body: inspec.profile.file("fixtures/compliance/compliance-failure-big-report.json")
      )
    end

    let(:send_failed_non_critical_inspec_report_1) do
      automate_api_request(
        '/data-collector/v0',
        http_method: 'POST',
        request_body: inspec.profile.file("fixtures/compliance/non-critical-failure-report_1.json")
      )
    end

    let(:send_failed_non_critical_inspec_report_2) do
      automate_api_request(
        '/data-collector/v0',
        http_method: 'POST',
        request_body: inspec.profile.file("fixtures/compliance/non-critical-failure-report_2.json")
      )
    end

    let(:send_failed_inspec_report_with_refs) do
      automate_api_request(
        '/data-collector/v0',
        http_method: 'POST',
        request_body: inspec.profile.file("fixtures/compliance/scan_ref_bug.json")
      )
    end

    let(:send_successful_inspec_report) do
      automate_api_request(
        '/data-collector/v0',
        http_method: 'POST',
        request_body: inspec.profile.file("fixtures/compliance/runner-1604-1-delivered.cd.chef.co_inspec_report.json")
      )
    end

    let(:requestbin_url) do
      ENV['test_notifications_endpoint']
    end

    let(:rule_name) do
      "#{TEST_RULE_NAME_PREFIX}-#{SecureRandom.rand(1<<20)}"
    end

    let(:requestbin_name) do
      return @requestbin_name ||= "#{TEST_RULE_NAME_PREFIX}-#{SecureRandom.rand(1<<20)}"
    end

    let(:create_ccr_rule) do
      request = automate_api_request(
        '/api/v0/notifications/rules',
        http_method: 'POST',
        request_body: {
          rule: {
            name: rule_name,
            event: CCR_FAILURE,
            SlackAlert: {
              url: "#{requestbin_url}/#{requestbin_name}"
            }
          }
        }.to_json
      )
    end

    let(:create_inspec_service_now_rule) do
      request = automate_api_request(
        '/api/v0/notifications/rules',
        http_method: 'POST',
        request_body: {
          rule: {
            name: rule_name,
            event: COMPLIANCE_FAILURE,
            ServiceNowAlert: {
              url: "#{requestbin_url}/#{requestbin_name}",
              secret_id: "3705adff-c61c-4a09-ab56-c3aeed9659a8"
            }
          }
        }.to_json
      )
    end

    let(:create_inspec_rule) do
      request = automate_api_request(
        '/api/v0/notifications/rules',
        http_method: 'POST',
        request_body: {
          rule: {
            name: rule_name,
            event: COMPLIANCE_FAILURE,
            SlackAlert: {
              url: "#{requestbin_url}/#{requestbin_name}"
            }
          }
        }.to_json
      )
    end

    context "for failed CCRs" do
      it "the webhook receives an alert once when a failed CCR is processed twice" do
        # Get the current request count on the requestbin service
        request = inspec.http("#{requestbin_url}/#{requestbin_name}")
        before_request_count = JSON.parse(request.body, symbolize_names: true)[:request_count]

        # Create the CCR Rule
        expect(create_ccr_rule.http_status).to eq(200)
        sleep (5)

        # Send Failed CCR
        expect(send_failed_ccr.http_status).to eq(200)
        sleep (5)

        # Check if the requestbin received the alert
        request = inspec.http("#{requestbin_url}/#{requestbin_name}")
        after_request_count = JSON.parse(request.body, symbolize_names: true)[:request_count]
        expect(after_request_count).to eq(before_request_count + 1)

        # Send Failed CCR for a second time
        expect(send_failed_ccr.http_status).to eq(200)
        sleep (5)

        # Check that the requestbin did not received the alert
        request = inspec.http("#{requestbin_url}/#{requestbin_name}")
        after_request_count = JSON.parse(request.body, symbolize_names: true)[:request_count]
        expect(after_request_count).to eq(before_request_count + 1)
      end

      it "the webhook does not receives an alert when a successful CCR is processed" do
        # Get the current request count on the requestbin service
        request = inspec.http("#{requestbin_url}/#{requestbin_name}")
        before_request_count = JSON.parse(request.body, symbolize_names: true)[:request_count]

        # Create the CCR Rule
        expect(create_ccr_rule.http_status).to eq(200)
        sleep (5)

        # Send successful CCR
        expect(send_successful_ccr.http_status).to eq(200)
        sleep (5)

        # Check if the requestbin received the alert
        request = inspec.http("#{requestbin_url}/#{requestbin_name}")
        after_request_count = JSON.parse(request.body, symbolize_names: true)[:request_count]
        expect(after_request_count).to eq(before_request_count)
      end
    end

    context "for failed InSpec reports" do
      it "the webhook receives an alert only once when a failed InSpec report is processed twice" do
        # Get the current request count on the requestbin service
        request = inspec.http("#{requestbin_url}/#{requestbin_name}")
        before_request_count = JSON.parse(request.body, symbolize_names: true)[:request_count]

        # Create the InSpec failure Rule
        expect(create_inspec_rule.http_status).to eq(200)
        sleep (5)

        # Send Failed InSpec report
        expect(send_failed_inspec_report.http_status).to eq(200)
        sleep (5)

        # Check if the requestbin received the alert
        request = inspec.http("#{requestbin_url}/#{requestbin_name}")
        after_request_count = JSON.parse(request.body, symbolize_names: true)[:request_count]
        expect(after_request_count).to eq(before_request_count + 1)

        # Send Failed InSpec report for a second time
        expect(send_failed_inspec_report.http_status).to eq(200)
        sleep (5)

        # Check that the requestbin did not received a second alert
        request = inspec.http("#{requestbin_url}/#{requestbin_name}")
        after_request_count = JSON.parse(request.body, symbolize_names: true)[:request_count]
        expect(after_request_count).to eq(before_request_count + 1)
      end

      it "when 'refs' are part of a failed InSpec report a notification is still sent" do
        # Get the current request count on the requestbin service
        request = inspec.http("#{requestbin_url}/#{requestbin_name}")
        before_request_count = JSON.parse(request.body, symbolize_names: true)[:request_count]

        # Create the InSpec failure Rule
        expect(create_inspec_rule.http_status).to eq(200)
        sleep (5)

        # Send Failed InSpec report
        expect(send_failed_inspec_report_with_refs.http_status).to eq(200)
        sleep (5)

        # Check if the requestbin received the alert
        request = inspec.http("#{requestbin_url}/#{requestbin_name}")
        after_request_count = JSON.parse(request.body, symbolize_names: true)[:request_count]
        expect(after_request_count).to eq(before_request_count + 1)
      end

      it "the webhook does not receives an alert when a successful InSpec report is processed" do
        # Get the current request count on the requestbin service
        request = inspec.http("#{requestbin_url}/#{requestbin_name}")
        before_request_count = JSON.parse(request.body, symbolize_names: true)[:request_count]

        # Create the InSpec failure Rule
        expect(create_ccr_rule.http_status).to eq(200)
        sleep (5)

        # Send successful InSpec report
        expect(send_successful_inspec_report.http_status).to eq(200)
        sleep (5)

        # Check if the requestbin received the alert
        request = inspec.http("#{requestbin_url}/#{requestbin_name}")
        after_request_count = JSON.parse(request.body, symbolize_names: true)[:request_count]
        expect(after_request_count).to eq(before_request_count)
      end

      it "the webhook does not receives an alert when a failed non critical InSpec report is processed" do
        # Get the current request count on the requestbin service
        request = inspec.http("#{requestbin_url}/#{requestbin_name}")
        before_request_count = JSON.parse(request.body, symbolize_names: true)[:request_count]

        # Create the slack InSpec failure Rule
        expect(create_inspec_rule.http_status).to eq(200)
        sleep (5)

        # Send the failed non critical InSpec report
        expect(send_failed_non_critical_inspec_report_1.http_status).to eq(200)
        sleep (5)

        # Check that the requestbin does not received an alert
        request = inspec.http("#{requestbin_url}/#{requestbin_name}")
        after_request_count = JSON.parse(request.body, symbolize_names: true)[:request_count]
        expect(after_request_count).to eq(before_request_count)
      end

      it "the service now webhook receives an alert only once when a failed non critical InSpec report is processed twice" do
        skip 'FIXME flakey test disabled'

        # Get the current request count on the requestbin service
        request = inspec.http("#{requestbin_url}/#{requestbin_name}")
        before_request_count = JSON.parse(request.body, symbolize_names: true)[:request_count]

        # Create the InSpec failure Rule
        expect(create_inspec_service_now_rule.http_status).to eq(200)
        sleep (5)

        # Send Failed InSpec report
        expect(send_failed_non_critical_inspec_report_2.http_status).to eq(200)
        sleep (5)

        # Check if the requestbin received the alert
        request = inspec.http("#{requestbin_url}/#{requestbin_name}")
        after_request_count = JSON.parse(request.body, symbolize_names: true)[:request_count]
        expect(after_request_count).to eq(before_request_count + 1)

        # Send Failed InSpec report for a second time
        expect(send_failed_non_critical_inspec_report_2.http_status).to eq(200)
        sleep (5)

        # Check that the requestbin did not received a second alert
        request = inspec.http("#{requestbin_url}/#{requestbin_name}")
        after_request_count = JSON.parse(request.body, symbolize_names: true)[:request_count]
        expect(after_request_count).to eq(before_request_count + 1)
      end
    end
  end
end
