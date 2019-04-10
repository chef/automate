# encoding: utf-8
# copyright: 2017, Chef Software, Inc.
# license: All rights reserved

CCR_FAILURE = "CCRFailure"
CCR_SUCCESS = "CCRSuccess"
COMPLIANCE_FAILURE = "ComplianceFailure"
COMPLIANCE_SUCCESS = "ComplianceSuccess"
TEST_RULE_NAME_PREFIX = "notifications-test"

title 'notifications-service REST API integration tests'

control 'notifications-crud-rule' do
  title 'Notifications Rule CRUD operations'
  desc 'Verifies CRUD operations on the notifications rules api'

  describe "when creating a rule" do
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

    let(:create_request) do
      automate_api_request(
        '/api/v0/notifications/rules',
        http_method: 'POST',
        request_body: rule_body
      )
    end

    let(:get_request) do
      automate_api_request(
        "/api/v0/notifications/rules/#{create_request.parsed_response_body[:id]}",
        http_method: 'GET'
      )
    end

    let(:list_request) do
      automate_api_request(
        "/api/v0/notifications/rules",
        http_method: 'GET'
      )
    end

    let(:delete_request) do
      automate_api_request(
        "/api/v0/notifications/rules/#{create_request.parsed_response_body[:id]}",
        http_method: 'DELETE'
      )
    end

    let(:rule_body) do
      {
        rule: {
          name: rule_name,
          event: rule_event
        }.merge(rule_action)
      }.to_json
    end

    let(:rule_action) do
      [rule_slack_alert, rule_webhook_alert].inject({}) do |acc, alert|
        if alert
          acc.merge(alert)
        else
          acc
        end
      end
    end

    let(:rule_name) do
      "#{TEST_RULE_NAME_PREFIX}-#{SecureRandom.rand(1<<20)}"
    end

    let(:rule_event) { "" }

    let(:rule_slack_alert) do
      if rule_slack_alert_url
        {
          SlackAlert: {
            url: rule_slack_alert_url
          }
        }
      else
        nil
      end
    end
    let(:rule_slack_alert_url) { nil }

    let(:rule_webhook_alert) do
      if rule_webhook_alert_url
        {
          WebhookAlert: {
            url: rule_webhook_alert_url
          }
        }
      else
        nil
      end
    end
    let(:rule_webhook_alert_url) { nil }

    context "when an alert is not specified" do
      it "fails with a 400" do
        expect(create_request.http_status).to eq(400)
        expect(create_request.parsed_response_body[:error] || "").not_to eq("")
      end
    end

    context "when event type is not valid" do
      let(:rule_slack_alert_url) do
        "http://localhost:55436"
      end
      it "fails with a 400" do
        expect(create_request.http_status).to eq(400)
        expect(create_request.parsed_response_body[:error] || "").not_to eq("")
      end
    end

    shared_examples "rule update tests" do
      let(:update_request) do
        automate_api_request(
          "/api/v0/notifications/rules/#{create_request.parsed_response_body[:id]}",
          http_method: 'PUT',
          request_body: updated_rule_body
        )
      end

      let(:updated_get_request) do
        expect(update_request.http_status).to eq(200)
        automate_api_request(
          "/api/v0/notifications/rules/#{create_request.parsed_response_body[:id]}",
          http_method: 'GET'
        )
      end

      let(:updated_rule_body) do
        {
          rule: {
            name: updated_rule_name,
            event: updated_rule_event
          }.merge(updated_rule_action)
        }.to_json
      end

      let(:updated_rule_action) do
        [updated_rule_slack_alert, updated_rule_webhook_alert].inject({}) do |acc, alert|
          if alert
            acc.merge(alert)
          else
            acc
          end
        end
      end

      let(:updated_rule_name) do
        "#{TEST_RULE_NAME_PREFIX}-#{SecureRandom.rand(1<<20)}"
      end

      let(:updated_rule_event) { "" }

      let(:updated_rule_slack_alert) do
        if updated_rule_slack_alert_url
          {
            SlackAlert: {
              url: updated_rule_slack_alert_url
            }
          }
        else
          nil
        end
      end
      let(:updated_rule_slack_alert_url) { nil }

      let(:updated_rule_webhook_alert) do
        if updated_rule_webhook_alert_url
          {
            WebhookAlert: {
              url: updated_rule_webhook_alert_url
            }
          }
        else
          nil
        end
      end
      let(:updated_rule_webhook_alert_url) { nil }

      context "when an action is not specified" do
        let(:updated_rule_event) { CCR_FAILURE }
        it "fails with a 400" do
          expect(update_request.http_status).to eq(400)
          expect(update_request.parsed_response_body[:error] || "").not_to eq("")
        end
      end

      context "when event type is not valid" do
        let(:rule_slack_alert_url) do
          "http://localhost:55436"
        end
        it "fails with a 400" do
          expect(update_request.http_status).to eq(400)
          expect(update_request.parsed_response_body[:error] || "").not_to eq("")
        end
      end

      context "when the url is not valid" do
        let(:updated_rule_event) { CCR_FAILURE }
        let(:rule_slack_alert_url) do
          "localhost:55436"
        end
        # TODO 
        # AuthZ system may fail to validate parameter and return Unauthorized
        it "fails with a 400 or 403" do
          expect(update_request.http_status.to_s).to match(/400|403/)
          expect(update_request.parsed_response_body[:error] || "").not_to eq("")
        end
      end

      context "when everything is valid" do
        let(:updated_rule_name) { rule_name }
        let(:updated_rule_event) { rule_event }
        let(:updated_rule_slack_alert_url) { rule_slack_alert_url }
        let(:updated_rule_webhook_alert_url) { rule_webhook_alert_url }

        def check_updated_rule
          rule = updated_get_request.parsed_response_body[:rule]
          expect(rule[:name]).to eq(updated_rule_name)
          expect(rule[:event]).to eq(updated_rule_event)
          if updated_rule_slack_alert
            expect(rule[:SlackAlert][:url]).to eq(updated_rule_slack_alert_url)
          else
            expect(rule[:SlackAlert]).to eq(nil)
          end

          if updated_rule_webhook_alert
            expect(rule[:WebhookAlert][:url]).to eq(updated_rule_webhook_alert_url)
          else
            expect(rule[:WebhookAlert]).to eq(nil)
          end
        end

        context "when updating name" do
          let(:updated_rule_name) { "#{TEST_RULE_NAME_PREFIX}-#{SecureRandom.rand(1<<20)}" }

          it "successfully applies the updates" do
            expect(update_request.http_status).to eq(200)
            check_updated_rule
          end
        end

        context "when updating action url" do
          let(:updated_rule_webhook_alert_url) do
            if rule_webhook_alert_url
              "http://localhost:55437"
            else
              nil
            end
          end

          let(:updated_rule_slack_alert_url) do
            if rule_slack_alert_url
              "http://localhost:55437"
            else
              nil
            end
          end

          it "successfully applies the updates" do
            expect(update_request.http_status).to eq(200)
            check_updated_rule
          end
        end

        context "when updating the action type" do
          let(:updated_rule_webhook_alert_url) do
            if rule_webhook_alert_url
              nil
            else
              "http://localhost:55437"
            end
          end

          let(:updated_rule_slack_alert_url) do
            if rule_slack_alert_url
              nil
            else
              "http://localhost:55437"
            end
          end

          it "successfully applies the updates" do
            expect(update_request.http_status).to eq(200)
            check_updated_rule
          end
        end

        context "when updating event" do
          [CCR_FAILURE, COMPLIANCE_FAILURE, CCR_SUCCESS, COMPLIANCE_SUCCESS].each do |event_type|
            context "to #{event_type}" do
              let(:updated_rule_event) do
                event_type
              end

              it "successfully applies the updates" do
                expect(update_request.http_status).to eq(200)
                check_updated_rule
              end
            end
          end
        end
      end
    end

    [CCR_FAILURE, COMPLIANCE_FAILURE].each do |ev|
      context "that notifies on #{ev}" do
        let(:rule_event) { ev }

        context "to slack" do
          let(:rule_slack_alert_url) do
            "http://localhost:55436"
          end

          context "when the rule does not exist" do
            it "succeeds" do
              expect(create_request.http_status).to eq(200)
            end

            it "can be fetched" do
              expect(get_request.http_status).to eq(200)
              rule = get_request.parsed_response_body[:rule]
              expect(rule[:name]).to eq(rule_name)
              expect(rule[:event]).to eq(rule_event)
              expect(rule[:SlackAlert][:url]).to eq(rule_slack_alert_url)
            end

            it "shows up in the list request" do
              expect(create_request.http_status).to eq(200)
              rule = get_request.parsed_response_body[:rule]
              expect(list_request.http_status).to eq(200)
              expect(list_request.parsed_response_body[:rules]).to include(rule)
            end

            it "can be deleted" do
              expect(delete_request.http_status).to(eq(200)) if create_request.http_status == 200
            end

            context "when updating the rule" do
              include_examples "rule update tests"
            end
          end

          context "when the rule already exists" do
            before do
              req = automate_api_request(
                '/api/v0/notifications/rules',
                http_method: 'POST',
                request_body: rule_body
              )
              expect(req.http_status).to eq(200)
            end

            it "should fail with 409" do
              expect(create_request.http_status).to eq(409)
              expect(create_request.parsed_response_body[:error]).to eq(
                "A rule with this name already exists")
            end
          end
        end

        context "to a webhook" do
          let(:rule_webhook_alert_url) do
            "http://localhost:55436"
          end

          context "when the rule does not exist" do
            it "succeeds" do
              expect(create_request.http_status).to eq(200)
            end

            it "can be fetched" do
              expect(get_request.http_status).to eq(200)
              rule = get_request.parsed_response_body[:rule]
              expect(rule[:name]).to eq(rule_name)
              expect(rule[:event]).to eq(rule_event)
              expect(rule[:WebhookAlert][:url]).to eq(rule_webhook_alert_url)
            end

            it "shows up in the list request" do
              expect(create_request.http_status).to eq(200)
              rule = get_request.parsed_response_body[:rule]
              expect(list_request.http_status).to eq(200)
              expect(list_request.parsed_response_body[:rules]).to include(rule)
            end

            it "can be deleted" do
              expect(delete_request.http_status).to(eq(200)) if create_request.http_status == 200
            end

            context "when updating the rule" do
              include_examples "rule update tests"
            end
          end

          context "when the rule already exists" do
            before do
              req = automate_api_request(
                '/api/v0/notifications/rules',
                http_method: 'POST',
                request_body: rule_body
              )
              expect(req.http_status).to eq(200)
            end

            it "should fail with 409" do
              expect(create_request.http_status).to eq(409)
              expect(create_request.parsed_response_body[:error]).to eq(
                "A rule with this name already exists")
            end
          end
        end
      end
    end
  end

  describe "when a rule does not exist" do
    let(:rule_id) { "123a1ed0-cad9-11e7-abc4-cec278b6b50a" }

    context "and a get request is performed on it" do
      let(:get_request) do
        automate_api_request(
          "/api/v0/notifications/rules/#{rule_id}",
          http_method: 'GET'
        )
      end

      it "404s" do
        expect(get_request.http_status).to eq(404)
      end
    end

    context "and a delete operation is performed on it" do
      let(:delete_request) do
        automate_api_request(
          "/api/v0/notifications/rules/#{rule_id}",
          http_method: 'DELETE'
        )
      end

      it "404s" do
        expect(delete_request.http_status).to eq(404)
      end
    end

    context "and an update operation is performed on it" do

      let(:rule_body) do
        {
          rule: {
            name: "#{TEST_RULE_NAME_PREFIX}-#{SecureRandom.rand(1<<20)}",
            event: CCR_FAILURE,
            SlackAlert: {
              url: "http://localhost:55436"
            },
          },
        }.to_json
      end

      let(:update_request) do
        automate_api_request(
          "/api/v0/notifications/rules/#{rule_id}",
          http_method: 'PUT',
          request_body: rule_body
        )
      end

      it "404s" do
        expect(update_request.http_status).to eq(404)
      end
    end
  end
end
