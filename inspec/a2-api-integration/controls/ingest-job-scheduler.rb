title 'ingest-service/JobScheduler REST API integration tests'



control 'ingest-job-scheduler' do
  title 'check ingest job-scheduler endpoints'
  desc 'Verifies configuration of jobs inside the ingest scheduler'

  describe 'GET status response' do
    let(:job_scheduler_status_endpoint) do
      '/api/v0/retention/nodes/status'
    end

    it 'GET /api/v0/retention/nodes/status returns the correct status' do
      job_scheduler_status = automate_api_request(
        job_scheduler_status_endpoint,
        http_method: 'GET',
      )
      expect(job_scheduler_status.http_status).to eq 200
      expect(job_scheduler_status.parsed_response_body[:running]).to eq(true)
    end

    describe 'Jobs run after updating them' do
      let(:config_off) do
        {
          'threshold': '1h',
          'running': false,
          'every': '15m',
        }
      end

      let(:config_on) do
        {
          'threshold': '1d',
          'running': true,
          'every': '15m',
        }
      end

      let(:get_test_node) do
        automate_api_request(
          '/api/v0/cfgmgmt/nodes',
          request_params: {
            filter: 'node_id:82760210-4686-497e-b039-efca78dee64b',
          },
          http_method: 'GET'
        )
      end

      let(:delete_request_params) do
        {
          'node_ids': [
            '82760210-4686-497e-b039-efca78dee64b'
          ],
        }
      end

      let(:elasticsearch_url) do
        ENV['ELASTICSEARCH_URL'] || "http://localhost:10144"
      end

      def refresh_elasticsearch()
        request = inspec.http("#{elasticsearch_url}/_refresh")
        failed_count = JSON.parse(request.body, symbolize_names: true)[:_shards][:failed]
        expect(failed_count).to eq 0
      end

      it 'if update has running == true, run the job' do
        # turn off missing node job
        expect(automate_api_request(
          '/api/v0/retention/nodes/missing-nodes/config',
          http_method: 'POST',
          request_body: config_off.to_json,
          ).http_status
        ).to eq 200

        # Add old CCR
        expect(automate_api_request(
            '/data-collector/v0',
            http_method: 'POST',
            request_body: inspec.profile.file("fixtures/converge/chefdk-debian-7-tester-2d206b_run_converge.json")
          ).http_status
        ).to eq 200

        # wait for elastic search to update
        refresh_elasticsearch()

        # turn on node missing with a day old threshold
        expect(automate_api_request(
          '/api/v0/retention/nodes/missing-nodes/config',
          http_method: 'POST',
          request_body: config_on.to_json,
          ).http_status
        ).to eq 200

        # wait for elastic search to update
        refresh_elasticsearch()

        # # check that the node added is missing
        expect(get_test_node.http_status).to eq 200
        expect(get_test_node.parsed_response_body.length).to eq 1
        node = get_test_node.parsed_response_body[0]
        expect(node[:status]).to eq 'missing'

        # clean up delete node
        expect(automate_api_request(
            '/api/v0/ingest/events/chef/node-multiple-deletes',
            request_body: delete_request_params.to_json,
            http_method: 'POST'
          ).http_status
        ).to eq 200
      end

      it 'if update running == false, do not run the job' do
        # turn off missing node job
        expect(automate_api_request(
          '/api/v0/retention/nodes/missing-nodes/config',
          http_method: 'POST',
          request_body: config_off.to_json,
          ).http_status
        ).to eq 200

        # Add old CCR
        expect(automate_api_request(
            '/data-collector/v0',
            http_method: 'POST',
            request_body: inspec.profile.file("fixtures/converge/chefdk-debian-7-tester-2d206b_run_converge.json")
          ).http_status
        ).to eq 200

        # wait for elastic search to update
        refresh_elasticsearch()

        # update node missing job config with a day old threshold
        expect(automate_api_request(
          '/api/v0/retention/nodes/missing-nodes/config',
          http_method: 'POST',
          request_body: config_off.to_json,
          ).http_status
        ).to eq 200

        # wait for elastic search to update
        refresh_elasticsearch()

        # check that the node added is not missing
        expect(get_test_node.http_status).to eq 200
        expect(get_test_node.parsed_response_body.length).to eq 1
        node = get_test_node.parsed_response_body[0]
        expect(node[:status]).to eq 'success'

        # clean up: delete node
        expect(automate_api_request(
            '/api/v0/ingest/events/chef/node-multiple-deletes',
            request_body: delete_request_params.to_json,
            http_method: 'POST'
          ).http_status
        ).to eq 200
      end
    end

    describe 'POST configures jobs' do
      let(:test_object) do
        {
          'threshold': '321h',
          'running': true,
          'every': '123m',
        }
      end

      [
        {
          'name': 'missing_nodes_for_deletion',
          'url': '/api/v0/retention/nodes/missing-nodes-deletion/config',
        },
        {
          'name': 'missing_nodes',
          'url': '/api/v0/retention/nodes/missing-nodes/config',
        },
        {
          'name': 'delete_nodes',
          'url': '/api/v0/retention/nodes/delete-nodes/config',
        },
      ].each do |job|
        it "POST #{job[:url]} configures the #{job[:name]} job" do
            expect(automate_api_request(
                job[:url],
                http_method: 'POST',
                request_body: test_object.to_json,
              ).http_status
            ).to eq 200

            job_scheduler_status = automate_api_request(
              job_scheduler_status_endpoint,
              http_method: 'GET',
            )
            expect(job_scheduler_status.http_status).to eq 200
            expect(job_scheduler_status.parsed_response_body[:running]).to eq(true)

            # Get the job we are testing
            job_status = job_scheduler_status
              .parsed_response_body[:jobs].select {|j| j[:name] == job[:name]}.first

            expect(job_status[:running]).to eq(test_object[:running])
            expect(job_status[:every]).to eq(test_object[:every])
            expect(job_status[:threshold]).to eq(test_object[:threshold])
        end

        [
          {
            'threshold': '1h',
            'running': false,
            'every': '1m',
          },
          {
            'threshold': '2h',
            'running': true,
            'every': '2m',
          },
          {
            'threshold': '3h',
            'running': false,
            'every': '3m',
          }
        ].each do |config|
          it 'updates are performed before returning' do
            # queuing up requests.
            for i in 1..10 do
              expect(automate_api_request(
                  job[:url],
                  http_method: 'POST',
                  request_body: {
                    'threshold': '77h',
                    'running': !config[:running],
                    'every': '77m',
                  }.to_json,
                ).http_status
              ).to eq 200
            end

            expect(automate_api_request(
                job[:url],
                http_method: 'POST',
                request_body: config.to_json,
              ).http_status
            ).to eq 200
    
            job_scheduler_status = automate_api_request(
              job_scheduler_status_endpoint,
              http_method: 'GET',
            )
            expect(job_scheduler_status.http_status).to eq 200
    
            job_status = job_scheduler_status
              .parsed_response_body[:jobs].select {|j| j[:name] == job[:name]}.first
    
            expect(job_status[:running]).to eq(config[:running])
            expect(job_status[:every]).to eq(config[:every])
            expect(job_status[:threshold]).to eq(config[:threshold])
          end
        end
      end
    end
  end
end
