# encoding: utf-8
# copyright: 2017, Chef Software, Inc.
# license: All rights reserved
title 'compliance-service REST API integration tests'

# TODO: enable this!!
control 'compliance-1' do
  title 'Submit compliance fixture data'
  desc 'Verifies that we can submit data to the data collector'

  %w(
    policyfile-fixture-1-delivered.cd.chef.co_insepc_report
    automate-delivered.cd.chef.co_inspec_report
    runner-1604-1-delivered.cd.chef.co_inspec_report
  ).each do |report|
    describe "POST inspec report to /data-collector/v0 : #{report}" do
      let(:api_request) do
        automate_api_request(
          '/data-collector/v0',
          http_method: 'POST',
          request_body: inspec.profile.file("fixtures/compliance/#{report}.json")
        )
      end

      it 'should ingest the data successfully' do
        expect(api_request.http_status).to eq 200
      end
    end
  end
  # Wait for data to be indexed
  describe command('sleep 15') do
    its('exit_status') { should eq 0 }
  end
end

control 'compliance-nodes' do
  title '/api/v0/nodes/search'

  describe 'POST /api/v0/nodes/search' do
    let(:api_request) do
      automate_api_request(
        '/api/v0/nodes/search',
        http_method: 'POST',
        request_body: '{}'
      )
    end

    describe 'nodes call' do
      it 'should succeed' do
        expect(api_request.http_status).to eq 200
      end
      it 'should include the three nodes from the three ingested reports' do
        expect(api_request.parsed_response_body[:nodes]).to include(
          include(name: 'automate-delivered.cd.chef.co'),
          include(name: 'policyfile-fixture-1-delivered.cd.chef.co'),
          include(name: 'runner-1604-1-delivered.cd.chef.co'),
        )
      end
    end
  end
end

control 'compliance-jobs' do
  title '/api/v0/compliance/scanner/jobs/search'

  describe 'POST /api/v0/compliance/scanner/jobs/search' do
    let(:api_request) do
      automate_api_request(
        '/api/v0/compliance/scanner/jobs/search',
        http_method: 'POST',
        request_body: '{}'
      )
    end

    describe 'jobs call' do
      it 'should succeed' do
        expect(api_request.http_status).to eq 200
      end
    end
  end
end

control 'nodemanagers' do
  title '/api/v0/nodemanagers/search'

  describe 'POST /api/v0/nodemanagers/search' do
    let(:api_request) do
      automate_api_request(
        '/api/v0/nodemanagers/search',
        http_method: 'POST',
        request_body: '{}'
      )
    end

    describe 'nodemanagers call' do
      it 'should succeed' do
        expect(api_request.http_status).to eq 200
      end
    end
  end
end

control 'compliance-stats-summary' do
  title '/api/v0/compliance/reporting/stats/summary'

  describe 'POST /api/v0/compliance/reporting/stats/summary' do
    let(:api_request) do
      automate_api_request(
        '/api/v0/compliance/reporting/stats/summary',
        http_method: 'POST',
        request_body: '{}'
      )
    end

    describe 'stats summary' do
      it 'should succeed' do
        expect(api_request.http_status).to eq 200
      end
    end
  end
end

control 'compliance-reporting-nodes' do
  title '/api/v0/compliance/reporting/nodes'

  describe 'POST /api/v0/compliance/reporting/nodes/search' do
    let(:api_request) do
      automate_api_request(
        '/api/v0/compliance/reporting/nodes/search',
        http_method: 'POST',
        request_body: '{"filters":[{"type":"end_time","values":["2017-11-14T23:59:59Z"]}]}'
      )
    end

    describe 'reporting nodes' do
      it 'should succeed' do
        expect(api_request.http_status).to eq 200
      end
      it 'should include data from the ingested reports' do
        expect(api_request.parsed_response_body[:nodes]).to include(
          include(name: 'automate-delivered.cd.chef.co'),
          include(name: 'policyfile-fixture-1-delivered.cd.chef.co'),
          include(name: 'runner-1604-1-delivered.cd.chef.co'),
        )
      end
    end
  end
end
