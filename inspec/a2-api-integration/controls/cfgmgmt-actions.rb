# encoding: utf-8
# copyright: 2018, Chef Software, Inc.
# license: All rights reserved
title 'config-mgmt-service actions REST API integration tests'

control 'config-mgmt-action-1' do
  title 'Submit config-mgmt actions fixture data'
  desc 'Verifies that we can submit action data to the legacy data-collector'

  %w(
    bag_create
    bag_delete
    client_create
    environment_create
    environment_delete
    environment_update
    group_create
    group_update
    item_bag_create
    item_bag_update
    node_create
    node_delete
    org_create
    policy_update
    user_create
    user_update
    version_cookbook_create
    version_cookbook_update
  ).each do |action|
    describe "POST /data-collector/v0 for action #{action}" do
      let(:api_request) do
        automate_api_request(
          '/data-collector/v0',
          http_method: 'POST',
          request_body: inspec.profile.file("fixtures/converge/actions/#{action}.json") 
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

control 'config-mgmt-action-2' do
  title '/api/v0/eventfeed'

  #######################################################################
  # Event Feed
  #######################################################################
  describe 'GET /api/v0/eventfeed' do
    let(:api_request) do
      automate_api_request(
        '/api/v0/eventfeed',
        request_params: request_params,
        http_method: 'GET'
      )
    end

    let(:request_params) do
      {
        'page_size': 10,
        'collapse': false,
        'start': 1522270800000,
        'end': 1522274400000,
        'filter': 'entity_type:bag'
      }
    end

    let(:api_runs_request) do
      automate_api_request(
        "/api/v0/eventfeed",
        http_method: 'GET'
      )
    end

    describe 'return all bag type events' do
      it 'should return 2 event' do
        expect(api_request.http_status).to eq 200
        
        expect(api_request.parsed_response_body[:total_events]).to eq '2'
      end
    end
  end

  #######################################################################
  # Event Strings
  #######################################################################
  describe 'GET /api/v0/eventstrings' do
    let(:api_request) do
      automate_api_request(
        '/api/v0/eventstrings',
        request_params: request_params,
        http_method: 'GET'
      )
    end

    let(:request_params) do
      {
        'start': start_date,
        'end': end_date,
        'timezone': timezone,
        'hours_between': bucket_size,
      }
    end

    let(:timezone) { 'UTC' }
    let(:start_date) { '2018-03-26' }
    let(:end_date) { '2018-03-29' }
    let(:bucket_size) { 3 }
    let(:number_of_buckets) { 0 }

    shared_examples "string requests tests" do
      it 'should return 3 strings and the correct number of buckets' do
        expect(api_request.http_status).to eq 200
        
        expect(api_request.parsed_response_body[:strings].length).to eq 3

        create_string = api_request.parsed_response_body[:strings].detect {|string| string[:event_action] == 'create'}
        expect(create_string[:collection].length).to eq number_of_buckets

        delete_string = api_request.parsed_response_body[:strings].detect {|string| string[:event_action] == 'delete'}
        expect(delete_string[:collection].length).to eq number_of_buckets

        update_string = api_request.parsed_response_body[:strings].detect {|string| string[:event_action] == 'update'}
        expect(update_string[:collection].length).to eq number_of_buckets
      end
    end

    context 'requesting strings around ingested actions' do
      let(:start_date) { '2018-03-26' }
      let(:end_date) { '2018-03-29' }
      let(:bucket_size) { 12 }
      let(:number_of_buckets) { (24/bucket_size) * 4 }
      it 'should return strings that have all the ingested actions' do
        expect(api_request.http_status).to eq 200

        create_string = api_request.parsed_response_body[:strings].detect {|string| string[:event_action] == 'create'}
        
        #bucket 2018/03/26T00:00-2018/03/26T11:59
        expect(create_string[:collection][0][:events_count].length).to eq 0

        #bucket 2018/03/26T12:00-2018/03/26T23:59
        expect(create_string[:collection][1][:events_count].length).to eq 1
        expect(create_string[:collection][1][:events_count]).to include(
          include(name: 'group', count: '1'),
        )
        #bucket 2018/03/27T00:00-2018/03/27T11:59
        expect(create_string[:collection][2][:events_count].length).to eq 1
        expect(create_string[:collection][2][:events_count]).to include(
          include(name: 'client', count: '1'),
        )
        #bucket 2018/03/27T12:00-2018/03/27T23:59
        expect(create_string[:collection][3][:events_count].length).to eq 3
        expect(create_string[:collection][3][:events_count]).to include(
          include(name: 'node', count: '1'),
          include(name: 'user', count: '1'),
          include(name: 'organization', count: '1'),
        )
        #bucket 2018/03/28T00:00-2018/03/28T11:59
        expect(create_string[:collection][4][:events_count].length).to eq 0

        #bucket 2018/03/28T12:00-2018/03/28T23:59
        expect(create_string[:collection][5][:events_count].length).to eq 3
        expect(create_string[:collection][5][:events_count]).to include(
          include(name: 'bag', count: '1'),
          include(name: 'environment', count: '1'),
          include(name: 'item', count: '1'),
        )

        #bucket 2018/03/29T00:00-2018/03/29T11:59
        expect(create_string[:collection][6][:events_count].length).to eq 0

        # bucket 2018/03/29T12:00-2018/03/29T23:59
        expect(create_string[:collection][7][:events_count].length).to eq 1
        expect(create_string[:collection][7][:events_count]).to include(
          include(name: 'version', count: '1'),
        )

        delete_string = api_request.parsed_response_body[:strings].detect {|string| string[:event_action] == 'delete'}
        
        #bucket 2018/03/26T00:00-2018/03/26T11:59
        expect(delete_string[:collection][0][:events_count].length).to eq 1
        expect(delete_string[:collection][0][:events_count]).to include(
          include(name: 'node', count: '1'),
        )

        #bucket 2018/03/26T12:00-2018/03/26T23:59
        expect(delete_string[:collection][1][:events_count].length).to eq 0

        #bucket 2018/03/27T00:00-2018/03/27T11:59
        expect(delete_string[:collection][2][:events_count].length).to eq 0

        #bucket 2018/03/27T12:00-2018/03/27T23:59
        expect(delete_string[:collection][3][:events_count].length).to eq 0

        #bucket 2018/03/28T00:00-2018/03/28T11:59
        expect(delete_string[:collection][4][:events_count].length).to eq 0

        #bucket 2018/03/28T12:00-2018/03/28T23:59
        expect(delete_string[:collection][5][:events_count].length).to eq 2
        expect(delete_string[:collection][5][:events_count]).to include(
          include(name: 'environment', count: '1'),
          include(name: 'bag', count: '1'),
        )

        #bucket 2018/03/29T00:00-2018/03/29T11:59
        expect(delete_string[:collection][6][:events_count].length).to eq 0

        # bucket 2018/03/29T12:00-2018/03/29T23:59
        expect(delete_string[:collection][7][:events_count].length).to eq 0


        update_string = api_request.parsed_response_body[:strings].detect {|string| string[:event_action] == 'update'}
        
        #bucket 2018/03/26T00:00-2018/03/26T11:59
        expect(update_string[:collection][0][:events_count].length).to eq 0

        #bucket 2018/03/26T12:00-2018/03/26T23:59
        expect(update_string[:collection][1][:events_count].length).to eq 0

        #bucket 2018/03/27T00:00-2018/03/27T11:59
        expect(update_string[:collection][2][:events_count].length).to eq 0

        #bucket 2018/03/27T12:00-2018/03/27T23:59
        expect(update_string[:collection][3][:events_count].length).to eq 3
        expect(update_string[:collection][3][:events_count]).to include(
          include(name: 'group', count: '1'),
          include(name: 'item', count: '1'),
          include(name: 'policy', count: '1'),
        )

        #bucket 2018/03/28T00:00-2018/03/28T11:59
        expect(update_string[:collection][4][:events_count].length).to eq 0

        #bucket 2018/03/28T12:00-2018/03/28T23:59
        expect(update_string[:collection][5][:events_count].length).to eq 1
        expect(update_string[:collection][5][:events_count]).to include(
          include(name: 'user', count: '1'),
        )

        #bucket 2018/03/29T00:00-2018/03/29T11:59
        expect(update_string[:collection][6][:events_count].length).to eq 0

        # bucket 2018/03/29T12:00-2018/03/29T23:59
        expect(update_string[:collection][7][:events_count].length).to eq 2
        expect(update_string[:collection][7][:events_count]).to include(
          include(name: 'environment', count: '1'),
          include(name: 'version', count: '1'),
        )
      end

      include_examples "string requests tests"
    end

    [ 
      { start_date:  '2018-03-09', end_date: '2018-03-15'}, # US start of daylight saving
      { start_date:  '2017-11-01', end_date: '2017-11-07'}, # US end of daylight saving
      { start_date:  '2018-03-23', end_date: '2018-03-29'}, # Europe start of daylight saving
      { start_date:  '2017-10-25', end_date: '2017-10-31'}, # Europe end of daylight saving
    ].each do | date_range |
      [ 1, 2, 3, 4, 6, 8, 12, 24 ].each do |houly_bucket_size| # all the allowed hourly bucket sizes
        %w(
          UTC
          America/Los_Angeles
          America/New_York
          Europe/Berlin
          Europe/London
        ).each do |time_zone| # sample time zone
          describe "start: #{date_range[:start_date]} end: #{date_range[:end_date]} bucket size: #{houly_bucket_size} time zone: #{time_zone}" do
            let(:start_date) { date_range[:start_date] }
            let(:end_date) { date_range[:end_date] }
            let(:bucket_size) { houly_bucket_size }
            let(:number_of_buckets) { (24/houly_bucket_size) * 7 }
            let(:timezone) { time_zone }

            include_examples "string requests tests"
          end
        end
      end
    end
  end
end
