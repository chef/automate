##### GRPC SETUP #####
require 'api/reporting/reporting_pb'
require 'api/reporting/reporting_services_pb'

describe File.basename(__FILE__) do
  Reporting = Chef::Automate::Domain::Compliance::Api::Reporting unless defined?(Reporting)

  def reporting
    Reporting::ReportingService;
  end

  it "errors" do
    assert_grpc_error("'control_tag' filter is required for 'control_tag_value' suggestions", 2) do
      GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
        type: 'control_tag_value',
        filters: []
      )
    end
  end

  it "works" do
    # suggest control tag keys
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'control_tag_key',
      text: 'scope',
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-01T23:59:59Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ],
      size: 10
    )
    expected = [
      "scope",
      "scoop",
      "Scoops"
    ]
    assert_suggestions_text(expected, actual_data)

    # suggest control tag keys
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'control_tag_key',
      text: 'scope',
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-01T23:59:59Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ],
      size: 2
    )
    expected = [
      "scope",
      "scoop"
    ]
    assert_suggestions_text(expected, actual_data)

    # suggest control tag keys
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'control_tag_key',
      text: 'Scoo',
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-01T23:59:59Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ],
      size: 100
    )
    expected = [
      "scoop",
      "Scoops",
      "scope"
    ]
    assert_suggestions_text(expected, actual_data)

    # suggest control tag keys without text given
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'control_tag_key',
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-01T23:59:59Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ],
      size: 100
    )
    expected = [ "cci", "gtitle", "satisfies", "scoop", "Scoops", "scope", "stig_id", "tag1", "web" ]
    assert_suggestions_text(expected, actual_data)

    # suggest control tag values with a tag key filter without text
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'control_tag_value',
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-01T23:59:59Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
        Reporting::ListFilter.new(type: 'control_tag:scope', values: []),
      ],
      size: 100
    )
    expected = [ "Apache", "apalache", "NginX" ]
    assert_suggestions_text(expected, actual_data)

    # suggest control tag values with tag key filter without text
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'control_tag_value',
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-01T23:59:59Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
        Reporting::ListFilter.new(type: 'control_tag:satisfies', values: []),
      ],
      size: 5
    )
    expected = [ "apache-1", "apache-2", "NGX-1", "NGX-2", "SRG-00006" ]
    assert_suggestions_text(expected, actual_data)

    # suggest control tag values with a tag key filter without text
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'control_tag_value',
      text: 'apaCH',
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-01T23:59:59Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
        Reporting::ListFilter.new(type: 'control_tag:scope', values: []),
      ]
    )
    expected = [ "Apache", "apalache" ]
    assert_suggestions_text(expected, actual_data)

    # suggest control tag values with a missing tag key filter
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'control_tag_value',
      text: 'apaCH',
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-01T23:59:59Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
        Reporting::ListFilter.new(type: 'control_tag:missing', values: []),
      ]
    )
    expected = []
    assert_suggestions_text(expected, actual_data)
  end
end
