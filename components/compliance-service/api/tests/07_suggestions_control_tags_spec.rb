##### GRPC SETUP #####
require 'interservice/compliance/reporting/reporting_pb'
require 'interservice/compliance/reporting/reporting_services_pb'

describe File.basename(__FILE__) do
  Reporting = Chef::Automate::Domain::Compliance::Reporting unless defined?(Reporting)

  def reporting
    Reporting::ReportingService;
  end

  it "errors without control_tag filter" do
    assert_grpc_error("rpc error: code = InvalidArgument desc = 'control_tag' filter is required for 'control_tag_value' suggestions", 2) do
      GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
        type: 'control_tag_value',
        filters: []
      )
    end
  end

  it "suggests control tag keys matching 'scope'" do
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
      "scope"
    ]
    assert_suggestions_text(expected, actual_data)
  end

  it "suggests control tag keys with a size of 3" do
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'control_tag_key',
      text: 'sco',
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-01T23:59:59Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ],
      size: 3
    )
    expected = [
      "scoop",
      "Scoops",
      "scope"
    ]
    assert_suggestions_text(expected, actual_data)
  end

  it "suggests control tag keys matching 'Scoo'" do
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
      "Scoops"
    ]
    assert_suggestions_text(expected, actual_data)
  end

  it "suggests control tag keys without text" do
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
  end

  it "suggests control tag keys with platform filter" do
    # suggest control tag keys without text given
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'control_tag_key',
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-01T23:59:59Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
        Reporting::ListFilter.new(type: 'platform', values: ['centos', 'windows'])
      ]
    )
    expected = ["satisfies", "scoop", "Scoops", "scope", "web"]
    assert_suggestions_text(expected, actual_data)
  end

  it "suggests control tag values without text" do
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
  end

  it "suggests control tag values without text and size of 5" do
    # suggest control tag values with tag key filter without text
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'control_tag_value',
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-01T23:59:59Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
        Reporting::ListFilter.new(type: 'control_tag:satisfies', values: ["SRG"]), # N.B.  the control_tag filter gets ignored by the back end if it didn't, then the most you would see for suggs is what's in the filter
      ],
      size: 5
    )
    expected = [ "apache-1", "NGX-1", "NGX-2", "SRG-00006", "SRG-00007" ]
    assert_suggestions_text(expected, actual_data)
  end

  it "suggests control tag values without text and control_tag full filter" do
    # suggest control tag values with tag key filter without text
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'control_tag_value',
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-01T23:59:59Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
        Reporting::ListFilter.new(type: 'control_tag:satisfies', values: ['NGX-1', 'SRG-00006']),
      ]
    )
    expected = [ "apache-1", "apache-2", "NGX-1", "NGX-2", "SRG-00006", "SRG-00007" ]
    assert_suggestions_text(expected, actual_data)
  end

  it "suggests control tag values matching apaCH" do
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
    expected = [ "Apache" ]
    assert_suggestions_text(expected, actual_data)
  end

  it "suggests control tag values matching apa" do
    # suggest control tag values with a tag key filter without text
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
        type: 'control_tag_value',
        text: 'apa',
        filters: [
            Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-01T23:59:59Z']),
            Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
            Reporting::ListFilter.new(type: 'control_tag:scope', values: []),
        ]
    )
    expected = [ "Apache", "apalache" ]
    assert_suggestions_text(expected, actual_data)
  end

  it "suggests control tag values with missing key" do
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

  it "suggests control tag values when full control tag filter exists" do
    # suggest control tag values with a tag key filter without text
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'control_tag_value',
      type_key: 'control_tag:scope',
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-01T23:59:59Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
        Reporting::ListFilter.new(type: 'control_tag:satisfies', values: ['apache-1', 'SRG-00006']),
      ]
    )
    expected = ["Apache", "apalache", "NginX"]
    assert_suggestions_text(expected, actual_data)
  end

  it "suggests control tag values when full control tag filter exists and search text is provided but only want 2" do
    # suggest control tag values with a tag key filter without text
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
        type: 'control_tag_value',
        text: 'srg',
        size: 2,
        filters: [
            Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-01T23:59:59Z']),
            Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
            Reporting::ListFilter.new(type: 'control_tag:satisfies', values: ['apache-1', 'SRG-00006']),
            Reporting::ListFilter.new(type: 'control_tag:scope', values: []),
        ]
    )
    expected = ["SRG-00006", "SRG-00007"]
    assert_suggestions_text(expected, actual_data)
  end
end
