##### GRPC SETUP #####
require 'interservice/compliance/reporting/reporting_pb'
require 'interservice/compliance/reporting/reporting_services_pb'

describe File.basename(__FILE__) do
  Reporting = Chef::Automate::Domain::Compliance::Reporting unless defined?(Reporting)

  def reporting
    Reporting::ReportingService;
  end

  it "no filters gets them all filter" do
    actual_data = GRPC reporting, :list_control_items, Reporting::ControlItemRequest.new(
        filters: [
            Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
        ]
    )
    control_item_array = actual_data['control_items']
    assert_equal(18, control_item_array.size)
  end

  it "suggests control tag keys matching 'scope'" do
    actual_data = GRPC reporting, :list_control_items, Reporting::ControlItemRequest.new(
        filters: [
            Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-01T23:59:59Z']),
            Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
        ],
        size: 10
    )
    control_item_array = actual_data['control_items']
    assert_equal('apache-01', control_item_array[0]['id'])
    assert_equal('DevSec Apache Baseline', control_item_array[0]['profile']['title'])
    assert_equal(10, control_item_array.size)
  end

  it "control list items with a size of 2" do
    actual_data = GRPC reporting, :list_control_items, Reporting::ControlItemRequest.new(
        filters: [
            Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-01T23:59:59Z']),
            Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
        ],
        size: 2
    )

    expected_data = {
        "controlItems" => [
            {
                "id" => "apache-01",
                "title" => "Apache should be running",
                "profile" => {
                    "title" => "DevSec Apache Baseline",
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "version" => "2.0.1"
                }, "impact" => 1,
                "endTime" => "2018-03-04T00:00:10Z",
                "controlSummary" => {
                    "total" => 5,
                    "passed" => {},
                    "skipped" => {
                        "total" => 5
                    },
                    "failed" => {},
                    "waived" => {}
                }
            },
            {
                "id" => "apache-02",
                "title" => "Apache should be enabled",
                "profile" => {
                    "title" => "DevSec Apache Baseline",
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "version" => "2.0.1"
                },
                "impact" => 1,
                "endTime" => "2018-03-04T00:00:10Z",
                "controlSummary" => {
                    "total" => 5,
                    "passed" => {},
                    "skipped" => {
                        "total" => 4
                    },
                    "failed" => {
                        "total" => 1,
                        "critical" => 1
                    },
                    "waived" => {}
                }
            }
        ],
        "controlSummaryTotals" =>
            {
                "total" => 86,
                "passed" => {
                    "total" => 7
                },
                "skipped" => {
                    "total" => 73
                },
                "failed" => {
                    "total" => 6
                },
                "waived" => {}
            }
    }
    control_item_array = actual_data['control_items']
    assert_equal(2, control_item_array.size)
    assert_equal_json_content(expected_data, actual_data)
  end


  it "control list items with a size of 2 with nginx profile" do
    actual_data = GRPC reporting, :list_control_items, Reporting::ControlItemRequest.new(
        filters: [
            Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-01T23:59:59Z']),
            Reporting::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
            Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
            Reporting::ListFilter.new(type: "platform", values: ["centos"])

        ],
        size: 2
    )
    expected_data = {
        "controlItems" => [
            {
                "id" => "nginx-01",
                "title" => "Running worker process as non-privileged user",
                "profile" => {
                    "title" => "DevSec Nginx Baseline",
                    "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                    "version" => "2.1.0"
                },
                "impact" => 1,
                "endTime" => "2018-03-04T00:00:10Z",
                "controlSummary" => {
                    "total" => 1,
                    "passed" => {
                        "total" => 1
                    },
                    "skipped" => {},
                    "failed" => {},
                    "waived" => {}
                }
            },
            {
                "id" => "nginx-02",
                "title" => "Check NGINX config file owner, group and permissions.",
                "profile" => {
                    "title" => "DevSec Nginx Baseline",
                    "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                    "version" => "2.1.0"
                },
                "impact" => 1,
                "endTime" => "2018-03-04T00:00:10Z",
                "controlSummary" => {
                    "total" => 1,
                    "passed" => {
                        "total" => 1
                    },
                    "skipped" => {},
                    "failed" => {},
                    "waived" => {}
                }
            }
        ],
        "controlSummaryTotals" =>
            {
                "total" => 4,
                "passed" => {
                    "total" => 3
                },
                "skipped" => {
                    "total" => 1
                },
                "failed" => {},
                "waived" => {}
            }
    }

    control_item_array = actual_data['control_items']
    assert_equal(2, control_item_array.size)
    assert_equal_json_content(expected_data, actual_data)
  end

  it "control list items with a size of 2 but only care about skipped" do
    actual_data = GRPC reporting, :list_control_items, Reporting::ControlItemRequest.new(
        filters: [
            Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-01T23:59:59Z']),
            Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
            Reporting::ListFilter.new(type: 'control_status', values: ['skipped']),
        ],
        size: 2
    )

    expected_data = {
        "controlItems" => [
            {
                "id" => "apache-01",
                "title" => "Apache should be running",
                "profile" => {
                    "title" => "DevSec Apache Baseline",
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "version" => "2.0.1"
                }, "impact" => 1,
                "endTime" => "2018-03-04T00:00:10Z",
                "controlSummary" => {
                    "total" => 5,
                    "passed" => {},
                    "skipped" => {
                        "total" => 5
                    },
                    "failed" => {},
                    "waived" => {}
                }
            },
            {
                "id" => "apache-02",
                "title" => "Apache should be enabled",
                "profile" => {
                    "title" => "DevSec Apache Baseline",
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "version" => "2.0.1"
                },
                "impact" => 1,
                "endTime" => "2018-03-04T00:00:10Z",
                "controlSummary" => {
                    "total" => 4,
                    "passed" => {},
                    "skipped" => {
                        "total" => 4
                    },
                    "failed" => {},
                    "waived" => {}
                }
            }
        ],
        "controlSummaryTotals" => {
            "total" => 73,
            "passed" => {},
            "skipped" => {
                "total" => 73
            },
            "failed" => {},
            "waived" => {}
        }
    }

    control_item_array = actual_data['control_items']
    assert_equal(2, control_item_array.size)
    assert_equal_json_content(expected_data, actual_data)
  end

  it "control list items with a size of 2 but only care about failed" do
    actual_data = GRPC reporting, :list_control_items, Reporting::ControlItemRequest.new(
        filters: [
            Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-01T23:59:59Z']),
            Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
            Reporting::ListFilter.new(type: 'control_status', values: ['failed']),
        ],
        size: 2
    )

    expected_data = {
        "controlItems" => [
            {
                "id" => "apache-02",
                "title" => "Apache should be enabled",
                "profile" => {
                    "title" => "DevSec Apache Baseline",
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "version" => "2.0.1"
                },
                "impact" => 1,
                "endTime" => "2018-03-04T09:19:42Z",
                "controlSummary" => {
                    "total" => 1,
                    "passed" => {},
                    "skipped" => {},
                    "failed" => {
                        "total" => 1,
                        "critical" => 1
                    },
                    "waived" => {}
                }
            },
            {
                "id" => "nginx-01",
                "title" => "Running worker process as non-privileged user",
                "profile" => {
                    "title" => "DevSec Nginx Baseline",
                    "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                    "version" => "2.1.0"
                },
                "impact" => 1,
                "endTime" => "2018-03-04T09:18:42Z",
                "controlSummary" => {
                    "total" => 2,
                    "passed" => {},
                    "skipped" => {},
                    "failed" => {
                        "total" => 2,
                        "critical" => 2
                    },
                    "waived" => {}
                }
            }
        ],
        "controlSummaryTotals" => {
            "total" => 6,
            "passed" => {},
            "skipped" => {},
            "failed" => {"total" => 6},
            "waived" => {}
        }
    }

    control_item_array = actual_data['control_items']
    assert_equal(2, control_item_array.size)
    assert_equal_json_content(expected_data, actual_data)
  end

  it "control list items with a size of 2 but only care about failed" do
    actual_data = GRPC reporting, :list_control_items, Reporting::ControlItemRequest.new(
        filters: [
            Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-01T23:59:59Z']),
            Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
            Reporting::ListFilter.new(type: 'control_tag:satisfies', values: ['NGX-1', 'SRG-00006'])
        ],
        size: 2
    )

    expected_data = {
        "controlItems" => [
            {
                "id" => "nginx-01",
                "title" => "Running worker process as non-privileged user",
                "profile" => {
                    "title" => "DevSec Nginx Baseline",
                    "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                    "version" => "2.1.0"
                },
                "impact" => 1,
                "endTime" => "2018-03-04T09:18:43Z",
                "controlSummary" => {
                    "total" => 1,
                    "passed" => {},
                    "skipped" => {},
                    "failed" => {
                        "total" => 1,
                        "critical" => 1
                    },
                    "waived" => {}
                }
            }
        ],
        "controlSummaryTotals" => {
            "total" => 1,
            "passed" => {},
            "skipped" => {},
            "failed" => {
                "total" => 1
            },
            "waived" => {}
        }
    }
    control_item_array = actual_data['control_items']
    assert_equal(1, control_item_array.size)
    assert_equal_json_content(expected_data, actual_data)
  end

  it "control list items with a size of 4 filtered by control_tag" do
    actual_data = GRPC reporting, :list_control_items, Reporting::ControlItemRequest.new(
        filters: [
            Reporting::ListFilter.new(type: 'start_time', values: ['2018-03-01T23:59:59Z']),
            Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
            Reporting::ListFilter.new(type: 'control_tag:scoop', values: ['icecream'])
        ],
        size: 4
    )
    expected_data = {
        "controlItems" =>
            [
                {
                    "id" => "apache-01",
                    "title" => "Apache should be running",
                    "profile" => {
                        "title" => "DevSec Apache Baseline",
                        "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                        "version" => "2.0.1"
                    },
                    "impact" => 1,
                    "endTime" => "2018-03-04T00:00:10Z",
                    "controlSummary" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {
                            "total" => 1
                        },
                        "failed" => {},
                        "waived" => {}
                    }
                }
            ],
        "controlSummaryTotals" => {
            "total" => 1,
            "passed" => {},
            "skipped" => {
                "total" => 1
            },
            "failed" => {},
            "waived" => {}
        }
    }
    assert_equal_json_content(expected_data, actual_data)
  end


  it "control list items with a size of 4 filtered by control_tag" do
    actual_data = GRPC reporting, :list_control_items, Reporting::ControlItemRequest.new(
        filters: [
            Reporting::ListFilter.new(type: 'start_time', values: ['2018-03-01T23:59:59Z']),
            Reporting::ListFilter.new(type: 'end_time', values: ['2018-04-01T23:59:59Z']),
            Reporting::ListFilter.new(type: "control", values: ["pro1-con3"]),

        ],
        size: 4
    )
    expected_data = {
        "controlItems" =>
            [
                {
                    "id" => "pro1-con3",
                    "title" => "Profile 1 - Control 3",
                    "profile" => {
                        "title" => "My Profile 1 title",
                        "id" => "447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4",
                        "version" => "1.0.1"
                    },
                    "impact" => 1,
                    "endTime" => "2018-04-01T10:18:41Z",
                    "controlSummary" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {},
                        "failed" => {
                            "total" => 1,
                            "critical" => 1
                        },
                        "waived" => {}
                    }
                }
            ],
        "controlSummaryTotals" => {
            "total" => 1,
            "passed" => {},
            "skipped" => {},
            "failed" => {
                "total" => 1
            },
            "waived" => {}
        }
    }

    assert_equal_json_content(expected_data, actual_data)
  end

  it "control list items filtered by environment and platform" do
    actual_data = GRPC reporting, :list_control_items, Reporting::ControlItemRequest.new(
        filters: [
            Reporting::ListFilter.new(type: 'start_time', values: ['2018-03-01T23:59:59Z']),
            Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
            Reporting::ListFilter.new(type: 'environment', values: ['DevSec Prod Zeta', 'missing']),
            Reporting::ListFilter.new(type: 'platform', values: ['windows', 'missing'])
        ],
        size: 1000
    )
    control_item_array = actual_data['control_items']
    assert_equal(14, control_item_array.size)
  end

  it "control list items on 2018-04-01" do
    actual_data = GRPC reporting, :list_control_items, Reporting::ControlItemRequest.new(
        filters: [
            Reporting::ListFilter.new(type: 'start_time', values: ['2017-04-01T00:00:00Z']),
            Reporting::ListFilter.new(type: 'end_time', values: ['2018-04-01T23:59:59Z'])
        ],
        size: 10
    )

    expected_data = {
        "controlItems" =>
            [
                {
                    "id" => "pro1-con1",
                    "title" => "Profile 1 - Control 1",
                    "profile" => {
                        "title" => "My Profile 1 title",
                        "id" => "447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4",
                        "version" => "1.0.1"
                    },
                    "impact" => 0.80000001,
                    "endTime" => "2018-04-01T10:18:41Z",
                    "controlSummary" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {},
                        "failed" => {},
                        "waived" => {
                            "total" => 1
                        }
                    },
                    "waivers" =>
                        [{"waivedStr" => "yes_run",
                          "justification" => "Sound reasoning",
                          "waiverSummary" =>
                              {
                                  "passed" => {},
                                  "skipped" => {},
                                  "failed" => {
                                      "total" => 1
                                  },
                                  "waived" => {
                                      "total" => 1
                                  }
                              }
                         }
                        ]
                },
                {
                    "id" => "pro1-con2",
                    "title" => "Profile 1 - Control 2",
                    "profile" => {
                        "title" => "My Profile 1 title",
                        "id" => "447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4",
                        "version" => "1.0.1"
                    },
                    "impact" => 0.89999998,
                    "endTime" => "2018-04-01T10:18:41Z",
                    "controlSummary" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {},
                        "failed" => {},
                        "waived" => {
                            "total" => 1
                        }
                    },
                    "waivers" =>
                        [
                            {
                                "waivedStr" => "yes_run",
                                "justification" => "Sheer cleverness",
                                "waiverSummary" =>
                                    {
                                        "passed" => {},
                                        "skipped" => {
                                            "total" => 1
                                        },
                                        "failed" => {},
                                        "waived" => {
                                            "total" => 1
                                        }
                                    }
                            }
                        ]
                },
                {
                    "id" => "pro1-con3",
                    "title" => "Profile 1 - Control 3",
                    "profile" => {
                        "title" => "My Profile 1 title",
                        "id" => "447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4",
                        "version" => "1.0.1"
                    },
                    "impact" => 1,
                    "endTime" => "2018-04-01T10:18:41Z",
                    "controlSummary" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {},
                        "failed" => {
                            "total" => 1,
                            "critical" => 1
                        },
                        "waived" => {}
                    }
                },
                {
                    "id" => "pro1-con4",
                    "title" => "Profile 1 - Control 4",
                    "profile" => {
                        "title" => "My Profile 1 title",
                        "id" => "447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4",
                        "version" => "1.0.1"
                    },
                    "impact" => 0.89999998,
                    "endTime" => "2018-04-01T10:18:41Z",
                    "controlSummary" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {},
                        "failed" => {},
                        "waived" => {
                            "total" => 1
                        }
                    },
                    "waivers" =>
                        [
                            {
                                "waivedStr" => "yes",
                                "expirationDate" => "2025-06-01",
                                "justification" => "Whimsy",
                                "waiverSummary" =>
                                    {
                                        "passed" => {},
                                        "skipped" => {
                                            "total" => 1
                                        },
                                        "failed" => {},
                                        "waived" => {
                                            "total" => 1
                                        }
                                    }
                            }
                        ]
                },
                {
                    "id" => "pro1-con5",
                    "title" => "Profile 1 - Control 5",
                    "profile" => {
                        "title" => "My Profile 1 title",
                        "id" => "447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4",
                        "version" => "1.0.1"
                    },
                    "impact" => 0.89999998,
                    "endTime" => "2018-04-01T10:18:41Z",
                    "controlSummary" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {},
                        "failed" => {
                            "total" => 1,
                            "critical" => 1
                        },
                        "waived" => {}
                    }
                },
                {
                    "id" => "pro2-con1",
                    "title" => "Profile 2 - Control 1",
                    "profile" => {
                        "title" => "My Profile 2 title",
                        "id" => "447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea5",
                        "version" => "1.0.5"
                    },
                    "impact" => 0.80000001,
                    "endTime" => "2018-04-01T10:18:41Z",
                    "controlSummary" => {
                        "total" => 2,
                        "passed" => {},
                        "skipped" => {},
                        "failed" => {},
                        "waived" => {
                            "total" => 2
                        }
                    },
                    "waivers" =>
                        [
                            {
                                "waivedStr" => "yes_run",
                                "justification" => "Sound reasoning",
                                "waiverSummary" =>
                                    {
                                        "passed" => {},
                                        "skipped" => {},
                                        "failed" => {
                                            "total" => 2
                                        },
                                        "waived" => {
                                            "total" => 2
                                        }
                                    }
                            }
                        ]
                }
            ],
        "controlSummaryTotals" => {
            "total" => 7,
            "passed" => {},
            "skipped" => {},
            "failed" => {
                "total" => 2
            },
            "waived" => {
                "total" => 5
            }
        }
    }
    assert_equal_json_content(expected_data, actual_data)
  end

  it "control list items on the day without profiles" do
    actual_data = GRPC reporting, :list_control_items, Reporting::ControlItemRequest.new(
        filters: [
            Reporting::ListFilter.new(type: 'start_time', values: ['2017-04-03T00:00:00Z']),
            Reporting::ListFilter.new(type: 'end_time', values: ['2018-04-03T23:59:59Z'])
        ],
        size: 10
    )
    expected_data = {
      "controlSummaryTotals" => {
        "passed" => {},
        "skipped" => {},
        "failed" => {},
        "waived" => {}
      }
    }
    assert_equal_json_content(expected_data, actual_data)
  end

  it "control list items without time filters (last 24h search)" do
    actual_data = GRPC reporting, :list_control_items, Reporting::ControlItemRequest.new(
        filters: [],
        size: 10
    )
    actual_data_hash = actual_data.to_h
    actual_data_hash[:control_items].each { |c| c[:end_time] = 'SOMETIME_IN_THE_LAST_24H' }

    expected_data = {
      "control_items": [
        {
          "id": "pro1-con1",
          "title": "Profile 1 - Control 1",
          "profile": {
            "name": "",
            "title": "My Profile 1 title",
            "id": "447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4",
            "version": "1.0.1",
            "status": ""
          },
          "impact": 0.800000011920929,
          "end_time": "SOMETIME_IN_THE_LAST_24H",
          "control_summary": {
            "total": 1,
            "passed": {
              "total": 1
            },
            "skipped": {
              "total": 0
            },
            "failed": {
              "total": 0,
              "minor": 0,
              "major": 0,
              "critical": 0
            },
            "waived": {
              "total": 0
            }
          },
          "waivers": []
        },
        {
          "id": "pro1-con2",
          "title": "Profile 1 - Control 2",
          "profile": {
            "name": "",
            "title": "My Profile 1 title",
            "id": "447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4",
            "version": "1.0.1",
            "status": ""
          },
          "impact": 0.8999999761581421,
          "end_time": "SOMETIME_IN_THE_LAST_24H",
          "control_summary": {
            "total": 1,
            "passed": {
              "total": 0
            },
            "skipped": {
              "total": 1
            },
            "failed": {
              "total": 0,
              "minor": 0,
              "major": 0,
              "critical": 0
            },
            "waived": {
              "total": 0
            }
          },
          "waivers": []
        },
        {
          "id": "pro1-con3",
          "title": "Profile 1 - Control 3",
          "profile": {
            "name": "",
            "title": "My Profile 1 title",
            "id": "447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4",
            "version": "1.0.1",
            "status": ""
          },
          "impact": 1.0,
          "end_time": "SOMETIME_IN_THE_LAST_24H",
          "control_summary": {
            "total": 1,
            "passed": {
              "total": 1
            },
            "skipped": {
              "total": 0
            },
            "failed": {
              "total": 0,
              "minor": 0,
              "major": 0,
              "critical": 0
            },
            "waived": {
              "total": 0
            }
          },
          "waivers": []
        },
        {
          "id": "pro1-con4",
          "title": "Profile 1 - Control 4",
          "profile": {
            "name": "",
            "title": "My Profile 1 title",
            "id": "447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4",
            "version": "1.0.1",
            "status": ""
          },
          "impact": 0.8999999761581421,
          "end_time": "SOMETIME_IN_THE_LAST_24H",
          "control_summary": {
            "total": 1,
            "passed": {
              "total": 1
            },
            "skipped": {
              "total": 0
            },
            "failed": {
              "total": 0,
              "minor": 0,
              "major": 0,
              "critical": 0
            },
            "waived": {
              "total": 0
            }
          },
          "waivers": []
        },
        {
          "id": "pro1-con5",
          "title": "Profile 1 - Control 5",
          "profile": {
            "name": "",
            "title": "My Profile 1 title",
            "id": "447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4",
            "version": "1.0.1",
            "status": ""
          },
          "impact": 0.8999999761581421,
          "end_time": "SOMETIME_IN_THE_LAST_24H",
          "control_summary": {
            "total": 1,
            "passed": {
              "total": 1
            },
            "skipped": {
              "total": 0
            },
            "failed": {
              "total": 0,
              "minor": 0,
              "major": 0,
              "critical": 0
            },
            "waived": {
              "total": 0
            }
          },
          "waivers": []
        }
      ],
      "control_summary_totals": {
        "total": 5,
        "passed": {
          "total": 4
        },
        "skipped": {
          "total": 1
        },
        "failed": {
          "total": 0,
          "minor": 0,
          "major": 0,
          "critical": 0
        },
        "waived": {
          "total": 0
        }
      }
    }
    assert_equal_json_content(expected_data, actual_data_hash)
  end
end
