export let mockPlatforms = {
  'children': [
    {
      'count': 23,
      'name': 'centos'
    },
    {
      'count': 22,
      'name': 'debian'
    },
    {
      'count': 23,
      'name': 'other'
    },
    {
      'count': 22,
      'name': 'more-i-say'
    },
    {
      'count': 22,
      'name': 'lorax'
    }
  ]
};

export let mockEnvironments = {
  'children': [
    {
      'name': 'alpha',
      'count': 23
    },
    {
      'name': 'zeta',
      'count': 22
    },
    {
      'name': 'beta',
      'count': 22
    }
  ]
};

export let mockProfiles = {
  'children': [
    {
      'count': 45,
      'id': 'b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015',
      'name': 'linux-baseline'
    },
    {
      'count': 0,
      'id': '65707cb4299e5e821c687f6d5a704ffd3e21f6139a9ad0cc3b438c343b129d8c',
      'name': 'apache-baseline'
    },
    {
      'count': 16,
      'id': '3984753145f0db693e2c6fc79f764e9aff78d892a874391fc5f5cc18f4675b68',
      'name': 'ssh-baseline'
    }
  ]
};

export let mockControls = {
  'children': [
    {
      'count': 45,
      'name': 'tmp-1.0',
      'id': 'linux-baseline'
    },
    {
      'count': 10,
      'name': 'tmp-2.0',
      'id': 'apache-baseline'
    },
    {
      'count': 2,
      'name': 'tmp-3.0',
      'id': 'ssh-baseline'
    }
  ]
};

export let mockSummaryData: {} = {
  status: 'Not Compliant',
  target: ['targets'],
  date: 'date',
  duration: 'duration',
  platform: ['platforms'],
  env: ['envs'],
  os: ['os'],
  profiles: ['profiles']
};

export let mockNodesData: Array<{}> = [
  {
    'node_id': '74a54a28-c628-4f82-86df-61c43866db6a',
    'node_name': 'teal-spohn',
    'environment': 'alpha',
    'end_time': '2017-04-04T10:18:41+01:00',
    'platform': 'centos',
    'failed': {'total': 23, 'minor': 0, 'major': 0, 'critical': 23}
  },
  {
    'node_id': '99516108-8126-420e-b03e-a90a52f25751',
    'node_name': 'red-brentwood',
    'environment': 'zeta',
    'end_time': '2017-03-06T09:18:41Z',
    'platform': 'debian',
    'failed': {'total': 22, 'minor': 0, 'major': 0, 'critical': 22}
  }
];

export let mockProfilesData: Array<{}> = [
  {
    'name': 'linux-baseline',
    'id': 'b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015',
    'failures': 45,
    'majors': 0,
    'minors': 0,
    'criticals': 45,
    'passed': 45,
    'skipped': 0
  },
  {
    'name': 'apache-baseline',
    'id': '65707cb4299e5e821c687f6d5a704ffd3e21f6139a9ad0cc3b438c343b129d8c',
    'failures': 0,
    'majors': 0,
    'minors': 0,
    'criticals': 0,
    'passed': 0,
    'skipped': 14
  },
  {
    'name': 'ssh-baseline',
    'id': '3984753145f0db693e2c6fc79f764e9aff78d892a874391fc5f5cc18f4675b68',
    'failures': 0,
    'majors': 0,
    'minors': 0,
    'criticals': 0,
    'passed': 0,
    'skipped': 68
  }
];

const data = {
  'compliant': 0,
  'high_risk': 2,
  'low_risk': 0,
  'medium_risk': 0,
  'noncompliant': 2,
  'skipped': 0
};

export let mockRadialData = {
  innerCircle: [{
      'displayName': 'Failed',
      'name': 'orange',
      'count': data.noncompliant
  }, {
      'displayName': 'Passed',
      'name': 'blue',
      'count': data.compliant
  }, {
      'displayName': 'Skipped',
      'name': 'gray',
      'count': data.skipped
  }],
  outerCircle: [{
      'displayName': 'High Risk',
      'name': 'orange',
      'count': data.high_risk
  }, {
      'displayName': 'Medium Risk',
      'name': 'yellow',
      'count': data.medium_risk
  }, {
      'displayName': 'Low Risk',
      'name': 'ltblue',
      'count': data.low_risk
  }]
};

export let mockTrendData = [{
      'date': '20111001',
      'Failed': '120',
      'Passed': '60',
      'Skipped': '20'
  }, {
      'date': '20111002',
      'Failed': '180',
      'Passed': '40',
      'Skipped': '10'
  }, {
      'date': '20111003',
      'Failed': '170',
      'Passed': '125',
      'Skipped': '5'
  },
  {
      'date': '20111004',
      'Failed': '130',
      'Passed': '180',
      'Skipped': '32'
  },
  {
      'date': '20111005',
      'Failed': '140',
      'Passed': '120',
      'Skipped': '20'
  }
];

export let mockProfileWithResults = {
  'name': 'profile-attribute',
  'title': 'InSpec Profile',
  'maintainer': 'The Authors',
  'copyright': 'The Authors',
  'copyright_email': 'you@example.com',
  'license': 'All Rights Reserved',
  'summary': 'An InSpec Compliance Profile',
  'version': '0.1.0',
  'supports': [],
  'controls': [
    {
      'title': null,
      'impact': 0.5,
      'results': {
        'failedCount': 1,
        'passedCount': 0,
        'skippedCount': 2
      }
    },
    {
      'title': null,
      'desc': null,
      'impact': 0.8,
      'results': {
        'failedCount': 1,
        'passedCount': 5,
        'skippedCount': 0
      }
  }]
};


export let layerOne = {
  'id': '123',
  'failed' : ['profile-id', 'profile-id'],
  'passed' : ['profile-id', 'profile-id']
};

export let layerTwo = {
  'id': '123',
  'failed' : ['control-id', 'control-id'],
  'passed' : ['control-id', 'control-id']
};

export let control = {
  'title': 'Verify the version number of Gordon',
  'desc': 'An optional description...',
  'impact': 0.7,
  'refs': [
    {
      'uri': 'http:\/\/...',
      'ref': 'Gordon Requirements 1.0'
    }
  ],
  'tags': {
    'gordon': null
  },
  // tslint:disable-next-line:max-line-length
  'code': 'control "gordon-1.0" do\n  impact 0.7\n  title "Verify the version number of Gordon"\n  desc "An optional description..."\n  tag "gordon"\n  ref "Gordon Requirements 1.0", uri: "http:\/\/..."\n\n  # Test using the custom gordon_config Inspec resource\n  # Find the resource content here: ..\/libraries\/\n  describe gordon_config do\n    it { should exist }\n    its("version") { should eq("1.0") }\n    its("file_size") { should <= 20 }\n    its("comma_count") { should eq 0 }\n  end\n\n  # Test the version again to showcase variables\n  g = gordon_config\n  g_path = g.file_path\n  g_version = g.version\n  describe file(g_path) do\n    its("content") { should match g_version }\n  end\nend\n',
  'source_location': {
    'ref': '\/Users\/vjeffrey\/code\/compliance\/inspec\/examples\/profile\/controls\/gordon.rb',
    'line': 14
  },
  'id': 'gordon-1.0',
  'results': [
    {
      'status': 'skipped',
      'code_desc': 'gordon_config',
      'skip_message': 'Can\'t find file \"\/tmp\/gordon\/config.yaml\"',
      'resource': 'gordon_config',
      'run_time': 8.0e-6,
      'start_time': '2017-04-17 09:05:58 -0600'
    },
    {
      'status': 'passed',
      'code_desc': 'File  content should match nil',
      'run_time': 0.000849,
      'start_time': '2017-04-17 09:05:58 -0600'
    },
    {
      'status': 'failed',
      'code_desc': 'alice should eq \'bob\'',
      'run_time': 0.009641,
      'start_time': '2017-04-17 09:15:35 -0600',
      'message': '\nexpected: \'bob\'\n     got: \'alice\'\n\n(compared using ==)\n'
    }
  ]
};
