import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';

import { NodeRunsService } from './node-runs.service';

import { environment } from '../../../environments/environment';
const CONFIG_MGMT_URL = environment.config_mgmt_url;

describe('NodeRunsService', () => {
  let service: NodeRunsService;
  let httpTestingController: HttpTestingController;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpClientTestingModule
      ],
      providers: [
        NodeRunsService
      ]
    });

    service = TestBed.get(NodeRunsService);
    httpTestingController = TestBed.get(HttpTestingController);
  });

  describe('getPolicyCookbooks()', () => {
    it('formats policycookbooks with key value map from response', () => {
      const getPolicyCookbooksResponse = getRawPolicyCookbooks();
      const revisionId = 'someId';
      const expectedUrl = `${CONFIG_MGMT_URL}/policy_revision/${revisionId}`;

      service.getPolicyCookbooks(revisionId).then(policyCookbooks => {
        expect(policyCookbooks).toBeDefined();
        expect(policyCookbooks.policyName).toEqual('test-base');
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('GET');

      req.flush(getPolicyCookbooksResponse);
    });
  });

  describe('getNodeRun()', () => {
    it('when the node run object returned has an empty error section. ' +
      'The data should be return with no errors', () => {
      const nodeId = 'fake_id';
      const runId = 'runId';
      const endTime = new Date(0);
      const expectedUrl =
        `${CONFIG_MGMT_URL}/nodes/${nodeId}/runs/${runId}?end_time=1970-01-01T00:00:00.000Z`;
      const errorSection = {
        'class': '',
        'message': '',
        'backtrace': [],
        'description': null
      };
      const getNodeRunResponse = getRawRun(errorSection, []);

      service.getNodeRun(nodeId, runId, endTime).then(nodeRun => {
        expect(nodeRun).toBeDefined();
        expect(nodeRun.nodeId).toEqual('0271e125-97dd-498a-b026-8448ee60aafe');
        expect(nodeRun.runId).toEqual('e2d05569-dfeb-4ff0-a5a2-863ce12cc13f');
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('GET');

      req.flush(getNodeRunResponse);
    });

    it('when run\'s error description title gets updated from 412 "Precondition Failed"' +
      ' to Error Resolving Cookbooks for Run List.', () => {
      const nodeId = 'fake_id';
      const runId = 'runId';
      const endTime = new Date(0);
      const expectedUrl =
        `${CONFIG_MGMT_URL}/nodes/${nodeId}/runs/${runId}?end_time=1970-01-01T00:00:00.000Z`;

      const getNodeRunResponse = getRawRun({
        'class': '',
        'message': '',
        'backtrace': [],
        'description': {
          'title': '412 "Precondition Failed"'
        }
      }, []);

      service.getNodeRun(nodeId, runId, endTime).then(nodeRun => {
        expect(nodeRun).toBeDefined();
        expect(nodeRun.error.description.title).toEqual('Error Resolving Cookbooks for Run List.');
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('GET');

      req.flush(getNodeRunResponse);
    });

    it('the run\'s error description title replaces the ' +
      '":" at the end of the line with a ".". The title is updated from ' +
      '"Error Resolving Cookbooks for Run List:" to ' +
      '"Error Resolving Cookbooks for Run List."', () => {
      const nodeId = 'fake_id';
      const runId = 'runId';
      const endTime = new Date(0);
      const expectedUrl =
        `${CONFIG_MGMT_URL}/nodes/${nodeId}/runs/${runId}?end_time=1970-01-01T00:00:00.000Z`;

      const getNodeRunResponse = getRawRun({
        'class': '',
        'message': '',
        'backtrace': [],
        'description': {
          'title': 'Error Resolving Cookbooks for Run List:'
        }
      }, []);

      service.getNodeRun(nodeId, runId, endTime).then(nodeRun => {
        expect(nodeRun).toBeDefined();
        expect(nodeRun.error.description.title).toEqual('Error Resolving Cookbooks for Run List.');
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('GET');

      req.flush(getNodeRunResponse);
    });

    it('when deprecations are not provided', () => {
      const nodeId = 'fake_id';
      const runId = 'runId';
      const endTime = new Date(0);
      const expectedUrl =
        `${CONFIG_MGMT_URL}/nodes/${nodeId}/runs/${runId}?end_time=1970-01-01T00:00:00.000Z`;
      const getNodeRunResponse = getRawRun([], []);

      service.getNodeRun(nodeId, runId, endTime).then(nodeRun => {
        expect(nodeRun).toBeDefined();
        expect(nodeRun.deprecations.length).toEqual(0);
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('GET');

      req.flush(getNodeRunResponse);
    });

    it('when deprecations are provided', () => {
      const nodeId = 'fake_id';
      const runId = 'runId';
      const endTime = new Date(0);
      const expectedUrl =
        `${CONFIG_MGMT_URL}/nodes/${nodeId}/runs/${runId}?end_time=1970-01-01T00:00:00.000Z`;
      const deprecations = [
          {
            'message': 'rename version to new_resource.version',
            'url': 'https://docs.chef.io/deprecations_namespace_collisions.html',
            'location': '/var/chef/cache/cookbooks/audit/resources/inspec_gem.rb:12:in ' +
              '"block in class_from_file"'
          },
          {
            'message': 'rename version to new_resource.version',
            'url': 'https://docs.chef.io/deprecations_namespace_collisions.html',
            'location':
              '/var/chef/cache/cookbooks/audit/resources/inspec_gem.rb:18:in' +
                '"block in class_from_file"'
          },
          {
            'message': 'rename file to new_resource.file',
            'url': 'https://docs.chef.io/deprecations_namespace_collisions.html',
            'location': '/var/chef/cache/cookbooks/patch/resources/append_line.rb:12:in' +
              '"block in class_from_file"'
          },
          {
            'message': 'rename path to new_resource.path',
            'url': 'https://docs.chef.io/deprecations_namespace_collisions.html',
            'location': '/var/chef/cache/cookbooks/patch/resources/append_line.rb:12:in' +
              '"block in class_from_file"'
          },
          {
            'message': 'rename name to new_resource.name',
            'url': 'https://docs.chef.io/deprecations_namespace_collisions.html',
            'location': '/var/chef/cache/cookbooks/patch/resources/append_line.rb:12:in' +
              '"block in class_from_file"'
          },
          {
            'message': 'rename line to new_resource.line',
            'url': 'https://docs.chef.io/deprecations_namespace_collisions.html',
            'location': '/var/chef/cache/cookbooks/patch/resources/append_line.rb:15:in' +
              '"block in class_from_file"'
          }
      ];
      const getNodeRunResponse = getRawRun([], deprecations);
      service.getNodeRun(nodeId, runId, endTime).then(nodeRun => {
        expect(nodeRun).toBeDefined();
        expect(nodeRun.deprecations.length).toEqual(6);
        expect(nodeRun.deprecations[0].message).toEqual('rename version to new_resource.version');
        expect(nodeRun.deprecations[0].url).toEqual(
          'https://docs.chef.io/deprecations_namespace_collisions.html');
        expect(nodeRun.deprecations[0].location).toEqual(
          '/var/chef/cache/cookbooks/audit/resources/inspec_gem.rb:12:in ' +
          '"block in class_from_file"');

        expect(nodeRun.deprecations[3].message).toEqual('rename path to new_resource.path');
        expect(nodeRun.deprecations[3].url).toEqual(
          'https://docs.chef.io/deprecations_namespace_collisions.html');
        expect(nodeRun.deprecations[3].location).toEqual(
          '/var/chef/cache/cookbooks/patch/resources/append_line.rb:12:in' +
          '"block in class_from_file"');

        expect(nodeRun.deprecations[5].message).toEqual('rename line to new_resource.line');
        expect(nodeRun.deprecations[5].url).toEqual(
          'https://docs.chef.io/deprecations_namespace_collisions.html');
        expect(nodeRun.deprecations[5].location).toEqual(
          '/var/chef/cache/cookbooks/patch/resources/append_line.rb:15:in' +
          '"block in class_from_file"');
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('GET');

      req.flush(getNodeRunResponse);
    });

  });
});

function getRawPolicyCookbooks(): any {
  return {
    'policy_name': 'test-base',
    'cookbook_locks': [
       {
          'cookbook': 'chef-test',
          'policy_identifier': '1a5264faffcd39113a76d6bf5a29f8f281be405b'
       },
       {
          'cookbook': 'chef_client_updater',
          'policy_identifier': '99ca43268ffca4d9c78fa8cbf28c3565f397b161'
       }
    ]
  };
}

function getRawRun(errorSection: any, deprecations: any): any {
  return {
    'id': 'e2d05569-dfeb-4ff0-a5a2-863ce12cc13f',
    'node_id': '0271e125-97dd-498a-b026-8448ee60aafe',
    'node_name': 'insights.chef.co',
    'organization': 'chef_delivery',
    'start_time': '2016-06-28T15:13:21Z',
    'end_time': '2016-06-28T15:13:22Z',
    'source': 'chef_delivery',
    'status': 'success',
    'total_resource_count': 10,
    'updated_resource_count': 4,
    'chef_version': '12.6.0',
    'uptime_seconds': 12342607,
    'environment': 'wilson',
    'fqdn': 'ip-10-194-15-22.us-west-2.compute.internal',
    'source_fqdn': 'chef-server.insights.co',
    'ipaddress': '10.194.15.22',
    'resources': [
       {
          'type': 'file',
          'name': '/tmp/test.txt',
          'id': '/tmp/test.txt',
          'duration': '0',
          'delta': '',
          'cookbook_name': 'insights-test',
          'cookbook_version': '0.1.1',
          'status': 'skipped',
          'recipe_name': '',
          'result': '',
          'conditional': 'not_if { action == :nothing }',
          'ignore_failure': false
       },
       {
          'type': 'execute',
          'name': 'ls',
          'id': 'ls',
          'duration': '16',
          'delta': '',
          'cookbook_name': 'insights-test',
          'cookbook_version': '0.1.1',
          'status': 'updated',
          'recipe_name': '',
          'result': '',
          'conditional': '',
          'ignore_failure': false
       },
       {
          'type': 'file',
          'name': '/tmp/test.txt',
          'id': '/tmp/test.txt',
          'duration': '9',
          'delta': '',
          'cookbook_name': 'insights-test',
          'cookbook_version': '0.1.1',
          'status': 'updated',
          'recipe_name': '',
          'result': '',
          'conditional': '',
          'ignore_failure': false
       },
       {
          'type': 'execute',
          'name': 'ls -l',
          'id': 'ls -l',
          'duration': '11',
          'delta': '',
          'cookbook_name': 'insights-test',
          'cookbook_version': '0.1.1',
          'status': 'updated',
          'recipe_name': '',
          'result': '',
          'conditional': '',
          'ignore_failure': false
       },
       {
          'type': 'file',
          'name': '/tmp/test.txt',
          'id': '/tmp/test.txt',
          'duration': '10',
          'delta': '',
          'cookbook_name': 'insights-test',
          'cookbook_version': '0.1.1',
          'status': 'up-to-date',
          'recipe_name': '',
          'result': '',
          'conditional': '',
          'ignore_failure': false
       },
       {
          'type': 'file',
          'name': '/tmp/always-updated.txt',
          'id': '/tmp/always-updated.txt',
          'duration': '8',
          'delta': '',
          'cookbook_name': 'insights-test',
          'cookbook_version': '0.1.1',
          'status': 'updated',
          'recipe_name': '',
          'result': '',
          'conditional': '',
          'ignore_failure': false
       },
       {
          'type': 'file',
          'name': '/failed/file/resource',
          'id': '/failed/file/resource',
          'duration': '0',
          'delta': '',
          'cookbook_name': 'insights-test',
          'cookbook_version': '0.1.1',
          'status': 'skipped',
          'recipe_name': '',
          'result': '',
          'conditional': 'not_if { #code block }',
          'ignore_failure': false
       },
       {
          'type': 'file',
          'name': '/tmp/do-not-write.txt',
          'id': '/tmp/do-not-write.txt',
          'duration': '0',
          'delta': '',
          'cookbook_name': 'insights-test',
          'cookbook_version': '0.1.1',
          'status': 'skipped',
          'recipe_name': '',
          'result': '',
          'conditional': 'not_if { #code block }',
          'ignore_failure': false
       },
       {
          'type': 'file',
          'name': '/path/does/not/exist/but/we/will/ignore/this/failure.txt',
          'id': '/path/does/not/exist/but/we/will/ignore/this/failure.txt',
          'duration': '0',
          'delta': '',
          'cookbook_name': 'insights-test',
          'cookbook_version': '0.1.1',
          'status': 'skipped',
          'recipe_name': '',
          'result': '',
          'conditional': 'not_if { #code block }',
          'ignore_failure': false
       },
       {
          'type': 'file',
          'name': '/path/does/not/exist/so/this/should/fail.txt',
          'id': '/path/does/not/exist/so/this/should/fail.txt',
          'duration': '0',
          'delta': '',
          'cookbook_name': 'insights-test',
          'cookbook_version': '0.1.1',
          'status': 'skipped',
          'recipe_name': '',
          'result': '',
          'conditional': 'not_if { #code block }',
          'ignore_failure': false
       }
    ],
    'run_list': [
       'recipe[insights-test::default]'
    ],
    'deprecations': deprecations,
    'error': errorSection,
    'tags': [],
    'resource_names': [
       '/tmp/test.txt',
       'ls',
       '/tmp/test.txt',
       'ls -l',
       '/tmp/test.txt',
       '/tmp/always-updated.txt',
       '/failed/file/resource',
       '/tmp/do-not-write.txt',
       '/path/does/not/exist/but/we/will/ignore/this/failure.txt',
       '/path/does/not/exist/so/this/should/fail.txt'
    ],
    'recipes': [
       'opscode-ci::slave',
       'opscode-ci::_build_support',
       'chef-sugar::default',
       'opscode-ci::_platform_tweaks',
       'yum-epel::default',
       'opscode-ci::_migration',
       'omnibus::default',
       'omnibus::_common',
       'omnibus::_user',
       'omnibus::_omnibus_toolchain',
       'omnibus::_cacerts',
       'omnibus::_compile',
       'build-essential::default',
       'omnibus::_git',
       'omnibus::_ruby',
       'omnibus::_github',
       'omnibus::_libffi',
       'omnibus::_packaging',
       'omnibus::_selinux',
       'omnibus::_environment',
       'opscode-ci::_github',
       'opscode-ci::_ntp',
       'opscode-ci::_package_signing',
       'opscode-ci::_rubygems',
       'opscode-ci::_users',
       'opscode-ci::_sudo',
       'sudo::default',
       'opscode-ci::_java',
       'aws::default',
       'opscode-ci::_omnibus'
    ],
    'chef_tags': [
       'slave',
       'supermarket',
       'builder'
    ],
    'cookbooks': [
       'bluepill',
       'compat_resource',
       'windows-sdk',
       'aws',
       'nginx',
       'sudo',
       'windows',
       'build-essential',
       'chef-ingredient',
       'chef-sugar',
       'chef_handler',
       'homebrew',
       'mingw',
       'opscode-ci',
       'rsyslog',
       'runit',
       'dmg',
       'git',
       'jenkins',
       'languages',
       'seven_zip',
       'apt',
       'freebsd',
       'ohai',
       'remote_install',
       'wix',
       'packagecloud',
       'xml',
       'yum',
       'omnibus',
       'route53',
       'yum-epel'
    ],
    'platform': 'redhat',
    'platform_family': 'rhel',
    'platform_version': '7.1',
    'roles': [],
    'policy_name': '',
    'policy_group': '',
    'policy_revision': '',
    'expanded_run_list': {
       'id': '_default',
       'run_list': [
          {
             'type': 'recipe',
             'name': 'insights-test::default',
             'version': '',
             'skipped': false
          }
       ]
    }
 };
}
