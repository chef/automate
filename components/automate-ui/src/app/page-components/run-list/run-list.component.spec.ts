import { TestBed } from '@angular/core/testing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { By } from '@angular/platform-browser';
import { ChefStatusIconPipe } from '../../pipes/chef-status-icon.pipe';
import { RunListComponent } from './run-list.component';
import {
  RunListRoleHeaderComponent
} from '../run-list-role-header/run-list-role-header.component';
import { RunListTableComponent } from '../run-list-table/run-list-table.component';
import { PolicyCookbooks, RespPolicyCookbooks, NodeRun } from 'app/types/types';
import { NodeRunsService } from 'app/services/node-details/node-runs.service';
import {
  SimpleChanges,
  SimpleChange
} from '@angular/core';

class MockNodeRunsService {
  getPolicyCookbooks(_revisionId: string): Promise<PolicyCookbooks> {
    return Promise.resolve(new PolicyCookbooks(getRawPolicyCookbooks()));
  }
}

describe('RunListComponent', () => {
  let fixture, element;
  let component: RunListComponent;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [
        ChefStatusIconPipe,
        RunListComponent,
        RunListRoleHeaderComponent,
        RunListTableComponent
      ],
      providers: [
        { provide: NodeRunsService, useClass: MockNodeRunsService }
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(RunListComponent);
    element = fixture.debugElement;
    component = fixture.componentInstance;
  });

  it('renders the component correctly', () => {
    expect(element.query(By.css('.run-list-rollup'))).not.toBeNull();
  });

  describe('create local runlist', () => {
    it('empty expanded_run_list', () => {
      const nodeRun = createNodeRun();
      nodeRun.expandedRunList = undefined;
      component.ngOnChanges(create_changes(createNodeRun(), nodeRun));

      expect(component.runList).toEqual([]);
      expect(component.cookbooksCount).toEqual(0);
      expect(component.rolesCount).toEqual(0);
      expect(component.recipesCount).toEqual(0);
      expect(component.failedCount).toEqual(0);
      expect(component.successCount).toEqual(0);
    });

    it('one cookbook and empty resources', () => {
      const nodeRun = createNodeRun();

      nodeRun.resources = [];
      nodeRun.expandedRunList = {
        id: '_default',
        run_list: [
          {
            type: 'recipe',
            name: 'test::default',
            version: null,
            skipped: false,
            children: []
          }
        ]
      };

      component.ngOnChanges(create_changes(createNodeRun(), nodeRun));

      expect(component.runList.length).toEqual(1);
      const item = component.runList[0];
      expect(item.name).toEqual('test::default');
      expect(item.type).toEqual('recipe');
      expect(item.children).toEqual([]);
      expect(item.skipped).toEqual(false);
      expect(item.status).toEqual('success');
      expect(item.version).toBeFalsy();

      expect(component.cookbooksCount).toEqual(1);
      expect(component.rolesCount).toEqual(0);
      expect(component.recipesCount).toEqual(1);
      expect(component.failedCount).toEqual(0);
      expect(component.successCount).toEqual(1);
    });

    it('one cookbook with resource', () => {
      const nodeRun = createNodeRun();

      nodeRun.resources = [
        {
          type: 'file',
          name: '/home/vagrant/text.txt',
          id: '/home/vagrant/text.txt',
          duration: '4',
          delta: '',
          ignore_failure: false,
          result: 'create',
          status: 'up-to-date',
          cookbook_name: 'test',
          cookbook_version: '1.0.2'
        }
      ];
      nodeRun.expandedRunList = {
        id: '_default',
        run_list: [
          {
            type: 'recipe',
            name: 'test::default',
            version: null,
            skipped: false,
            children: []
          }
        ]
      };

      component.ngOnChanges(create_changes(createNodeRun(), nodeRun));

      expect(component.runList.length).toEqual(1);
      const item = component.runList[0];
      expect(item.version).toEqual('1.0.2');

      expect(component.cookbooksCount).toEqual(1);
      expect(component.rolesCount).toEqual(0);
      expect(component.recipesCount).toEqual(1);
      expect(component.failedCount).toEqual(0);
      expect(component.successCount).toEqual(1);
    });

    it('one cookbook with updated resource', () => {
      const nodeRun = createNodeRun();

      nodeRun.resources = [
        {
          type: 'file',
          name: '/home/vagrant/text.txt',
          id: '/home/vagrant/text.txt',
          duration: '4',
          delta: '',
          ignore_failure: false,
          result: 'create',
          status: 'updated',
          cookbook_name: 'test',
          cookbook_version: '1.0.2'
        }
      ];
      nodeRun.expandedRunList = {
        id: '_default',
        run_list: [
          {
            type: 'recipe',
            name: 'test::default',
            version: null,
            skipped: false,
            children: []
          }
        ]
      };

      component.ngOnChanges(create_changes(createNodeRun(), nodeRun));

      expect(component.runList.length).toEqual(1);
      const item = component.runList[0];
      expect(item.status).toEqual('success');

      expect(component.cookbooksCount).toEqual(1);
      expect(component.rolesCount).toEqual(0);
      expect(component.recipesCount).toEqual(1);
      expect(component.failedCount).toEqual(0);
      expect(component.successCount).toEqual(1);
    });

    it('one cookbook with up-to-date resource', () => {
      const nodeRun = createNodeRun();

      nodeRun.resources = [
        {
          type: 'file',
          name: '/home/vagrant/text.txt',
          id: '/home/vagrant/text.txt',
          duration: '4',
          delta: '',
          ignore_failure: false,
          result: 'create',
          status: 'up-to-date',
          cookbook_name: 'test',
          cookbook_version: '1.0.2'
        }
      ];
      nodeRun.expandedRunList = {
        id: '_default',
        run_list: [
          {
            type: 'recipe',
            name: 'test::default',
            version: null,
            skipped: false,
            children: []
          }
        ]
      };

      component.ngOnChanges(create_changes(createNodeRun(), nodeRun));

      expect(component.runList.length).toEqual(1);
      const item = component.runList[0];
      expect(item.status).toEqual('success');

      expect(component.cookbooksCount).toEqual(1);
      expect(component.rolesCount).toEqual(0);
      expect(component.recipesCount).toEqual(1);
      expect(component.failedCount).toEqual(0);
      expect(component.successCount).toEqual(1);
    });

    it('one cookbook with failed resource', () => {
      const nodeRun = createNodeRun();

      nodeRun.resources = [
        {
          type: 'file',
          name: '/home/vagrant/text.txt',
          id: '/home/vagrant/text.txt',
          duration: '4',
          delta: '',
          ignore_failure: false,
          result: 'create',
          status: 'failed',
          cookbook_name: 'test',
          cookbook_version: '1.0.2'
        }
      ];
      nodeRun.expandedRunList = {
        id: '_default',
        run_list: [
          {
            type: 'recipe',
            name: 'test::default',
            version: null,
            skipped: false,
            children: []
          }
        ]
      };

      component.ngOnChanges(create_changes(createNodeRun(), nodeRun));

      expect(component.runList.length).toEqual(1);
      const item = component.runList[0];
      expect(item.status).toEqual('failed');

      expect(component.cookbooksCount).toEqual(1);
      expect(component.rolesCount).toEqual(0);
      expect(component.recipesCount).toEqual(1);
      expect(component.failedCount).toEqual(1);
      expect(component.successCount).toEqual(0);
    });

    it('one role with one cookbook', () => {
      const nodeRun = createNodeRun();

      nodeRun.resources = [];
      nodeRun.expandedRunList = {
        id: '_default',
        run_list: [
          {
            type: 'role',
            name: 'web',
            version: null,
            skipped: null,
            children: [
              {
                type: 'recipe',
                name: 'test::default',
                version: null,
                skipped: false,
                children: []
              }
            ]
          }
        ]
      };

      component.ngOnChanges(create_changes(createNodeRun(), nodeRun));

      expect(component.runList.length).toEqual(1);
      const item = component.runList[0];
      expect(item.type).toEqual('role');
      expect(item.name).toEqual('web');
      expect(item.has_child_role).toEqual(false);
      expect(item.children.length).toEqual(1);

      const child = item.children[0];
      expect(child.name).toEqual('test::default');
      expect(child.type).toEqual('recipe');
      expect(child.children).toEqual([]);
      expect(child.skipped).toEqual(false);

      expect(component.cookbooksCount).toEqual(1);
      expect(component.rolesCount).toEqual(1);
      expect(component.recipesCount).toEqual(1);
      expect(component.failedCount).toEqual(0);
      expect(component.successCount).toEqual(1);
    });

    it('two roles with one cookbook', () => {
      const nodeRun = createNodeRun();

      nodeRun.resources = [];
      nodeRun.expandedRunList = {
        id: '_default',
        run_list: [
          {
            type: 'role',
            name: 'web',
            version: null,
            skipped: null,
            children: [
              {
                type: 'recipe',
                name: 'test::default',
                version: null,
                skipped: false,
                children: []
              }
            ]
          },
          {
            type: 'role',
            name: 'none',
            version: null,
            skipped: null,
            children: [
              {
                type: 'recipe',
                name: 'chef-client::default',
                version: null,
                skipped: false,
                children: []
              }
            ]
          }
        ]
      };

      component.ngOnChanges(create_changes(createNodeRun(), nodeRun));

      expect(component.runList.length).toEqual(2);
      const item1 = component.runList[0];
      expect(item1.type).toEqual('role');
      expect(item1.name).toEqual('web');
      expect(item1.has_child_role).toEqual(false);
      expect(item1.children.length).toEqual(1);

      expect(item1.children[0].name).toEqual('test::default');
      expect(item1.children[0].type).toEqual('recipe');
      expect(item1.children[0].children).toEqual([]);
      expect(item1.children[0].skipped).toEqual(false);

      const item2 = component.runList[1];
      expect(item2.type).toEqual('role');
      expect(item2.name).toEqual('none');
      expect(item2.has_child_role).toEqual(false);
      expect(item2.children.length).toEqual(1);

      expect(item2.children[0].name).toEqual('chef-client::default');
      expect(item2.children[0].type).toEqual('recipe');
      expect(item2.children[0].children).toEqual([]);
      expect(item2.children[0].skipped).toEqual(false);

      expect(component.cookbooksCount).toEqual(2);
      expect(component.rolesCount).toEqual(2);
      expect(component.recipesCount).toEqual(2);
      expect(component.failedCount).toEqual(0);
      expect(component.successCount).toEqual(2);
    });

    it('recursive roles', () => {
      const nodeRun = createNodeRun();

      nodeRun.resources = [];
      nodeRun.expandedRunList = {
        id: '_default',
        run_list: [
          {
            type: 'role',
            name: 'web3',
            version: null,
            skipped: null,
            children: [
              {
                type: 'recipe',
                name: 'chef-client::default',
                version: null,
                skipped: false,
                children: []
              },
              {
                type: 'recipe',
                name: 'chef-client::delete_validation',
                version: null,
                skipped: false,
                children: []
              },
              {
                type: 'recipe',
                name: 'lamp2::default',
                version: null,
                skipped: false,
                children: []
              },
              {
                type: 'role',
                name: 'none',
                version: null,
                skipped: null,
                children: [
                  {
                    type: 'recipe',
                    name: 'test::default',
                    version: null,
                    skipped: false,
                    children: []
                  },
                  {
                    type: 'role',
                    name: 'web3',
                    version: null,
                    skipped: true,
                    children: []
                  }
                ]
              }
            ]
          }
        ]
      };

      component.ngOnChanges(create_changes(createNodeRun(), nodeRun));

      expect(component.runList.length).toEqual(1);
      const item = component.runList[0];
      expect(item.type).toEqual('role');
      expect(item.name).toEqual('web3');
      expect(item.has_child_role).toEqual(true);
      expect(item.children.length).toEqual(4);

      expect(item.children[0].name).toEqual('chef-client::default');
      expect(item.children[0].type).toEqual('recipe');
      expect(item.children[0].children).toEqual([]);
      expect(item.children[0].skipped).toEqual(false);

      expect(item.children[3].name).toEqual('none');
      expect(item.children[3].type).toEqual('role');
      expect(item.children[3].children.length).toEqual(2);

      expect(item.children[3].children[0].name).toEqual('test::default');
      expect(item.children[3].children[0].type).toEqual('recipe');
      expect(item.children[3].children[0].children.length).toEqual(0);
      expect(item.children[3].children[0].skipped).toEqual(false);

      expect(item.children[3].children[1].name).toEqual('web3');
      expect(item.children[3].children[1].type).toEqual('role');
      expect(item.children[3].children[1].children.length).toEqual(0);
      expect(item.children[3].children[1].skipped).toEqual(true);

      expect(component.cookbooksCount).toEqual(3);
      expect(component.rolesCount).toEqual(2);
      expect(component.recipesCount).toEqual(4);
      expect(component.failedCount).toEqual(0);
      expect(component.successCount).toEqual(4);
    });
  });
});

function create_changes(
  previousValue: NodeRun,
  currentValue: NodeRun): SimpleChanges {
  const changesObj: SimpleChanges = {
    nodeRun: new SimpleChange(previousValue, currentValue, true)
  };
  return changesObj;
}

function getRawPolicyCookbooks(): RespPolicyCookbooks {
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
function createNodeRun(): NodeRun {
  return new NodeRun({
    node_id: 'node_id',
    node_name: 'R2D2',
    organization: 'Chef',
    resources: [],
    chef_tags: 'chef_tags',
    id: 'run_id',
    run_list: [''],
    start_time: new Date('September 1, 2017 10:13:00'),
    end_time: new Date(),
    source: '',
    deprecations: [],
    status: 'failure',
    total_resource_count: '',
    updated_resource_count: '',
    tags: [''],
    resource_names: [''],
    recipes: [''],
    cookbooks: [''],
    platform: 'ubuntu',
    platform_family: '',
    platform_version: '',
    chef_version: '',
    uptime_seconds: 3,
    environment: 'acceptance-chef-products-automate-master',
    roles: '',
    policy_group: 'policy_group',
    policy_name: 'policy_name',
    policy_revision: 'policyRevision',
    fqdn: 'automate-acceptance.cd.chef.co',
    ipaddress: '172.21.104.221',
    source_fqdn: '',
    timestamp: new Date(),
    version: '',
    error: {
      class: 'Mixlib::ShellOut::ShellCommandFailed',
      message: 'service[chef-client] (chef-client::systemd_service line 62) had an error:',
      backtrace: [],
      description: {
        title: 'Error executing action `stop` on resource "service[chef-client]"',
        sections: []
      }
    },
    expanded_run_list: {
      id: 'acceptance-chef-products-automate-master',
      run_list: []
   }});
}
