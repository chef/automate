import { TestBed } from '@angular/core/testing';
import {
  NodeDetailsService
} from '../../services/node-details/node-details.service';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { RunSummaryComponent } from './run-summary.component';
import { MockComponent } from 'ng2-mock-component';
import { NodeRun } from '../../types/types';

describe('RunSummaryComponent', () => {
  let fixture, component;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [
        RunSummaryComponent,
        MockComponent({ selector: 'chef-radial-chart',
                inputs: ['chartData', 'chartColors', 'labelIcon', 'labelText'] })
      ],
      providers: [
        NodeDetailsService
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(RunSummaryComponent);
    component = fixture.componentInstance;
    component.nodeRun = createNodeRun();
  });


  describe('null checks in NodeRun object', () => {
    it('When error.description is null, no errors are found', () => {
      const nodeRun = createNodeRun();
      nodeRun.error  = {
        class: 'Mixlib::ShellOut::ShellCommandFailed',
        message: 'service[chef-client] (chef-client::systemd_service line 62)' +
          'had an error: Mixlib::ShellOut::ShellCommandFailed',
        backtrace: [],
        description: null
      };

      component.nodeRun = nodeRun;

      fixture.detectChanges();

      expect(component).toBeTruthy();
    });
    it('When status is null, no errors are found', () => {
      const nodeRun = createNodeRun();
      nodeRun.status = null;
      component.nodeRun = nodeRun;

      fixture.detectChanges();

      expect(component).toBeTruthy();
    });

    it('When startTime is null, no errors are found', () => {
      const nodeRun = createNodeRun();
      nodeRun.startTime = null;
      component.nodeRun = nodeRun;

      fixture.detectChanges();

      expect(component).toBeTruthy();
    });
  });

  describe('radial', () => {

    it('leaves percentComplete unset by default', () => {
      expect(component.percentComplete).toBeUndefined();
    });

    describe('with no converge resources', () => {

      beforeEach(() => {
        component.nodeRun.resources = [];
      });

      describe('when the run status is "success"', () => {

        beforeEach(() => {
          component.nodeRun.status = 'success';
        });

        it('sets percentComplete to 100', () => {
          component.loadConverge(component.nodeRun);
          expect(component.percentComplete).toBe(100);
        });
      });

      describe('when the run status is not "success"', () => {

        beforeEach(() => {
          component.nodeRun.status = 'failure';
        });

        it('sets percentComplete to 0', () => {
          component.loadConverge(component.nodeRun);
          expect(component.percentComplete).toBe(0);
        });
      });
    });

    describe('with one or more converge resources', () => {

      describe('when all converged successfully', () => {

        beforeEach(() => {
          component.nodeRun.resources = [
            { status: 'updated' },
            { status: 'skipped' },
            { status: 'up-to-date' }
          ];
        });

        it('sets percentComplete to 100', () => {
          component.loadConverge(component.nodeRun);
          expect(component.percentComplete).toBe(100);
        });
      });

      describe('when some failed to converge', () => {

        beforeEach(() => {
          component.nodeRun.resources = [
            { status: 'failed' },
            { status: 'up-to-date' },
            { status: 'skipped' },
            { status: 'updated' },
            { status: 'unprocessed' },
            { status: 'unprocessed' }
          ];
        });

        it('sets percentComplete accordingly', () => {
          component.loadConverge(component.nodeRun);
          expect(component.percentComplete).toBe(50);
        });

        describe('but some were ignored', () => {

          beforeEach(() => {
            component.nodeRun.resources.push({
              status: 'failed',
              ignore_failure: true
            });
          });

          it('considers the failed/ignored resources successful', () => {
            component.loadConverge(component.nodeRun);
            expect(component.percentComplete).toBe(58);
          });
        });
      });
    });
  });
});

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
    source_fqdn: 'source_fqdn',
    timestamp: new Date(),
    version: '',
    error: {
      class: 'Mixlib::ShellOut::ShellCommandFailed',
      message: 'service[chef-client] (chef-client::systemd_service line 62)' +
        'had an error: Mixlib::ShellOut::ShellCommandFailed',
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
