import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { TestBed } from '@angular/core/testing';
import { LogsModalComponent } from './logs-modal.component';
import { NodeDetailsService } from '../../services/node-details/node-details.service';
import { NodeRun } from '../../types/types';

describe('LogsModalComponent', () => {
  let fixture, component, eventService;
  const nodeRunData = {
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
   }};

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [
        LogsModalComponent
      ],
      providers: [
        NodeDetailsService
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(LogsModalComponent);
    component = fixture.componentInstance;
    eventService = TestBed.get(NodeDetailsService);
    component.nodeRun = new NodeRun(nodeRunData);
  });

  describe('errorSections()', () => {
    describe('when no error sections are present', () => {
      it('returns undefined', () => {
        component.nodeRun = {'error': {}};
        expect(component.errorSections()).toBe(undefined);
      });
    });

    describe('when error sections are present', () => {
      it('returns a properly-formatted object', () => {
        component.nodeRun.error.description.sections =
          [
            {'section1': 'value1'},
            {'section2': 'value2'},
            {'section3': 'value3'}
          ];

        expect(component.errorSections()[0]['heading']).toBe('section1');
        expect(component.errorSections()[0]['text']).toBe('value1');
        expect(component.errorSections()[1]['heading']).toBe('section2');
        expect(component.errorSections()[1]['text']).toBe('value2');
        expect(component.errorSections()[2]['heading']).toBe('section3');
        expect(component.errorSections()[2]['text']).toBe('value3');
      });
    });
  });

  describe('formatBacktrace()', () => {
    describe('when no error is present', () => {
      it('returns undefined', () => {
        component.nodeRun.error = {};
        expect(component.formatBacktrace()).toBe(undefined);
      });
    });

    describe('when an error is present', () => {
      it('returns a properly-formatted backtrace', () => {
        component.nodeRun.error.backtrace =  ['line1', 'line2', 'line3'];
        expect(component.formatBacktrace()).toBe('line1\nline2\nline3');
      });
    });
  });

  describe('closeModal()', () => {
    it('sets showModal on the service to false', () => {
      spyOn(eventService, 'showModal');
      component.closeModal();
      expect(eventService.showModal).toHaveBeenCalledWith(false);
    });
  });
});
