import { NO_ERRORS_SCHEMA, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { TestBed, ComponentFixtureAutoDetect, ComponentFixture } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';
import { RouterTestingModule } from '@angular/router/testing';
import { ActivatedRoute } from '@angular/router';
import { StoreModule } from '@ngrx/store';
import { BehaviorSubject, Subject } from 'rxjs';
import { MockComponent } from 'ng2-mock-component';

import { runtimeChecks, ngrxReducers } from 'app/ngrx.reducers';
import { NodeRun } from 'app/types/types';
import { NodeDetailsService, LogModalObject } from 'app/services/node-details/node-details.service';
import { AttributesService } from 'app/services/attributes/attributes.service';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { NodeDetailsComponent  } from './node-details.component';

class MockTelemetryService {
  track() { }
}

describe('NodeDetailsComponent', () => {
  let fixture: ComponentFixture<NodeDetailsComponent>;
  let component: NodeDetailsComponent;
  let eventService: NodeDetailsService;
  let nodeRunSource: Subject<{ nodeRun: NodeRun }>;
  let logModalObject: LogModalObject;
  logModalObject = { 'isVisible': true };

  describe('after nodeRun is set', () => {
    beforeEach(() => {
      nodeRunSource = new Subject<{ nodeRun: NodeRun }>();

      fixture = createTestFixture(nodeRunSource);
      component = fixture.componentInstance;
      eventService = TestBed.inject(NodeDetailsService);
      nodeRunSource.next({ nodeRun: createNodeRun() });
    });

    describe('on initialization', () => {
      it('subscribes to the node-details event service', () => {
        spyOn(eventService.showModal$, 'subscribe')
          .and.returnValue(new BehaviorSubject<boolean>(false));
        component.ngOnInit();
        // Next line complains "subscribe is deprecated" -- but this usage is OK!
        // tslint:disable-next-line
        expect(eventService.showModal$.subscribe).toHaveBeenCalled();
      });
    });

    describe('#toggleModal()', () => {
      it('sets the logs-modal visibility', () => {
        expect(component.modalIsVisible).toBe(false);
        component.toggleModal(logModalObject);
        expect(component.modalIsVisible).toBe(true);
      });
    });
  });

  describe('before nodeRun is set', () => {
    beforeEach(() => {
      nodeRunSource = new Subject<{ nodeRun: NodeRun }>();

      fixture = createTestFixture(nodeRunSource);
      component = fixture.componentInstance;
      eventService = TestBed.inject(NodeDetailsService);
    });

    describe('nodeRun', () => {
      it('should be equal to NodeRun.Null', () => {
        expect(component.nodeRun).toBe(NodeRun.Null);
      });
    });
  });

});

function createTestFixture(
  nodeRunSource: Subject<{ nodeRun: NodeRun }>): ComponentFixture<NodeDetailsComponent> {
  const snapshot = { params: { node_id: 'node_id', 'run-id': 'run_id' } };

  TestBed.configureTestingModule({
    imports: [
      FormsModule,
      RouterTestingModule,
      StoreModule.forRoot({
        ...ngrxReducers,
        router: () => ({
          state: {
            url: '/',
            queryParams: {},
            params: {},
            fragment: '',
            path: ['/']
          },
          previousRoute: {},
          navigationId: 0
        })
      }, { runtimeChecks })
    ],
    declarations: [
      MockComponent({ selector: 'app-logs-modal', inputs: ['isVisible', 'nodeRun'] }),
      MockComponent({ selector: 'app-resources', inputs: ['nodeRun'] }),
      MockComponent({
        selector: 'app-run-history',
        inputs: ['nodeId', 'nodeName', 'initialRunId', 'initialDate']
      }),
      MockComponent({ selector: 'app-run-list', inputs: ['nodeRun'] }),
      MockComponent({ selector: 'app-attributes', inputs: ['nodeId'] }),
      MockComponent({ selector: 'app-run-summary', inputs: ['nodeRun' ]}),
      MockComponent({ selector: 'app-tab', inputs: ['active'] }),
      MockComponent({ selector: 'app-tabs' }),
      MockComponent({ selector: 'json-tree', inputs: ['json'] }),
      MockComponent({ selector: 'chef-breadcrumbs'}),
      MockComponent({ selector: 'chef-breadcrumb', inputs: ['link']}),
      MockComponent({ selector: 'chef-sidebar-entry '}),
      MockComponent({ selector: 'app-server-org-filter-sidebar'}),
      NodeDetailsComponent
    ],
    providers: [
      { provide: ComponentFixtureAutoDetect, useValue: true },
      { provide: TelemetryService, useClass: MockTelemetryService },
      { provide: ActivatedRoute, useValue: {snapshot: snapshot, data: nodeRunSource} },
      NodeDetailsService,
      AttributesService,
      FeatureFlagsService
    ],
    schemas: [
      NO_ERRORS_SCHEMA,
      CUSTOM_ELEMENTS_SCHEMA
    ]
  });

  return TestBed.createComponent(NodeDetailsComponent);
}

function createNodeRun(): NodeRun {
  return new NodeRun({
    node_id: 'node_id',
    node_name: 'R2D2',
    organization: 'Chef',
    resources: [],
    chef_tags: ['chef_tags'],
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
    roles: [''],
    policy_group: 'policy_group',
    policy_name: 'policy_name',
    policy_revision: 'policyRevision',
    projects: [],
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
   },
   versioned_cookbooks: [
     {
       name: '',
       version: ''
     }
   ]
  });
}
