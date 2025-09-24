import { NO_ERRORS_SCHEMA, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { TestBed, ComponentFixture, ComponentFixtureAutoDetect } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';
import { RouterTestingModule } from '@angular/router/testing';
import { ActivatedRoute } from '@angular/router';
import { StoreModule } from '@ngrx/store';
import { BehaviorSubject } from 'rxjs';

import { runtimeChecks, ngrxReducers } from 'app/ngrx.reducers';
import { NodeRun } from 'app/types/types';
import { NodeDetailsService, LogModalObject } from 'app/services/node-details/node-details.service';
import { AttributesService } from 'app/services/attributes/attributes.service';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { NodeDetailsComponent } from './node-details.component';
import { MockComponent } from 'ng2-mock-component';
import { MockChefBreadcrumb, MockChefBreadcrumbs, MockChefPageHeader, MockChefSidebarEntry, MockChefSubheading, MockChefTable } from 'app/testing/mock-components';
let routeData$: BehaviorSubject<any>;

class MockTelemetryService {
  track() {}
}

class MockNodeDetailsService {
  showModal$ = new BehaviorSubject<LogModalObject>({ isVisible: false });

  showModal(isVisible: boolean, resourceId?: string) {
    this.showModal$.next({ isVisible, resourceId });
  }
}

class MockAttributesService {}
class MockFeatureFlagsService {}

describe('NodeDetailsComponent', () => {
  let fixture: ComponentFixture<NodeDetailsComponent>;
  let component: NodeDetailsComponent;
  let eventService: NodeDetailsService;
  let logModalObject: LogModalObject = { isVisible: true };

  afterEach(() => {
    // Destroy the component which should unsubscribe from all observables
    if (fixture) {
      fixture.destroy();
    }
    // Complete any BehaviorSubjects to prevent EmptyError
    if (routeData$ && !routeData$.closed) {
      routeData$.complete();
    }
  });

  describe('after nodeRun is set', () => {
    beforeEach(() => {
      fixture = createTestFixture();
      component = fixture.componentInstance;
      eventService = TestBed.inject(NodeDetailsService);
    });

    describe('on initialization', () => {
      it('should initialize and subscribe to showModal$', () => {
        expect(component).toBeDefined();
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
      fixture = createTestFixture(NodeRun.Null);
      component = fixture.componentInstance;
      eventService = TestBed.inject(NodeDetailsService);
    });

    describe('nodeRun', () => {
      it('should be equal to NodeRun.Null', () => {
        expect(component.nodeRun).toBe(NodeRun.Null);
      });
    });
  });

  function createTestFixture(nodeRun: NodeRun = createNodeRun()): ComponentFixture<NodeDetailsComponent> {
  const snapshot = { params: { 'node-id': 'node_id', 'run-id': 'run_id' } };

  // Create or reuse the global BehaviorSubject
  if (!routeData$ || routeData$.closed) {
    routeData$ = new BehaviorSubject<any>({ nodeRun });
  } else {
    routeData$.next({ nodeRun });
  }

  // Proper mock ActivatedRoute with snapshot and data as Observable
  const mockActivatedRoute = {
    snapshot,
    data: routeData$
  };

  TestBed.configureTestingModule({
    imports: [
      FormsModule,
      RouterTestingModule,
      StoreModule.forRoot(ngrxReducers, { runtimeChecks }),
      // Move MockComponent() calls to imports for Angular 15+
      MockChefPageHeader,
      MockChefSubheading,
      MockChefTable,
      MockComponent({ selector: 'app-logs-modal', inputs: ['isVisible', 'nodeRun'] }),
      MockComponent({ selector: 'app-resources', inputs: ['nodeRun'] }),
      MockComponent({ selector: 'app-run-history', inputs: ['nodeId', 'nodeName', 'initialRunId', 'initialDate'] }),
      MockComponent({ selector: 'app-run-list', inputs: ['nodeRun'] }),
      MockComponent({ selector: 'app-attributes', inputs: ['nodeId'] }),
      MockComponent({ selector: 'app-run-summary', inputs: ['nodeRun'] }),
      MockComponent({ selector: 'app-tab', inputs: ['active'] }),
      MockComponent({ selector: 'app-tabs' }),
      MockComponent({ selector: 'json-tree', inputs: ['json'] }),
      MockChefBreadcrumbs,
      MockChefBreadcrumb,
      MockChefSidebarEntry,
      MockComponent({ selector: 'app-server-org-filter-sidebar' })
    ],
    declarations: [NodeDetailsComponent],
    providers: [
      { provide: ComponentFixtureAutoDetect, useValue: true },
      { provide: TelemetryService, useClass: MockTelemetryService },
      { provide: ActivatedRoute, useValue: mockActivatedRoute },
      { provide: NodeDetailsService, useClass: MockNodeDetailsService },
      { provide: AttributesService, useClass: MockAttributesService },
      { provide: FeatureFlagsService, useClass: MockFeatureFlagsService }
    ],
    schemas: [NO_ERRORS_SCHEMA, CUSTOM_ELEMENTS_SCHEMA]
  });

  const fixture = TestBed.createComponent(NodeDetailsComponent);
  fixture.detectChanges();
  return fixture;
}

});

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
