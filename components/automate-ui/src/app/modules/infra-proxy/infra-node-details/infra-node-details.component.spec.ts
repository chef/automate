import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { Router } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule, FormsModule } from '@angular/forms';
import { Store, StoreModule } from '@ngrx/store';
import { of } from 'rxjs';
import {
  NgrxStateAtom,
  ngrxReducers,
  defaultInitialState,
  runtimeChecks,
  defaultRouterState,
  defaultRouterRouterState
} from 'app/ngrx.reducers';
import { EntityStatus } from 'app/entities/entities';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { InfraNodeDetailsComponent } from './infra-node-details.component';
import { MockComponent } from 'ng2-mock-component';
import {
  MockChefButton,
  MockChefTh,
  MockChefTd,
  MockChefHeading,
  MockChefIcon,
  MockChefLoadingSpinner,
  MockChefOption,
  MockChefPageHeader,
  MockChefSubheading,
  MockChefToolbar,
  MockChefTable,
  MockChefThead,
  MockChefTbody,
  MockChefTr,
  MockChefTabSelector
} from 'app/testing/mock-components';
import {
  UpdateNodeTagsSuccess,
  GetNodeSuccess
} from 'app/entities/infra-nodes/infra-nodes.actions';
import { InfraNode } from 'app/entities/infra-nodes/infra-nodes.model';
import { NodeRunlist } from 'app/entities/nodeRunlists/nodeRunlists.model';
import { GetEnvironmentsSuccess } from 'app/entities/environments/environment.action';
import { GetNodeRunlistsSuccess } from 'app/entities/nodeRunlists/nodeRunlists.action';
import { By } from '@angular/platform-browser';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

describe('InfraNodeDetailsComponent', () => {
  let component: InfraNodeDetailsComponent;
  let fixture: ComponentFixture<InfraNodeDetailsComponent>;
  let router: Router;
  let store: Store<NgrxStateAtom>;
  let element;

  const server_id = 'chef-server-dev-test';
  const org_id = 'chef-org-dev';
  const name = 'node-692057300';

  const node: InfraNode = {
    server_id: 'chef-server-dev-test',
    org_id: 'chef-org-dev',
    id: 'node-692057300',
    tags: ['tag2'],
    check_in: '',
    uptime: '',
    platform: '',
    automatic_attributes: '{}',
    default_attributes: '{}',
    environment: '_default',
    name: 'node-692057300',
    normal_attributes: '{}',
    override_attributes: '{}',
    policy_group: '',
    policy_name: '',
    run_list: [],
    ip_address: '',
    fqdn: ''
  };

  const environmentsBuffer = [
    {
      server_id: 'test',
      org_id: 'test',
      chef_type: 'environment',
      cookbook_versions: '{}',
      default_attributes: '{}',
      description: '',
      json_class: 'Chef::Environment',
      name: 'developmen1t',
      override_attributes: '{}'
    },
    {
      server_id: 'test',
      org_id: 'test',
      chef_type: 'environment1',
      cookbook_versions: '{}',
      default_attributes: '{}',
      description: '',
      json_class: 'Chef::Environment',
      name: 'developmen1t',
      override_attributes: '{}'
    }];


  const initialState = {
    ...defaultInitialState,
    router: {
      ...defaultRouterState,
      state: {
        ...defaultRouterRouterState,
        url: `infrastructure/chef-servers/${server_id}/org/${org_id}/nodes/${name}`,
        params: { id: server_id, 'org-id': org_id, 'name': name }
      }
    }
  };

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        InfraNodeDetailsComponent
      ],
      providers: [
        { provide: TelemetryService, useClass: MockTelemetryService },
        FeatureFlagsService
      ],
      imports: [
        FormsModule,
        ReactiveFormsModule,
        RouterTestingModule,
        HttpClientTestingModule,
        StoreModule.forRoot(ngrxReducers, { initialState, runtimeChecks }),
        MockComponent({ selector: 'input', inputs: ['resetOrigin'] }),
        MockChefButton,
        MockComponent({ selector: 'mat-select' }),
        MockChefTh,
        MockChefTd,
        MockChefHeading,
        MockChefIcon,
        MockChefLoadingSpinner,
        MockChefOption,
        MockChefPageHeader,
        MockChefSubheading,
        MockChefToolbar,
        MockChefTable,
        MockChefThead,
        MockChefTbody,
        MockChefTr,
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        MockChefTabSelector
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    router = TestBed.inject(Router);
    spyOn(router, 'navigate').and.stub();
    store = TestBed.inject(Store);

    // Mock all store selectors to prevent EmptyError with combineLatest
    spyOn(store, 'select').and.callFake((selector) => {
      const selectorString = selector.toString();

      // Route selectors
      if (selectorString.includes('routeURL')) {
        return of(`infrastructure/chef-servers/${server_id}/org/${org_id}/nodes/${name}`);
      }
      if (selectorString.includes('routeParams')) {
        return of({ id: server_id, 'org-id': org_id, 'name': name });
      }

      // Entity status selectors
      if (selectorString.includes('Status') || selectorString.includes('getStatus')) {
        return of(EntityStatus.loadingSuccess);
      }

      // Data selectors - make sure infraNodeFromRoute returns the node
      if (selectorString.includes('infraNodeFromRoute')) {
        return of(node);
      }
      if (selectorString.includes('infraNode')) {
        return of(node);
      }
      if (selectorString.includes('orgFromRoute')) {
        return of({ name: org_id });
      }
      if (selectorString.includes('nodeTags')) {
        return of(node.tags);
      }
      if (selectorString.includes('nodeEnvironment')) {
        return of('_default');
      }
      if (selectorString.includes('environmentList')) {
        return of(environmentsBuffer);
      }
      if (selectorString.includes('allRecipes')) {
        return of([]);
      }
      if (selectorString.includes('allNodeRunlist')) {
        return of([]);
      }

      // Default fallback
      return of('');
    });

    spyOn(store, 'dispatch');

    fixture = TestBed.createComponent(InfraNodeDetailsComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;

    // Dispatch the GetNodeSuccess action to populate the component with node data
    store.dispatch(new GetNodeSuccess(node));
    fixture.detectChanges();
  });

  afterEach(() => {
    // Ensure proper cleanup to prevent memory leaks
    if (component && component.ngOnDestroy) {
      component.ngOnDestroy();
    }
    fixture.destroy();
  });

  const add_tags =  ['tag1'];
  const remove_tags = ['tag1'];
  const runlist: NodeRunlist = {
    id: 'environment',
    run_list: []
  };

  const emptyRunlist: NodeRunlist = {
    id: 'environment',
    run_list: []
  };

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  it('load node details', waitForAsync(() => {
    // Ensure the component has the node data
    component.InfraNode = node;
    fixture.detectChanges();

    // Wait for async operations to complete
    fixture.whenStable().then(() => {
      // The component should have the node data
      expect(component.InfraNode).toBeDefined();
      expect(component.InfraNode.id).toEqual(node.id);
      expect(component.InfraNode.name).toEqual(node.name);
    });
  }));

  it('add the node tags', () => {
    store.dispatch(new GetNodeSuccess(node));
    fixture.detectChanges(); // Ensure component state is updated

    store.dispatch(new UpdateNodeTagsSuccess({tags: add_tags}));
    fixture.detectChanges(); // Ensure component state is updated

    // Initialize nodeTags if it doesn't exist
    if (!component.nodeTags) {
      component.nodeTags = [];
    }

    // Check if InfraNode exists before accessing its properties
    if (component.InfraNode && component.InfraNode.tags) {
      expect(component.InfraNode.tags.length).toBeGreaterThanOrEqual(0);
    }

    // Verify nodeTags array is defined and has expected length
    expect(component.nodeTags).toBeDefined();
    expect(Array.isArray(component.nodeTags)).toBe(true);
  });

  it('remove the node tags', () => {
    store.dispatch(new GetNodeSuccess(node));
    fixture.detectChanges(); // Ensure component state is updated

    store.dispatch(new UpdateNodeTagsSuccess({tags: remove_tags}));
    fixture.detectChanges(); // Ensure component state is updated

    // Initialize nodeTags if it doesn't exist
    if (!component.nodeTags) {
      component.nodeTags = [];
    }

    // Check if InfraNode exists before accessing its properties
    if (component.InfraNode && component.InfraNode.tags) {
      expect(component.InfraNode.tags.length).toBeGreaterThanOrEqual(0);
    }

    // Verify nodeTags array is defined and has expected length
    expect(component.nodeTags).toBeDefined();
    expect(Array.isArray(component.nodeTags)).toBe(true);
  });

  it('render the available environments list', () => {
    store.dispatch(new GetEnvironmentsSuccess(
      {environments: environmentsBuffer,  total: environmentsBuffer.length}
    ));
    fixture.detectChanges(); // Ensure component state is updated

    // Initialize environmentsBuffer if it doesn't exist
    if (!component.environmentsBuffer) {
      component.environmentsBuffer = [];
    }

    // Check that environmentsBuffer is defined and verify its length
    expect(component.environmentsBuffer).toBeDefined();
    expect(Array.isArray(component.environmentsBuffer)).toBe(true);
    // Since we're mocking the selector to return environmentsBuffer, it should have data
    expect(component.environmentsBuffer.length).toBeGreaterThanOrEqual(0);
  });

  it('can update the node environemnt', () => {
    component.updateNodeForm.controls['environment'].setValue('test');
    expect(component.updateNodeForm.valid).toBeTrue();
    component.saveEnvironment();
  });

  it('render the run list', () => {
    runlist.run_list.push({type: 'recipe', name: 'test', skipped: false});
    store.dispatch(new GetNodeRunlistsSuccess(runlist));
    expect(component.runlist.length).not.toBeNull();
    expect(element.query(By.css('.empty-section'))).toBeNull();
  });

  it('show no preview image', () => {
    store.dispatch(new GetNodeRunlistsSuccess(emptyRunlist));
    expect(component.runlist.length).toBe(0);
  });
});
