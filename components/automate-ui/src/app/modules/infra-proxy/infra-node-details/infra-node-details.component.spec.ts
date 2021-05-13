import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { Router } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule, FormsModule } from '@angular/forms';
import { Store, StoreModule } from '@ngrx/store';
import {
  NgrxStateAtom,
  ngrxReducers,
  defaultInitialState,
  runtimeChecks,
  defaultRouterState,
  defaultRouterRouterState
} from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { InfraNodeDetailsComponent } from './infra-node-details.component';
import { MockComponent } from 'ng2-mock-component';
import {
  UpdateNodeTagsSuccess,
  GetNodeSuccess
} from 'app/entities/infra-nodes/infra-nodes.actions';
import { InfraNode } from 'app/entities/infra-nodes/infra-nodes.model';
import { GetEnvironmentsSuccess } from 'app/entities/environments/environment.action';

describe('InfraNodeDetailsComponent', () => {
  let component: InfraNodeDetailsComponent;
  let fixture: ComponentFixture<InfraNodeDetailsComponent>;
  let router: Router;
  let store: Store<NgrxStateAtom>;

  const server_id = 'chef-server-dev-test';
  const org_id = 'chef-org-dev';
  const name = 'node-692057300';


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
        MockComponent({ selector: 'input', inputs: ['resetOrigin'] }),
        MockComponent({ selector: 'chef-button',
          inputs: ['disabled', 'routerLink'] }),
        MockComponent({ selector: 'mat-select' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-icon' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-option' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-toolbar' }),
        MockComponent({ selector: 'chef-table' }),
        MockComponent({ selector: 'chef-thead' }),
        MockComponent({ selector: 'chef-tbody' }),
        MockComponent({ selector: 'chef-tr' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        MockComponent({ selector: 'chef-tab-selector',
          inputs: ['value', 'routerLink', 'fragment']
        }),
        InfraNodeDetailsComponent
      ],
      providers: [
        FeatureFlagsService
      ],
      imports: [
        FormsModule,
        ReactiveFormsModule,
        RouterTestingModule,
        HttpClientTestingModule,
        StoreModule.forRoot(ngrxReducers, { initialState, runtimeChecks })
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    router = TestBed.inject(Router);
    spyOn(router, 'navigate').and.stub();
    store = TestBed.inject(Store);

    fixture = TestBed.createComponent(InfraNodeDetailsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

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

  const tags =  ['tag1'];

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  it('load node details', () => {
    store.dispatch(new GetNodeSuccess(node));
    expect(component.InfraNode).not.toBeNull();
  });

  it('add the node tags', () => {
    store.dispatch(new GetNodeSuccess(node));
    const tagsLength = component.InfraNode.tags.length;
    store.dispatch(new UpdateNodeTagsSuccess({tags: tags}));
    expect(component.nodeTags.length).toEqual(tagsLength);
  });

  it('remove the node tags', () => {
    store.dispatch(new GetNodeSuccess(node));
    const tagsLength = component.InfraNode.tags.length;
    store.dispatch(new UpdateNodeTagsSuccess({tags: tags}));
    expect(component.nodeTags.length).toEqual(tagsLength);
  });

  it('render the available environments list', () => {
    store.dispatch(new GetEnvironmentsSuccess(
      {environments: environmentsBuffer,  total: environmentsBuffer.length}
    ));
    expect(component.environmentsBuffer.length).toBe(1);
  });

  it('can update the node environemnt', () => {
    component.updateNodeForm.controls['environment'].setValue('test');
    expect(component.updateNodeForm.valid).toBeTrue();
    component.saveEnvironment();
  });

});
