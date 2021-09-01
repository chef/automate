import { ComponentFixture, TestBed } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';
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
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { Router } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { PolicyGroupDetailsComponent } from './policy-group-details.component';
import { PolicyGroup } from 'app/entities/policy-files/policy-file.model';
import { GetPolicyGroupSuccess } from 'app/entities/policy-files/policy-file.action';
import { GetPolicyGroupNodesSuccess } from 'app/entities/infra-nodes/infra-nodes.actions';

describe('PolicyGroupDetailsComponent', () => {
  let component: PolicyGroupDetailsComponent;
  let fixture: ComponentFixture<PolicyGroupDetailsComponent>;
  let store: Store<NgrxStateAtom>;
  let router: Router;

  const server_id = 'chef-server-dev-test';
  const org_id = 'chef-org-dev';
  const name = 'policy-group-001';

  const initialState = {
    ...defaultInitialState,
    router: {
      ...defaultRouterState,
      state: {
        ...defaultRouterRouterState,
        url: `infrastructure/chef-servers/${server_id}/org/${org_id}/policygroups/${name}`,
        params: { id: server_id, 'org-id': org_id, 'name': name }
      }
    }
  };

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-breadcrumb' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-table' }),
        MockComponent({ selector: 'chef-thead' }),
        MockComponent({ selector: 'chef-tbody' }),
        MockComponent({ selector: 'chef-tr' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'chef-tab-selector',
          inputs: ['value', 'routerLink', 'fragment']
        }),
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        PolicyGroupDetailsComponent
      ],
      providers: [
        FeatureFlagsService
      ],
      imports: [
        RouterTestingModule,
        HttpClientTestingModule,
        StoreModule.forRoot(ngrxReducers, { initialState, runtimeChecks })
      ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    store = TestBed.inject(Store);
    router = TestBed.inject(Router);
    spyOn(router, 'navigate').and.stub();

    fixture = TestBed.createComponent(PolicyGroupDetailsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  const policyGroup: PolicyGroup = {
    name: 'test_policy_group',
    policies: [{
      name: 'test_policy_file',
      revision_id: '434979ef36f20af46a24c5c83e25d7bc4c667b4178ff5184109ed62e750fcdc5'
    }],
    uri: 'https://a2-dev.test/organizations/test/policy_groups/test_policy_group'
  };

  const nodes = {
    nodes: [{
      id: '',
      server_id: '',
      org_id: '',
      name: '',
      fqdn: '',
      ip_address: '',
      check_in: '',
      uptime: '',
      platform: '',
      environment: '',
      policy_group: '',
      policy_name: '',
      default_attributes: '',
      override_attributes: '',
      normal_attributes: '',
      automatic_attributes: '',
      run_list: [],
      tags: []
    }],
    total: 2
  };

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('load policy group details', () => {
    store.dispatch(new GetPolicyGroupSuccess(policyGroup));
    expect(component.policyGroup).not.toBeNull();
  });

  it('load policy group nodes', () => {
    store.dispatch(new GetPolicyGroupNodesSuccess(nodes));
    expect(component.nodes).not.toBeNull();
  });
});
