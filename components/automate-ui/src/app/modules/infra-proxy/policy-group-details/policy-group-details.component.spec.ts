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
import { PolicyGroupDetailsComponent } from './policy-group-details.component';
import { PolicyGroup } from 'app/entities/policy-files/policy-file.model';
import { GetPolicyGroupSuccess } from 'app/entities/policy-files/policy-file.action';

describe('PolicyGroupDetailsComponent', () => {
  let component: PolicyGroupDetailsComponent;
  let fixture: ComponentFixture<PolicyGroupDetailsComponent>;
  let store: Store<NgrxStateAtom>;

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
        PolicyGroupDetailsComponent
      ],
      providers: [
        FeatureFlagsService
      ],
      imports: [
        StoreModule.forRoot(ngrxReducers, { initialState, runtimeChecks })
      ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    store = TestBed.inject(Store);

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

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('load policy group details', () => {
    store.dispatch(new GetPolicyGroupSuccess(policyGroup));
    expect(component.policyGroup).not.toBeNull();
  });
});
