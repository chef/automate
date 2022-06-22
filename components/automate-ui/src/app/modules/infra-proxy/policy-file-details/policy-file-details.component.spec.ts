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
import { PolicyFileDetailsComponent } from './policy-file-details.component';
import { MockComponent } from 'ng2-mock-component';
import { PolicyFile } from 'app/entities/policy-files/policy-file.model';
import { GetPolicyFileSuccess } from 'app/entities/policy-files/policy-file.action';
import { By } from '@angular/platform-browser';

describe('PolicyFileDetailsComponent', () => {
  let component: PolicyFileDetailsComponent;
  let fixture: ComponentFixture<PolicyFileDetailsComponent>;
  let router: Router;
  let store: Store<NgrxStateAtom>;
  let element;

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
        MockComponent({
          selector: 'app-revision-id',
          inputs: ['serverId'],
          outputs: ['close']
        }),
        PolicyFileDetailsComponent
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

    fixture = TestBed.createComponent(PolicyFileDetailsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    element = fixture.debugElement;
  });

  const policyFile: PolicyFile = {
    name: 'test',
    cookbook_locks: [{
      name: 'audit',
      source: '../cookbook/audit',
      version: '1.2.3'
    }],
    included_policy_locks: [{
      name: 'test2',
      revision_id: '434979ef36f20af46a24c5c83e25d7bc4c667b4178ff5184109ed62e750fcdc5'
    }],
    policy_group: 'test_group',
    revision_id: '434979ef36f20af46a24c5c83e25d7bc4c667b4178ff5184109ed62e750fcdc5',
    default_attributes: '{"my-cookbook": {"port": 80, "code": {"location": "github"}}}',
    override_attributes: '{}'
  };

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  it('load policy file details', () => {
    store.dispatch(new GetPolicyFileSuccess(policyFile));
    expect(component.PolicyFile).not.toBeNull();
  });

  describe('Attributes Tab', () => {
    it('renders the attributes tab correctly', () => {
      expect(element.query(By.css('.jsontree_value_object'))).toBeNull();
    });
  });
});
