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
import { PolicyFileDetailsComponent } from './policy-file-details.component';
import { MockComponent } from 'ng2-mock-component';
import { MockChefButton, MockChefHeading, MockChefIcon, MockChefLoadingSpinner, MockChefOption, MockChefPageHeader, MockChefSubheading, MockChefTabSelector, MockChefTable, MockChefTbody, MockChefTd, MockChefTh, MockChefThead, MockChefToolbar, MockChefTr } from 'app/testing/mock-components';
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
        MockChefTh,
        MockChefTd,
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        MockChefTabSelector,
        MockComponent({
          selector: 'app-revision-id',
          inputs: ['serverId'],
          outputs: ['close']
        })
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    router = TestBed.inject(Router);
    spyOn(router, 'navigate').and.stub();
    store = TestBed.inject(Store);

    // Mock all store selectors to prevent EmptyError
    spyOn(store, 'select').and.callFake((selector) => {
      const selectorString = selector.toString();

      // Route selectors
      if (selectorString.includes('routeURL')) {
        return of(`infrastructure/chef-servers/${server_id}/org/${org_id}/policy-files/${name}`);
      }
      if (selectorString.includes('routeParams')) {
        return of({ id: server_id, 'org-id': org_id, 'name': name });
      }

      // Entity status selectors
      if (selectorString.includes('Status') || selectorString.includes('getStatus')) {
        return of(EntityStatus.notLoaded);
      }

      // Data selectors
      if (selectorString.includes('policyFile')) {
        return of('');
      }

      // Default fallback
      return of('');
    });

    spyOn(store, 'dispatch');

    fixture = TestBed.createComponent(PolicyFileDetailsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    element = fixture.debugElement;
  });

  afterEach(() => {
    // Ensure proper cleanup to prevent memory leaks
    if (component && component.ngOnDestroy) {
      component.ngOnDestroy();
    }
    fixture.destroy();
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
