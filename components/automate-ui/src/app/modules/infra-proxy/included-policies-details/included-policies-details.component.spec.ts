import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';

import { IncludedPoliciesDetailsComponent } from './included-policies-details.component';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { Store, StoreModule } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { PolicyFile } from 'app/entities/policy-files/policy-file.model';
import { GetPolicyFileSuccess } from 'app/entities/policy-files/policy-file.action';
import { PolicyFileRequests } from 'app/entities/policy-files/policy-file.requests';
import { HttpClient, HttpHandler } from '@angular/common/http';

describe('IncludedPoliciesDetailsComponent', () => {
  let component: IncludedPoliciesDetailsComponent;
  let fixture: ComponentFixture<IncludedPoliciesDetailsComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-icon' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'mat-select' }),
        MockComponent({ selector: 'mat-option' }),
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
        MockComponent({ selector: 'input', inputs: ['resetOrigin'] }),
        IncludedPoliciesDetailsComponent
      ],
      providers: [
        FeatureFlagsService,
        PolicyFileRequests,
        HttpClient,
        HttpHandler
      ],
      imports: [
        FormsModule,
        ReactiveFormsModule,
        RouterTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(IncludedPoliciesDetailsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('included policy file details', () => {
    let store: Store<NgrxStateAtom>;
    const includedpolicyFile: PolicyFile = {
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

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('load policy file details', () => {
      store.dispatch(new GetPolicyFileSuccess(includedpolicyFile));
      expect(component.Policyfile).not.toBeNull();
    });
  });
});
