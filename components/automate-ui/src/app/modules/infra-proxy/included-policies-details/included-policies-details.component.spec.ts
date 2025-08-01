import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';

import { IncludedPoliciesDetailsComponent } from './included-policies-details.component';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { MockChefError, MockChefFormField, MockChefHeading, MockChefIcon, MockChefLoadingSpinner, MockChefPageHeader, MockChefSubheading, MockChefTable, MockChefTbody, MockChefTd, MockChefTh, MockChefThead, MockChefToolbar, MockChefTr } from 'app/testing/mock-components';
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
        StoreModule.forRoot(ngrxReducers, { runtimeChecks }),
        MockChefTh,
        MockChefTd,
        MockChefError,
        MockChefFormField,
        MockChefHeading,
        MockChefIcon,
        MockChefLoadingSpinner,
        MockComponent({ selector: 'mat-select' }),
        MockComponent({ selector: 'mat-option' }),
        MockChefPageHeader,
        MockChefSubheading,
        MockChefToolbar,
        MockChefTable,
        MockChefThead,
        MockChefTbody,
        MockChefTr,
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        MockComponent({ selector: 'input', inputs: ['resetOrigin'] })
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
