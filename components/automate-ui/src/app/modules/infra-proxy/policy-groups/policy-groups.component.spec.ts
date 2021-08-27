import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ComponentFixture, waitForAsync, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { Store, StoreModule } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { By } from '@angular/platform-browser';
import { PolicyGroupsComponent } from './policy-groups.component';
import { PolicyFile } from 'app/entities/policy-files/policy-file.model';
import { GetPolicyGroupsSuccess } from 'app/entities/policy-files/policy-file.action';

describe('PolicyGroupsComponent', () => {
  let component: PolicyGroupsComponent;
  let fixture: ComponentFixture<PolicyGroupsComponent>;
  let element;

  beforeEach(waitForAsync (() => {
     TestBed.configureTestingModule({
      declarations: [
        MockComponent({
          selector: 'app-policy-groups-list',
          inputs: ['policyFiles']
        }),
        PolicyGroupsComponent
      ],
      providers: [
        FeatureFlagsService
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
    fixture = TestBed.createComponent(PolicyGroupsComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('policy groups list', () => {
    let store: Store<NgrxStateAtom>;
    const availablePolicyGroups: PolicyFile[] = [{
        name: 'aix',
        revision_id: '2.3.12',
        policy_group: 'test'
      }
    ];
    const emptyPolicyGroup: PolicyFile[] = [];

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('render the policy group list', () => {
      store.dispatch(new GetPolicyGroupsSuccess({policies: availablePolicyGroups}));
      expect(component.policyFiles.length).not.toBeNull();
      expect(element.query(By.css('.empty-section'))).toBeNull();
    });

    it('show no preview image', () => {
      store.dispatch(new GetPolicyGroupsSuccess({policies: emptyPolicyGroup}));
      expect(component.policyFiles.length).toBe(0);
    });
  });
});
