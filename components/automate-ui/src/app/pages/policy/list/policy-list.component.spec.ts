import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { StoreModule, Store } from '@ngrx/store';
import { of as observableOf } from 'rxjs';
import { MockComponent } from 'ng2-mock-component';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { customMatchers } from 'app/testing/custom-matchers';
import { policyEntityReducer } from 'app/entities/policies/policy.reducer';
import { GetPoliciesSuccess } from 'app/entities/policies/policy.actions';
import { IAMMajorVersion } from 'app/entities/policies/policy.model';
import { PolicyListComponent } from './policy-list.component';

describe('PolicyListComponent', () => {
  let component: PolicyListComponent;
  let fixture: ComponentFixture<PolicyListComponent>;
  let element: HTMLElement;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'app-settings-sidebar' }),
        MockComponent({ selector: 'app-authorized', inputs: ['allOf'] }),
        MockComponent({
          selector: 'app-delete-object-modal',
          inputs: ['visible', 'objectNoun', 'objectName', 'moreDetails'],
          outputs: ['close', 'deleteClicked']
        }),
        MockComponent({ selector: 'chef-control-menu' }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-option' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-table' }),
        MockComponent({ selector: 'chef-tbody' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-thead' }),
        MockComponent({ selector: 'chef-tr' }),
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        PolicyListComponent
      ],
      imports: [
        ChefPipesModule,
        StoreModule.forRoot({
          policies: policyEntityReducer
        })
      ]
    }).compileComponents();
  }));

  beforeEach(() => {
    jasmine.addMatchers(customMatchers);
    fixture = TestBed.createComponent(PolicyListComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement.nativeElement;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  it('contains key elements', () => {
    expect(element).toContainPath('app-settings-sidebar');
    expect(element).toContainPath('chef-page-header');
  });

  it('displays policy data for v2', () => {
    component.iamMajorVersion$ = observableOf(<IAMMajorVersion>'v2');
    fixture.detectChanges();
    expect(element).toContainPath('app-authorized');
  });

  it('does not display policy data for v1', () => {
    component.iamMajorVersion$ = observableOf(<IAMMajorVersion>'v1');
    fixture.detectChanges();
    expect(element).not.toContainPath('app-authorized');
  });

  describe('sortedPolicies$', () => {
    let store: Store<NgrxStateAtom>;
    beforeEach(() => {
      store = TestBed.get(Store);
   });

    it('intermixes capitals and lowercase with lowercase first', () => {
      store.dispatch(new GetPoliciesSuccess({
        policies: [
          {
            id: 'uuid-1', name: 'Viewer',
            members: [], statements: [], type: 'CHEF_MANAGED'
          },
          {
            id: 'uuid-2', name: 'developer',
            members: [], statements: [], type: 'CUSTOM'
          },
          {
            id: 'uuid-5', name: 'Developer',
            members: [], statements: [], type: 'CUSTOM'
          },
          {
            id: 'uuid-6', name: 'viewer',
            members: [], statements: [], type: 'CHEF_MANAGED'
          }
        ]
      }));
      component.sortedPolicies$.subscribe(policies => {
        expect(policies.length).toBe(4);
        expect(policies[0]).toEqual(jasmine.objectContaining({ name: 'developer' }));
        expect(policies[1]).toEqual(jasmine.objectContaining({ name: 'Developer' }));
        expect(policies[2]).toEqual(jasmine.objectContaining({ name: 'viewer' }));
        expect(policies[3]).toEqual(jasmine.objectContaining({ name: 'Viewer' }));
      });
    });

    it('sorts by whole string before case', () => {
      store.dispatch(new GetPoliciesSuccess({
        policies: [
          {
            id: 'uuid-2', name: 'developer',
            members: [], statements: [], type: 'CUSTOM'
          },
          {
            id: 'uuid-4', name: 'developer-Manager',
            members: [], statements: [], type: 'CUSTOM'
          },
          {
            id: 'uuid-5', name: 'Developer',
            members: [], statements: [], type: 'CUSTOM'
          }
        ]
      }));
      component.sortedPolicies$.subscribe(policies => {
        expect(policies.length).toBe(3);
        expect(policies[0]).toEqual(jasmine.objectContaining({ name: 'developer' }));
        expect(policies[1]).toEqual(jasmine.objectContaining({ name: 'Developer' }));
        expect(policies[2]).toEqual(jasmine.objectContaining({ name: 'developer-Manager' }));
      });
    });

    it('uses natural ordering', () => {
      store.dispatch(new GetPoliciesSuccess({
        policies: [
          {
            id: 'uuid-1', name: 'Viewer01',
            members: [], statements: [], type: 'CHEF_MANAGED'
          },
          {
            id: 'uuid-2', name: 'Viewer300',
            members: [], statements: [], type: 'CUSTOM'
          },
          {
            id: 'uuid-3', name: 'Viewer3',
            members: [], statements: [], type: 'CUSTOM'
          },
          {
            id: 'uuid-4', name: 'Viewer-2', // does not fit in same grouping
            members: [], statements: [], type: 'CUSTOM'
          },
          {
            id: 'uuid-6', name: 'viewer',
            members: [], statements: [], type: 'CHEF_MANAGED'
          }
        ]
      }));
      component.sortedPolicies$.subscribe(policies => {
        expect(policies.length).toBe(5);
        expect(policies[0]).toEqual(jasmine.objectContaining({ name: 'viewer' }));
        expect(policies[1]).toEqual(jasmine.objectContaining({ name: 'Viewer-2' }));
        expect(policies[2]).toEqual(jasmine.objectContaining({ name: 'Viewer01' }));
        expect(policies[3]).toEqual(jasmine.objectContaining({ name: 'Viewer3' }));
        expect(policies[4]).toEqual(jasmine.objectContaining({ name: 'Viewer300' }));
      });
    });
  });
});
