import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ComponentFixture, waitForAsync, TestBed } from '@angular/core/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { RouterTestingModule } from '@angular/router/testing';
import { By } from '@angular/platform-browser';

import { Store, StoreModule } from '@ngrx/store';
import { of } from 'rxjs';
import { MockComponent } from 'ng2-mock-component';

import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { using } from 'app/testing/spec-helpers';
import { PolicyGroupsComponent } from './policy-groups.component';
import { PolicyFile } from 'app/entities/policy-files/policy-file.model';
import { GetPolicyGroupsSuccess } from 'app/entities/policy-files/policy-file.action';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';
import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { EntityStatus } from 'app/entities/entities';

class MockTelemetryService {
  track() { }
}

class MockLayoutFacadeService {
  showSidebar() { }
}

describe('PolicyGroupsComponent', () => {
  let component: PolicyGroupsComponent;
  let fixture: ComponentFixture<PolicyGroupsComponent>;
  let element;
  let store: Store<NgrxStateAtom>;

  beforeEach(waitForAsync (() => {
     TestBed.configureTestingModule({
      declarations: [
        PolicyGroupsComponent
      ],
      providers: [
        { provide: TelemetryService, useClass: MockTelemetryService },
        { provide: LayoutFacadeService, useClass: MockLayoutFacadeService },
        FeatureFlagsService
      ],
      imports: [
        FormsModule,
        ReactiveFormsModule,
        RouterTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks }),
        MockComponent({
          selector: 'app-policy-groups-list',
          inputs: ['policyFiles']
        })
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(PolicyGroupsComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
    store = TestBed.inject(Store);

    // Mock required inputs
    component.serverId = 'test-server-id';
    component.orgId = 'test-org-id';

    // Mock store selectors to prevent EmptyError - provide default return values for all selectors
    spyOn(store, 'select').and.callFake((selector) => {
      // Return appropriate default values based on selector type
      if (selector.toString().includes('getGroupsStatus')) {
        return of(EntityStatus.notLoaded);
      }
      if (selector.toString().includes('policyFile')) {
        return of([]);
      }
      // Default fallback for any other selector
      return of('');
    });
    spyOn(store, 'dispatch');

    fixture.detectChanges();
  });

  afterEach(() => {
    // Ensure proper cleanup to prevent memory leaks
    component.ngOnDestroy();
    fixture.destroy();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('policy groups list', () => {
    const availablePolicyGroups: PolicyFile[] = [{
        name: 'aix',
        revision_id: '2.3.12',
        policy_group: 'test'
      }
    ];
    const emptyPolicyGroup: PolicyFile[] = [];

    beforeEach(() => {
      // Reset store spy for each test
      (store.select as jasmine.Spy).calls.reset();
    });

    it('render the policy group list', () => {
      // Mock store selectors to return successful state and data
      (store.select as jasmine.Spy).and.callFake((selector) => {
        if (selector.toString().includes('getGroupsStatus')) {
          return of(EntityStatus.loadingSuccess);
        }
        if (selector.toString().includes('policyFile')) {
          return of(availablePolicyGroups);
        }
        return of('');
      });

      component.ngOnInit();
      fixture.detectChanges();

      expect(component.policyFiles.length).not.toBeNull();
      expect(element.query(By.css('.empty-section'))).toBeNull();
    });

    it('show no preview image', () => {
      // Mock store selectors to return successful state but empty data
      (store.select as jasmine.Spy).and.callFake((selector) => {
        if (selector.toString().includes('getGroupsStatus')) {
          return of(EntityStatus.loadingSuccess);
        }
        if (selector.toString().includes('policyFile')) {
          return of(emptyPolicyGroup);
        }
        return of('');
      });

      component.ngOnInit();
      fixture.detectChanges();

      expect(component.policyFiles.length).toBe(0);
    });
  });

  describe('#search', () => {
    beforeEach(() => {
      // Ensure component is properly initialized before search tests
      component.policyFiles = [{
        name: 'test-policy',
        revision_id: '1.0.0',
        policy_group: 'test-group'
      }];
      component.groupList = component.policyFiles;
    });

    describe('search shows no data', () => {
      using([
        ['contains tilde.', 'policy~'],
        ['contains acute, back quote,', 'policy`'],
        ['contains exclamation mark', 'policy!'],
        ['contains ampersat, at', 'policy@'],
        ['contains dollar sign', 'policy$'],
        ['contains percent.', 'policy%'],
        ['contains caret or circumflex.,', 'policy^'],
        ['contains ampersand', 'policy&'],
        ['contains asterisk', 'policy*'],
        ['contains open or left parenthesis.', 'policy('],
        ['contains close or right parenthesis,', 'policy)'],
        ['contains plus', 'policy+'],
        ['contains equal', 'policy='],
        ['contains open brace', 'policy{'],
        ['contains close brace', 'policy}'],
        ['contains open bracket', 'policy['],
        ['contains closed bracket', 'policy]'],
        ['contains pipe', 'policy|'],
        ['contains backslash', 'policy\\'],
        ['contains forward slash', 'policy/'],
        ['contains colon', 'policy:'],
        ['contains semicolon.', 'policy;'],
        ['contains quote', 'policy"'],
        ['contains apostrophe', 'policy\'test'],
        ['contains less than', 'policy<'],
        ['contains greater than', 'policy>'],
        ['contains comma', 'policy,'],
        ['contains period, dot', 'policy.'],
        ['contains question mark', 'policy?'],
        ['contains space', 'policy test1'],
        ['has mixed alphabet, number, special character', 'policy-test!+ test1']
      ], function (description: string, input: string) {
        it(('when the name ' + description), () => {
          component.searchPolicyFiles(input);
          expect(component.searchArrayLength).toBe(undefined);
        });
      });

      using([
        ['contains tilde.', '~'],
        ['contains acute, back quote,', '`'],
        ['contains exclamation mark', '!'],
        ['contains ampersat, at', '@'],
        ['contains dollar sign', '$'],
        ['contains percent.', '%'],
        ['contains caret or circumflex.,', '^'],
        ['contains ampersand', '&'],
        ['contains asterisk', '*'],
        ['contains open or left parenthesis.', '('],
        ['contains close or right parenthesis,', ')'],
        ['contains plus', '+'],
        ['contains equal', '='],
        ['contains open brace', '{'],
        ['contains close brace', '}'],
        ['contains open bracket', '['],
        ['contains closed bracket', ']'],
        ['contains pipe', '|'],
        ['contains backslash', '\\'],
        ['contains forward slash', '/'],
        ['contains colon', ':'],
        ['contains semicolon.', ';'],
        ['contains quote', '"'],
        ['contains apostrophe', '\'test'],
        ['contains less than', '<'],
        ['contains greater than', '>'],
        ['contains comma', ','],
        ['contains period, dot', '.'],
        ['contains question mark', '?'],
        ['contains space', '    test1']
      ], function (description: string, input: string) {
        it(('when the name only' + description), () => {
          component.searchPolicyFiles(input);
          expect(component.searchArrayLength).toBe(undefined);
        });
      });
    });
  });
});
