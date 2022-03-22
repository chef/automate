import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ComponentFixture, waitForAsync, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { Store, StoreModule } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { By } from '@angular/platform-browser';
import { using } from 'app/testing/spec-helpers';
import { PolicyGroupsComponent } from './policy-groups.component';
import { PolicyFile } from 'app/entities/policy-files/policy-file.model';
import { GetPolicyGroupsSuccess } from 'app/entities/policy-files/policy-file.action';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

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
        { provide: TelemetryService, useClass: MockTelemetryService },
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

  describe('#search', () => {
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
