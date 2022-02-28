import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';

import { EnvironmentsComponent } from './environments.component';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { Store, StoreModule } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { By } from '@angular/platform-browser';
import { GetEnvironmentsSuccess } from 'app/entities/environments/environment.action';
import { Environment } from 'app/entities/environments/environment.model';
import { using } from 'app/testing/spec-helpers';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

describe('EnvironmentsComponent ', () => {
  let component: EnvironmentsComponent;
  let fixture: ComponentFixture<EnvironmentsComponent>;
  let element;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'app-create-environment-modal',
        inputs: ['openEvent', 'server_Id', 'org_Id', 'currentPage', 'environmentsList'] }),
        MockComponent({ selector: 'app-delete-infra-object-modal',
        inputs: ['visible', 'objectNoun', 'objectAction', 'objectName'],
        outputs: ['close', 'deleteClicked'] }),
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
        EnvironmentsComponent
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
    fixture = TestBed.createComponent(EnvironmentsComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('#search', () => {
    describe('search shows no data', () => {
      using([
        ['contains tilde.', 'environment~'],
        ['contains acute, back quote,', 'environment`'],
        ['contains exclamation mark', 'environment!'],
        ['contains ampersat, at', 'environment@'],
        ['contains dollar sign', 'environment$'],
        ['contains percent.', 'environment%'],
        ['contains caret or circumflex.,', 'environment^'],
        ['contains ampersand', 'environment&'],
        ['contains asterisk', 'environment*'],
        ['contains open or left parenthesis.', 'environment('],
        ['contains close or right parenthesis,', 'environment)'],
        ['contains plus', 'environment+'],
        ['contains equal', 'environment='],
        ['contains open brace', 'environment{'],
        ['contains close brace', 'environment}'],
        ['contains open bracket', 'environment['],
        ['contains closed bracket', 'environment]'],
        ['contains pipe', 'environment|'],
        ['contains backslash', 'environment\\'],
        ['contains forward slash', 'environment/'],
        ['contains colon', 'environment:'],
        ['contains semicolon.', 'environment;'],
        ['contains quote', 'environment"'],
        ['contains apostrophe', 'environment\'test'],
        ['contains less than', 'environment<'],
        ['contains greater than', 'environment>'],
        ['contains comma', 'environment,'],
        ['contains period, dot', 'environment.'],
        ['contains question mark', 'environment?'],
        ['contains space', 'environment test1'],
        ['has mixed alphabet, number, special character', 'environment-test!+ test1']
      ], function (description: string, input: string) {
        it(('when the name ' + description), () => {
          component.searchEnvironment(input);
          expect(component.environments.length).toBe(0);
          expect(component.total).toBe(0);
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
          component.searchEnvironment(input);
          expect(component.environments.length).toBe(0);
          expect(component.total).toBe(0);
        });
      });
    });

    describe('the form should be valid', () => {
      using([
        ['contains numbers range 0-9.', 'environment123'],
        ['contains alphabets a-z', 'environment-test'],
        ['contains underscore.', 'environment_test'],
        ['contains hyphen, minus, or dash.', 'environment_test-1'],
        ['has mixed characters', 'environment-Test_10']
      ], function (description: string, input: string) {
        it(('when the name ' + description), () => {
          component.searchEnvironment(input);
          expect(component.environments.length).not.toBeNull();
          expect(element.query(By.css('.empty-section'))).toBeNull();
        });
      });
    });
  });

  describe('environment list', () => {
    let store: Store<NgrxStateAtom>;
    const availableEnvironments: Environment[] = [{
      server_id: 'test_server_id',
      org_id: 'test_org_id',
      name: 'test4',
      description: 'test override',
      cookbook_versions: [],
      json_class: 'Chef::Environment',
      chef_type: 'environment',
      default_attributes: 'test',
      override_attributes:  'test',
      run_list: []
    }];

    const emptyEnvironments: Environment[] = [];

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('render the environment list', () => {
      store.dispatch(
        new GetEnvironmentsSuccess({
        environments: availableEnvironments, total: availableEnvironments.length
      }));
      expect(component.environments.length).not.toBeNull();
      expect(element.query(By.css('.empty-section'))).toBeNull();
    });

    it('show no preview image', () => {
      store.dispatch(
        new GetEnvironmentsSuccess({
          environments: emptyEnvironments, total: emptyEnvironments.length
        }));
      expect(component.environments.length).toBe(0);
    });
  });
});
