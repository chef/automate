import { ReactiveFormsModule, FormBuilder, Validators, FormGroup } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { ComponentFixture, TestBed, waitForAsync
  } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';

import { CreateEnvironmentModalComponent, CookbookConstraintGrid } from './create-environment-modal.component';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';
import { CreateEnvironmentSuccess, CreateEnvironmentFailure } from 'app/entities/environments/environment.action';
import { HttpErrorResponse } from '@angular/common/http';
import { HttpStatus } from 'app/types/types';
import { EventEmitter } from '@angular/core';
import { HttpClient, HttpHandler } from '@angular/common/http';
import { Regex } from 'app/helpers/auth/regex';
import { using } from 'app/testing/spec-helpers';

export interface CreateEnvironment {
  server_id: string;
  org_id: string;
  name: string;
  description: string;
  default_attributes: Object;
  override_attributes: Object;
  cookbook_versions: CookbookConstraintGrid[];
}

class MockTelemetryService {
  track() { }
}

describe('CreateEnvironmentModalComponent', () => {
  let component: CreateEnvironmentModalComponent;
  let fixture: ComponentFixture<CreateEnvironmentModalComponent>;

  let createForm: FormGroup;
  let errors = {};

  beforeEach( waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-toolbar' }),
        MockComponent({ selector: 'app-infra-tab-change' }),
        MockComponent({ selector: 'app-infra-tab', inputs: ['active', 'disabled'] }),

        MockComponent({ selector: 'chef-modal',
          inputs: ['visible']
        }),
        CreateEnvironmentModalComponent
      ],
      providers: [
        { provide: TelemetryService, useClass: MockTelemetryService },
        HttpClient, HttpHandler
      ],
      imports: [
        ReactiveFormsModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateEnvironmentModalComponent);
    component = fixture.componentInstance;
    component.detailsFormGroup = new FormBuilder().group({
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK),
        Validators.pattern(Regex.patterns.NO_WILDCARD_ALLOW_HYPHEN)]],
      description: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]]
    });
    component.defaultAttrFormGroup = new FormBuilder().group({
      default: ['', null]
    });
    component.overrideAttrFormGroup = new FormBuilder().group({
      override: ['', null]
    });
    component.openEvent = new EventEmitter();
    createForm = component.detailsFormGroup;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('form validity', () => {
    describe('the form should be invalid', () => {
      it('when all inputs are empty', () => {
        expect(createForm.valid).toBeFalsy();
      });

      it('when name is missing', () => {
        createForm.controls['description'].setValue('test');

        errors = createForm.controls['name'].errors || {};

        expect(createForm.valid).toBeFalsy();
        expect(errors['required']).toBeTruthy();
      });

      it('when description is missing', () => {
        createForm.controls['name'].setValue('test');

        errors = createForm.controls['description'].errors || {};

        expect(createForm.valid).toBeFalsy();
        expect(errors['required']).toBeTruthy();
      });

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

          createForm.controls['name'].setValue(input);
          errors = createForm.controls['name'].errors || {};

          expect(createForm.valid).toBeFalsy();
          expect(errors['pattern']).toBeTruthy();
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

          createForm.controls['name'].setValue(input);
          errors = createForm.controls['name'].errors || {};

          expect(createForm.valid).toBeFalsy();
          expect(errors['pattern']).toBeTruthy();
        });
      });
    });

    describe('the form should be valid', () => {
      it('when all inputs are filled and valid', () => {
        expect(createForm.valid).toBeFalsy();
        createForm.controls['name'].setValue('environment');
        createForm.controls['description'].setValue('test environment description');
        expect(createForm.valid).toBeTruthy();
      });

      using([
        ['contains numbers range 0-9.', 'environment123'],
        ['contains alphabets a-z', 'environment-test'],
        ['contains underscore.', 'environment_test'],
        ['contains hyphen, minus, or dash.', 'environment_test-1'],
        ['has mixed characters', 'environment-Test_10']
      ], function (description: string, input: string) {
        it(('when the name ' + description), () => {
          createForm.controls['description'].setValue('test environment description');

          createForm.controls['name'].setValue(input);
          errors = createForm.controls['name'].errors || {};

          expect(createForm.valid).toBeTruthy();
          expect(errors['pattern']).toBeFalsy();
        });
      });
    });
  });

  describe('check cookbook version', () => {
    const cookbookVersion = '2.3.4';

    it('cookbook version should be valid when version data is filled out', () => {
      component.constraintFormGroup.controls['version'].setValue(cookbookVersion);
      expect(component.constraintFormGroup.valid).toBeTruthy();
    });
  });

  describe('create environment', () => {
    let store: Store<NgrxStateAtom>;
    const environment: CreateEnvironment = {
      org_id: 'chef_manage',
      server_id: 'test',
      name: 'test',
      description: 'test environment',
      cookbook_versions: [{
        id: 1,
        name: 'aix',
        version: '2.3.4',
        operator: '<'
      }],
      default_attributes: {
        test: 'test'
      },
      override_attributes: {
        test: 'test'
      }
    };

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('opening create modal resets default tab to details tab', () => {
      component.visible = true;
      expect(component.detailsTab).toBe(true);
      expect(component.constraintsTab).toBe(false);
      expect(component.defaultTab).toBe(false);
      expect(component.overrideTab).toBe(false);
    });

    it('opening create modal resets name, description, default, override to empty string', () => {
      component.visible = true;
      expect(component.detailsFormGroup.controls['name'].value).toEqual('');
      expect(component.detailsFormGroup.controls['description'].value).toEqual('');
      expect(component.defaultAttrFormGroup.controls['default'].value).toEqual('');
      expect(component.overrideAttrFormGroup.controls['override'].value).toEqual('');
    });

    it('on conflict error, modal remains open and displays conflict error', () => {
      spyOn(component.conflictErrorEvent, 'emit');
      component.visible = true;
      component.detailsFormGroup.controls['name'].setValue(environment.name);
      component.detailsFormGroup.controls['description'].setValue(environment.description);
      component.constraints = environment.cookbook_versions;
      component.defaultAttrFormGroup.controls['default'].setValue(
        JSON.stringify(environment.default_attributes));
      component.overrideAttrFormGroup.controls['override'].setValue(
        JSON.stringify(environment.override_attributes));

      component.createEnvironment();

      const conflict = <HttpErrorResponse>{
        status: HttpStatus.CONFLICT,
        ok: false
      };
      store.dispatch(new CreateEnvironmentFailure(conflict));
      expect(component.conflictError).toBe(true);
    });

    it('on success, closes modal and adds new environment', () => {
      spyOn(component.conflictErrorEvent, 'emit');
      component.visible = true;
      component.detailsFormGroup.controls['name'].setValue(environment.name);
      component.detailsFormGroup.controls['description'].setValue(environment.description);
      component.constraints = environment.cookbook_versions;
      component.defaultAttrFormGroup.controls['default'].setValue(
        JSON.stringify(environment.default_attributes));
      component.overrideAttrFormGroup.controls['override'].setValue(
        JSON.stringify(environment.override_attributes));
      component.createEnvironment();

      store.dispatch(new CreateEnvironmentSuccess(environment));
      expect(component.creating).toBe(false);
      expect(component.visible).toBe(false);
    });

    it('on create error, modal is closed (because error is handled by failure banner)', () => {
      spyOn(component.conflictErrorEvent, 'emit');
      component.detailsFormGroup.controls['name'].setValue(environment.name);
      component.detailsFormGroup.controls['description'].setValue(environment.description);
      component.constraints = environment.cookbook_versions;
      component.defaultAttrFormGroup.controls['default'].setValue(
        JSON.stringify(environment.default_attributes));
      component.overrideAttrFormGroup.controls['override'].setValue(
        JSON.stringify(environment.override_attributes));

      component.createEnvironment();

      const error = <HttpErrorResponse>{
        status: HttpStatus.INTERNAL_SERVER_ERROR,
        ok: false
      };

      store.dispatch(new CreateEnvironmentFailure(error));

      expect(component.conflictError).toBe(false);
    });

  });

});
