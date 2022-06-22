import { ReactiveFormsModule, FormBuilder, Validators, FormGroup } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';
import { CreateInfraRoleModalComponent } from './create-infra-role-modal.component';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';
import { CreateRoleSuccess, CreateRoleFailure, CreateRolePayload } from 'app/entities/infra-roles/infra-role.action';
import { HttpErrorResponse } from '@angular/common/http';
import { HttpStatus } from 'app/types/types';
import { EventEmitter } from '@angular/core';
import { HttpClient, HttpHandler } from '@angular/common/http';
import { Regex } from 'app/helpers/auth/regex';
import { using } from 'app/testing/spec-helpers';

class MockTelemetryService {
  track() { }
}

describe('CreateInfraRoleModalComponent', () => {
  let component: CreateInfraRoleModalComponent;
  let fixture: ComponentFixture<CreateInfraRoleModalComponent>;

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
        CreateInfraRoleModalComponent
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
    fixture = TestBed.createComponent(CreateInfraRoleModalComponent);
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
        ['contains tilde.', 'role~'],
        ['contains acute, back quote,', 'role`'],
        ['contains exclamation mark', 'role!'],
        ['contains ampersat, at', 'role@'],
        ['contains dollar sign', 'role$'],
        ['contains percent.', 'role%'],
        ['contains caret or circumflex.,', 'role^'],
        ['contains ampersand', 'role&'],
        ['contains asterisk', 'role*'],
        ['contains open or left parenthesis.', 'role('],
        ['contains close or right parenthesis,', 'role)'],
        ['contains plus', 'role+'],
        ['contains equal', 'role='],
        ['contains open brace', 'role{'],
        ['contains close brace', 'role}'],
        ['contains open bracket', 'role['],
        ['contains closed bracket', 'role]'],
        ['contains pipe', 'role|'],
        ['contains backslash', 'role\\'],
        ['contains forward slash', 'role/'],
        ['contains colon', 'role:'],
        ['contains semicolon.', 'role;'],
        ['contains quote', 'role"'],
        ['contains apostrophe', 'role\'test'],
        ['contains less than', 'role<'],
        ['contains greater than', 'role>'],
        ['contains comma', 'role,'],
        ['contains period, dot', 'role.'],
        ['contains question mark', 'role?'],
        ['contains space', 'role test1'],
        ['has mixed alphabet, number, special character', 'role-test!+ test1']
      ], function (description: string, input: string) {
        it(('when the name' + description), () => {

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
        createForm.controls['name'].setValue('role');
        createForm.controls['description'].setValue('test role description');
        expect(createForm.valid).toBeTruthy();
      });

      using([
        ['contains numbers range 0-9.', 'role123'],
        ['contains alphabets a-z', 'role-test'],
        ['contains underscore.', 'role_test'],
        ['contains hyphen, minus, or dash.', 'role_test-1'],
        ['has mixed characters', 'role-Test_10']
      ], function (description: string, input: string) {
        it(('when the name ' + description), () => {
          createForm.controls['description'].setValue('test role description');

          createForm.controls['name'].setValue(input);
          errors = createForm.controls['name'].errors || {};

          expect(createForm.valid).toBeTruthy();
          expect(errors['pattern']).toBeFalsy();
        });
      });
    });
  });

  describe('create role', () => {
    let store: Store<NgrxStateAtom>;
    const role: CreateRolePayload  = {
      org_id: 'chef_manage',
      server_id: 'test',
      name: 'chef-test-role',
      description: 'test role',
      run_list: ['role[chef-load-role-580613600]', 'role[chef-load-role-37386300]'],
      default_attributes: '{test:test}',
      override_attributes: '{test:test}'
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
      component.detailsFormGroup.controls['name'].setValue(role.name);
      component.detailsFormGroup.controls['description'].setValue(role.description);
      component.selectedRunList = role.run_list;
      component.defaultAttrFormGroup.controls['default'].setValue(
        JSON.stringify(role.default_attributes));
      component.overrideAttrFormGroup.controls['override'].setValue(
        JSON.stringify(role.override_attributes));
      component.createRole();
      const conflict = <HttpErrorResponse>{
        status: HttpStatus.CONFLICT,
        ok: false
      };
      store.dispatch(new CreateRoleFailure(conflict));
      expect(component.conflictError).toBe(true);
    });

    it('on success, closes modal and adds new role', () => {
      spyOn(component.conflictErrorEvent, 'emit');
      component.visible = true;
      component.detailsFormGroup.controls['name'].setValue(role.name);
      component.detailsFormGroup.controls['description'].setValue(role.description);
      component.selectedRunList = role.run_list;
      component.defaultAttrFormGroup.controls['default'].setValue(
        JSON.stringify(role.default_attributes));
      component.overrideAttrFormGroup.controls['override'].setValue(
        JSON.stringify(role.override_attributes));
      component.createRole();
      store.dispatch(new CreateRoleSuccess(role));
      expect(component.creating).toBe(false);
      expect(component.visible).toBe(false);
    });

    it('on create error, modal is closed (because error is handled by failure banner)', () => {
      spyOn(component.conflictErrorEvent, 'emit');
      component.detailsFormGroup.controls['name'].setValue(role.name);
      component.detailsFormGroup.controls['description'].setValue(role.description);
      component.selectedRunList = role.run_list;
      component.defaultAttrFormGroup.controls['default'].setValue(
        JSON.stringify(role.default_attributes));
      component.overrideAttrFormGroup.controls['override'].setValue(
        JSON.stringify(role.override_attributes));
      component.createRole();
      const error = <HttpErrorResponse>{
        status: HttpStatus.INTERNAL_SERVER_ERROR,
        ok: false
      };
      store.dispatch(new CreateRoleFailure(error));
      expect(component.conflictError).toBe(false);
    });
  });
});
