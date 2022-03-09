import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { ReactiveFormsModule, FormBuilder, Validators, FormGroup } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { HttpErrorResponse } from '@angular/common/http';
import { MockComponent } from 'ng2-mock-component';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { CreateDataBagModalComponent } from './create-data-bag-modal.component';
import { EventEmitter } from '@angular/core';
import { HttpClient, HttpHandler } from '@angular/common/http';
import { HttpStatus } from 'app/types/types';
import { MatSelectModule } from '@angular/material/select';
import {
  CreateDataBagSuccess,
  CreateDataBagFailure
} from 'app/entities/data-bags/data-bags.actions';
import {
  DataBag
} from 'app/entities/data-bags/data-bags.model';
import { Regex } from 'app/helpers/auth/regex';
import { using } from 'app/testing/spec-helpers';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

describe('CreateDataBagModalComponent', () => {
  let component: CreateDataBagModalComponent;
  let fixture: ComponentFixture<CreateDataBagModalComponent>;

  let createForm: FormGroup;
  let errors = {};

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-modal',
          inputs: ['visible'],
          outputs: ['close']
        }),
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-toolbar' }),
        CreateDataBagModalComponent
      ],
      imports: [
        MatSelectModule,
        ReactiveFormsModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      providers: [
        HttpClient,
        HttpHandler,
        { provide: TelemetryService, useClass: MockTelemetryService }
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateDataBagModalComponent);
    component = fixture.componentInstance;
    component.createForm = new FormBuilder().group({
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK),
        Validators.pattern(Regex.patterns.NO_WILDCARD_ALLOW_HYPHEN)]]
    });
    component.openEvent = new EventEmitter();
    createForm = component.createForm;
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

        errors = createForm.controls['name'].errors || {};

        expect(createForm.valid).toBeFalsy();
        expect(errors['required']).toBeTruthy();
      });

      using([
        ['contains tilde.', 'databag~'],
        ['contains acute, back quote,', 'databag`'],
        ['contains exclamation mark', 'databag!'],
        ['contains ampersat, at', 'databag@'],
        ['contains dollar sign', 'databag$'],
        ['contains percent.', 'databag%'],
        ['contains caret or circumflex.,', 'databag^'],
        ['contains ampersand', 'databag&'],
        ['contains asterisk', 'databag*'],
        ['contains open or left parenthesis.', 'databag('],
        ['contains close or right parenthesis,', 'databag)'],
        ['contains plus', 'databag+'],
        ['contains equal', 'databag='],
        ['contains open brace', 'databag{'],
        ['contains close brace', 'databag}'],
        ['contains open bracket', 'databag['],
        ['contains closed bracket', 'databag]'],
        ['contains pipe', 'databag|'],
        ['contains backslash', 'databag\\'],
        ['contains forward slash', 'databag/'],
        ['contains colon', 'databag:'],
        ['contains semicolon.', 'databag;'],
        ['contains quote', 'databag"'],
        ['contains apostrophe', 'databag\'test'],
        ['contains less than', 'databag<'],
        ['contains greater than', 'databag>'],
        ['contains comma', 'databag,'],
        ['contains period, dot', 'databag.'],
        ['contains question mark', 'databag?'],
        ['contains space', 'databag test1'],
        ['has mixed alphabet, number, special character', 'databag-test!+ test1']
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
        createForm.controls['name'].setValue('databag');
        expect(createForm.valid).toBeTruthy();
      });

      using([
        ['contains numbers range 0-9.', 'databag123'],
        ['contains alphabets a-z', 'databag-test'],
        ['contains underscore.', 'databag_test'],
        ['contains hyphen, minus, or dash.', 'databag_test-1'],
        ['has mixed characters', 'databag-Test_10']
      ], function (description: string, input: string) {
        it(('when the name ' + description), () => {

          createForm.controls['name'].setValue(input);
          errors = createForm.controls['name'].errors || {};

          expect(createForm.valid).toBeTruthy();
          expect(errors['pattern']).toBeFalsy();
        });
      });
    });
  });

  describe('#createDataBagForm', () => {
    let store: Store<NgrxStateAtom>;
    const dataBag: DataBag = {
      server_id: 'test_server',
      org_id: 'test_server',
      name: 'test_data_bag'
    };

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('should be invalid when no fields are filled out', () => {
      expect(component.createForm.valid).toBeFalsy();
    });

     it('should be valid when all fields are filled out', () => {
      component.createForm.controls['name'].setValue(dataBag.name);
      expect(component.createForm.valid).toBeTruthy();
    });

    it('opening create modal resets name to empty string',
      () => {
      component.visible = true;
      expect(component.createForm.controls['name'].value).toEqual('');
    });

    it('hide modal after create a data bag.', () => {
      component.createForm.controls['name'].setValue(dataBag.name);
      component.createDataBag();

      store.dispatch(new CreateDataBagSuccess({databag: dataBag}));
      expect(component.visible).toBe(false);
    });

    it('on create error, modal is closed with failure banner', () => {
      component.createForm.controls['name'].setValue(dataBag.name);
      component.createDataBag();

      const error = <HttpErrorResponse>{
        status: HttpStatus.INTERNAL_SERVER_ERROR,
        ok: false
      };
      store.dispatch(new CreateDataBagFailure(error));
      expect(component.conflictError).toBe(false);
    });
  });
});
