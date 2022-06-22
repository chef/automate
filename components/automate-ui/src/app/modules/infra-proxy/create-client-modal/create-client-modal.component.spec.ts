import { ComponentFixture, TestBed } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';
import { ReactiveFormsModule, FormBuilder, Validators, FormGroup } from '@angular/forms';
import { StoreModule } from '@ngrx/store';
import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { CreateClientModalComponent } from './create-client-modal.component';
import { ClientKey } from 'app/entities/clients/client.model';
import { EventEmitter } from '@angular/core';
import { Regex } from 'app/helpers/auth/regex';
import { using } from 'app/testing/spec-helpers';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

describe('CreateClientModalComponent', () => {
  let component: CreateClientModalComponent;
  let fixture: ComponentFixture<CreateClientModalComponent>;

  let createForm: FormGroup;
  let errors = {};

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-modal', inputs: ['visible'] }),
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-checkbox', inputs: ['checked']}),
        MockComponent({ selector: 'chef-snippet', inputs: ['code'] }),
        CreateClientModalComponent
      ],
      providers: [
        { provide: TelemetryService, useClass: MockTelemetryService }
      ],
      imports: [
        ReactiveFormsModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateClientModalComponent);
    component = fixture.componentInstance;
    component.createForm = new FormBuilder().group({
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK),
        Validators.pattern(Regex.patterns.NO_WILDCARD_ALLOW_HYPHEN)]],
      validator: ['']
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
        ['contains tilde.', 'client~'],
        ['contains acute, back quote,', 'client`'],
        ['contains exclamation mark', 'client!'],
        ['contains ampersat, at', 'client@'],
        ['contains dollar sign', 'client$'],
        ['contains percent.', 'client%'],
        ['contains caret or circumflex.,', 'client^'],
        ['contains ampersand', 'client&'],
        ['contains asterisk', 'client*'],
        ['contains open or left parenthesis.', 'client('],
        ['contains close or right parenthesis,', 'client)'],
        ['contains plus', 'client+'],
        ['contains equal', 'client='],
        ['contains open brace', 'client{'],
        ['contains close brace', 'client}'],
        ['contains open bracket', 'client['],
        ['contains closed bracket', 'client]'],
        ['contains pipe', 'client|'],
        ['contains backslash', 'client\\'],
        ['contains forward slash', 'client/'],
        ['contains colon', 'client:'],
        ['contains semicolon.', 'client;'],
        ['contains quote', 'client"'],
        ['contains apostrophe', 'client\'test'],
        ['contains less than', 'client<'],
        ['contains greater than', 'client>'],
        ['contains comma', 'client,'],
        ['contains period, dot', 'client.'],
        ['contains question mark', 'client?'],
        ['contains space', 'client test1'],
        ['has mixed alphabet, number, special character', 'client-test!+ test1']
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
        createForm.controls['name'].setValue('client');
        expect(createForm.valid).toBeTruthy();
      });

      using([
        ['contains numbers range 0-9.', 'client123'],
        ['contains alphabets a-z', 'client-test'],
        ['contains underscore.', 'client_test'],
        ['contains hyphen, minus, or dash.', 'client_test-1'],
        ['has mixed characters', 'client-Test_10']
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

  describe('#createClientForm', () => {
    const clientKey: ClientKey = {
      name: 'test_name',
      public_key: 'test_public_key',
      expiration_date: 'test_expiration_date',
      private_key: 'test_private_key'
    };

    it('should be invalid when no fields are filled out', () => {
      expect(component.createForm.valid).toBeFalsy();
    });

    it('should be valid when all fields are filled out', () => {
      component.createForm.controls['name'].setValue(clientKey.name);
      expect(component.createForm.valid).toBeTruthy();
    });

    it('opening create modal resets name to empty string', () => {
      component.visible = true;
      expect(component.createForm.controls['name'].value).toEqual('');
    });
  });

});
