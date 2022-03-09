import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { ReactiveFormsModule, FormBuilder, Validators, FormGroup } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { HttpErrorResponse } from '@angular/common/http';
import { MockComponent } from 'ng2-mock-component';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { EventEmitter } from '@angular/core';
import { HttpClient, HttpHandler } from '@angular/common/http';
import { HttpStatus } from 'app/types/types';
import { CreateDatabagItemModalComponent } from './create-databag-item-modal.component';
import {
  CreateDataBagItemSuccess,
  CreateDataBagItemSuccessPayload,
  CreateDataBagItemFailure
} from 'app/entities/data-bags/data-bag-details.actions';
import {
  DataBagItem
} from 'app/entities/data-bags/data-bags.model';
import { Regex } from 'app/helpers/auth/regex';
import { using } from 'app/testing/spec-helpers';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

describe('CreateDatabagItemModalComponent', () => {
  let component: CreateDatabagItemModalComponent;
  let fixture: ComponentFixture<CreateDatabagItemModalComponent>;

  let createForm: FormGroup;
  let errors = {};

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-modal', inputs: ['visible'] }),
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-toolbar' }),
        CreateDatabagItemModalComponent
      ],
      imports: [
        ReactiveFormsModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      providers: [
        HttpClient,
        HttpHandler,
        { provide: TelemetryService, useClass: MockTelemetryService }
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateDatabagItemModalComponent);
    component = fixture.componentInstance;
    component.createForm = new FormBuilder().group({
      itemId: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK),
        Validators.pattern(Regex.patterns.NO_WILDCARD_ALLOW_HYPHEN)]],
      itemAttr: ['{}']
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

        errors = createForm.controls['itemId'].errors || {};

        expect(createForm.valid).toBeFalsy();
        expect(errors['required']).toBeTruthy();
      });

      using([
        ['contains tilde.', 'item~'],
        ['contains acute, back quote,', 'item`'],
        ['contains exclamation mark', 'item!'],
        ['contains ampersat, at', 'item@'],
        ['contains dollar sign', 'item$'],
        ['contains percent.', 'item%'],
        ['contains caret or circumflex.,', 'item^'],
        ['contains ampersand', 'item&'],
        ['contains asterisk', 'item*'],
        ['contains open or left parenthesis.', 'item('],
        ['contains close or right parenthesis,', 'item)'],
        ['contains plus', 'item+'],
        ['contains equal', 'item='],
        ['contains open brace', 'item{'],
        ['contains close brace', 'item}'],
        ['contains open bracket', 'item['],
        ['contains closed bracket', 'item]'],
        ['contains pipe', 'item|'],
        ['contains backslash', 'item\\'],
        ['contains forward slash', 'item/'],
        ['contains colon', 'item:'],
        ['contains semicolon.', 'item;'],
        ['contains quote', 'item"'],
        ['contains apostrophe', 'item\'test'],
        ['contains less than', 'item<'],
        ['contains greater than', 'item>'],
        ['contains comma', 'item,'],
        ['contains period, dot', 'item.'],
        ['contains question mark', 'item?'],
        ['contains space', 'item test1'],
        ['has mixed alphabet, number, special character', 'item-test!+ test1']
      ], function (description: string, input: string) {
        it(('when the item id ' + description), () => {

          createForm.controls['itemId'].setValue(input);
          errors = createForm.controls['itemId'].errors || {};

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
        it(('when the item id only' + description), () => {

          createForm.controls['itemId'].setValue(input);
          errors = createForm.controls['itemId'].errors || {};

          expect(createForm.valid).toBeFalsy();
          expect(errors['pattern']).toBeTruthy();
        });
      });
    });

    describe('the form should be valid', () => {
      it('when all inputs are filled and valid', () => {
        expect(createForm.valid).toBeFalsy();
        createForm.controls['itemId'].setValue('item');
        expect(createForm.valid).toBeTruthy();
      });

      using([
        ['contains numbers range 0-9.', 'item123'],
        ['contains alphabets a-z', 'item-test'],
        ['contains underscore.', 'item_test'],
        ['contains hyphen, minus, or dash.', 'item_test-1'],
        ['has mixed characters', 'item-Test_10']
      ], function (description: string, input: string) {
        it(('when the name ' + description), () => {

          createForm.controls['itemId'].setValue(input);
          errors = createForm.controls['itemId'].errors || {};

          expect(createForm.valid).toBeTruthy();
          expect(errors['pattern']).toBeFalsy();
        });
      });
    });
  });

  describe('#createDataBagItemForm', () => {
    let store: Store<NgrxStateAtom>;
    const dataBagItem: DataBagItem = {
      server_id: 'test_server',
      org_id: 'test_server',
      name: 'test_data_bag',
      data: 'test_data'
    };

    const responseData: CreateDataBagItemSuccessPayload = {
      name: 'test_name',
      id: 'test_id'
    };

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('should be invalid when no fields are filled out', () => {
      expect(component.createForm.valid).toBeFalsy();
    });

    it('should be valid when all fields are filled out', () => {
      component.createForm.controls['itemId'].setValue(dataBagItem.data);
      expect(component.createForm.valid).toBeTruthy();
    });

    it('opening create modal resets ID to empty string',
      () => {
      component.visible = true;
      expect(component.createForm.controls['itemId'].value).toEqual('');
    });

    it('hide modal after create a data bag.', () => {
      component.createForm.controls['itemId'].setValue(dataBagItem.data);
      component.createDataBagItem();

      store.dispatch(new CreateDataBagItemSuccess({name: responseData.name, id: responseData.id}));
      expect(component.visible).toBe(false);
    });

    it('on create error, modal is closed with failure banner', () => {
      component.createForm.controls['itemId'].setValue(dataBagItem.data);
      component.createDataBagItem();

      const error = <HttpErrorResponse>{
        status: HttpStatus.INTERNAL_SERVER_ERROR,
        ok: false
      };
      store.dispatch(new CreateDataBagItemFailure(error));
      expect(component.conflictError).toBe(false);
    });

  });
});
