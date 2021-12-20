import { ComponentFixture, TestBed } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';
import { ReactiveFormsModule, FormBuilder, Validators, FormGroup } from '@angular/forms';
import { UpdateWebUIKeySliderComponent } from './update-web-uikey-slider.component';
import { StoreModule, Store } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { HttpClient, HttpHandler } from '@angular/common/http';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { Regex } from 'app/helpers/auth/regex';
import { using } from 'app/testing/spec-helpers';
import { WebUIKey } from 'app/entities/servers/server.model';
import { UpdateWebUIKeyFailure, UpdateWebUIKeySuccess, WebUIKeyPayload } from 'app/entities/servers/server.actions';
import { HttpStatus } from 'app/types/types';
import { HttpErrorResponse } from '@angular/common/http';

describe('UpdateWebUIKeySliderComponent', () => {
  let component: UpdateWebUIKeySliderComponent;
  let fixture: ComponentFixture<UpdateWebUIKeySliderComponent>;

  let updateKeyForm: FormGroup;
  let errors = {};

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-icon' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        UpdateWebUIKeySliderComponent
      ],
        imports: [
        ReactiveFormsModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
        ],
        providers: [
          HttpClient,
          HttpHandler
        ],
        schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
      })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(UpdateWebUIKeySliderComponent);
    component = fixture.componentInstance;
    component.updateKeyForm = new FormBuilder().group({
      webuikey: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK),
        Validators.pattern(Regex.patterns.NO_WILDCARD_ALLOW_HYPHEN)]]
    });
    updateKeyForm = component.updateKeyForm;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('form validity', () => {
    describe('the form should be invalid', () => {
      it('when all inputs are empty', () => {
        expect(updateKeyForm.valid).toBeFalsy();
      });

      it('when webuikey is missing', () => {

        errors = updateKeyForm.controls['webuikey'].errors || {};

        expect(updateKeyForm.valid).toBeFalsy();
        expect(errors['required']).toBeTruthy();
      });

    describe('the form should be valid', () => {
      it('when all inputs are filled and valid', () => {
        expect(updateKeyForm.valid).toBeFalsy();
        updateKeyForm.controls['webuikey'].setValue('WebUIKey');
        expect(updateKeyForm.valid).toBeTruthy();
      });

      using([
        ['contains numbers range 0-9.', 'WebUIKey123'],
        ['contains alphabets a-z', 'WebUIKey-test'],
        ['contains underscore.', 'WebUIKey_test'],
        ['contains hyphen, minus, or dash.', 'WebUIKey_test-1'],
        ['has mixed characters', 'WebUIKey-Test_10']
      ], function (description: string, input: string) {
        it(('when the webuikey ' + description), () => {

          updateKeyForm.controls['webuikey'].setValue(input);
          errors = updateKeyForm.controls['webuikey'].errors || {};

          expect(updateKeyForm.valid).toBeTruthy();
          expect(errors['pattern']).toBeFalsy();
        });
      });
    });
  });

  describe('#webuikey', () => {
    let store: Store<NgrxStateAtom>;
    const webuikey: WebUIKey = {
      server_id: 'test_server',
      key: 'test_webuikey'
    };

    const key: WebUIKeyPayload = {
      server_id: 'test_server',
      key: 'test_webuikey'
    };

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('render the UI Key Slider', () => {
      component.slidePanel();
      expect(component.isSlideOpen).toBe(true);
    });

    it('should be invalid when no fields are filled out', () => {
      expect(component.updateKeyForm.valid).toBeFalsy();
    });

    it('should be valid when all fields are filled out', () => {
      component.updateKeyForm.controls['webuikey'].setValue(webuikey.key);
      expect(component.updateKeyForm.valid).toBeTruthy();
    });

    it('hide slider after updating webuikey.', () => {
      component.updateKeyForm.controls['webuikey'].setValue(webuikey.key);
      component.onSubmit();

      store.dispatch(new UpdateWebUIKeySuccess(
        { server_id: key.server_id, key: key.key}));
    });

    it('on create , slider is closed with failure banner', () => {
      component.updateKeyForm.controls['webuikey'].setValue(webuikey.key);
      component.onSubmit();

      const error = <HttpErrorResponse>{
        status: HttpStatus.INTERNAL_SERVER_ERROR,
        ok: false
      };
      store.dispatch(new UpdateWebUIKeyFailure(error));
      expect(component.conflictError).toBe(false);
    });
  });
 });
});
