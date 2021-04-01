import { EventEmitter } from '@angular/core';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ReactiveFormsModule, FormBuilder, Validators, FormGroup } from '@angular/forms';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';
import { Regex } from 'app/helpers/auth/regex';

import { CreateChefServerModalComponent } from './create-chef-server-modal.component';

describe('CreateChefServerModalComponent', () => {
  let component: CreateChefServerModalComponent;
  let fixture: ComponentFixture<CreateChefServerModalComponent>;

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
        MockComponent({ selector: 'chef-modal',
          inputs: ['visible'],
          outputs: ['close']
        }),
        CreateChefServerModalComponent
      ],
      imports: [
        ReactiveFormsModule
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateChefServerModalComponent);
    component = fixture.componentInstance;
    // This form must mimic the createForm including Validators
    component.createForm = new FormBuilder().group({
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      id: ['',
        [Validators.required, Validators.pattern(Regex.patterns.ID), Validators.maxLength(64)]],
      fqdn: ['', [Validators.required,
      Validators.pattern(Regex.patterns.NON_BLANK),
      Validators.pattern(Regex.patterns.VALID_FQDN)
      ]],
      ip_address: ['', [Validators.required,
      Validators.pattern(Regex.patterns.NON_BLANK),
      Validators.pattern(Regex.patterns.VALID_IP_ADDRESS)
      ]]
    });
    component.conflictErrorEvent = new EventEmitter();
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

      it('when only 3 of the 4 inputs are complete', () => {
        createForm.controls['name'].setValue('test');
        createForm.controls['id'].setValue('test');
        createForm.controls['fqdn'].setValue('test.net');

        errors = createForm.controls['ip_address'].errors || {};

        expect(createForm.valid).toBeFalsy();
        expect(errors['required']).toBeTruthy();
      });

      it('when the ip_address in invalid', () => {
        createForm.controls['name'].setValue('test');
        createForm.controls['id'].setValue('test');
        createForm.controls['fqdn'].setValue('chef.internal');

        createForm.controls['ip_address'].setValue('1.2234.3.4');
        errors = createForm.controls['ip_address'].errors || {};

        expect(createForm.valid).toBeFalsy();
        expect(errors['pattern']).toBeTruthy();
      });

      it('when the fqdn Top Level Domain is less than 2 characters', () => {
        createForm.controls['name'].setValue('test');
        createForm.controls['id'].setValue('test');
        createForm.controls['ip_address'].setValue('1.2.3.4');

        createForm.controls['fqdn'].setValue('chef.i');
        errors = createForm.controls['fqdn'].errors || {};

        expect(createForm.valid).toBeFalsy();
        expect(errors['pattern']).toBeTruthy();
      });

      it('when the fqdn Top Level Domain is longer than 25 characters', () => {
        createForm.controls['name'].setValue('test');
        createForm.controls['id'].setValue('test');
        createForm.controls['ip_address'].setValue('1.2.3.4');

        createForm.controls['fqdn'].setValue('chef.thistldisgoingtobetoolongwow');
        errors = createForm.controls['fqdn'].errors || {};

        expect(createForm.valid).toBeFalsy();
        expect(errors['pattern']).toBeTruthy();
      });
    });

    describe('the form should be valid', () => {
      it('when all 4 inputs are filled and valid', () => {
        expect(createForm.valid).toBeFalsy();
        createForm.controls['name'].setValue('test');
        createForm.controls['id'].setValue('test');
        createForm.controls['fqdn'].setValue('chef.internal');
        createForm.controls['ip_address'].setValue('1.2.3.4');
        expect(createForm.valid).toBeTruthy();
      });
    });
  });
});
