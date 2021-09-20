import { EventEmitter } from '@angular/core';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ReactiveFormsModule, FormBuilder, Validators, FormGroup } from '@angular/forms';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';
import { Regex } from 'app/helpers/auth/regex';
import { using } from 'app/testing/spec-helpers';

import { CreateChefServerModalComponent } from './create-chef-server-modal.component';

describe('CreateChefServerModalComponent', () => {
  let component: CreateChefServerModalComponent;
  let fixture: ComponentFixture<CreateChefServerModalComponent>;

  let createForm: FormGroup;
  let fqdnForm: FormGroup;
  let ipForm: FormGroup;

  let errors = {};

  beforeEach( waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-toolbar' }),
        MockComponent({ selector: 'chef-select' }),
        MockComponent({ selector: 'chef-option' }),
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
        [Validators.required, Validators.pattern(Regex.patterns.ID), Validators.maxLength(64)]]
    });
    component.fqdnForm = new FormBuilder().group({
      fqdn: ['', [Validators.required,
        Validators.pattern(Regex.patterns.NON_BLANK),
        Validators.pattern(Regex.patterns.VALID_FQDN)
      ]]
    });
    component.ipForm = new FormBuilder().group({
      ip_address: ['', [Validators.required,
        Validators.pattern(Regex.patterns.NON_BLANK),
        Validators.pattern(Regex.patterns.VALID_IP_ADDRESS)
      ]]
    });
    component.conflictErrorEvent = new EventEmitter();
    createForm = component.createForm;
    fqdnForm = component.fqdnForm;
    ipForm = component.ipForm;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('form validity', () => {
    describe('the form should be invalid', () => {
      it('when all inputs are empty', () => {
        expect(createForm.valid).toBeFalsy();
        expect(fqdnForm.valid).toBeFalsy();
        expect(ipForm.valid).toBeFalsy();
      });

      it('when name is missing', () => {
        createForm.controls['id'].setValue('test');
        fqdnForm.controls['fqdn'].setValue('test.net');
        ipForm.controls['ip_address'].setValue('1.2.3.4');

        errors = createForm.controls['name'].errors || {};

        expect(createForm.valid).toBeFalsy();
        expect(errors['required']).toBeTruthy();
      });

      it('when id is missing', () => {
        createForm.controls['name'].setValue('test');
        fqdnForm.controls['fqdn'].setValue('test.net');
        ipForm.controls['ip_address'].setValue('1.2.3.4');

        errors = createForm.controls['id'].errors || {};

        expect(createForm.valid).toBeFalsy();
        expect(errors['required']).toBeTruthy();
      });

      it('when fqdn is missing', () => {
        createForm.controls['name'].setValue('test');
        createForm.controls['id'].setValue('test');
        ipForm.controls['ip_address'].setValue('1.2.3.4');

        errors = fqdnForm.controls['fqdn'].errors || {};

        expect(fqdnForm.valid).toBeFalsy();
        expect(errors['required']).toBeTruthy();
      });

      it('when ip_address is missing', () => {
        createForm.controls['name'].setValue('test');
        createForm.controls['id'].setValue('test');
        fqdnForm.controls['fqdn'].setValue('test.net');

        errors = ipForm.controls['ip_address'].errors || {};

        expect(ipForm.valid).toBeFalsy();
        expect(errors['required']).toBeTruthy();
      });

      it('when the ip_address in invalid', () => {
        createForm.controls['name'].setValue('test');
        createForm.controls['id'].setValue('test');
        fqdnForm.controls['fqdn'].setValue('chef.internal');

        ipForm.controls['ip_address'].setValue('1.2234.3.4');
        errors = ipForm.controls['ip_address'].errors || {};

        expect(ipForm.valid).toBeFalsy();
        expect(errors['pattern']).toBeTruthy();
      });

      // Many testing ideas attributed to https://mathiasbynens.be/demo/url-regex

      using([
        ['is using something other than http or https', 'httpld://www.chef.io'],
        ['contains two periods', 'chef..internal'],
        ['there is no TLD suffix', 'http://foo.com.'],
        ['contains hyphens in the TLD', 'chef.this-will-not'],
        ['has a TLD that is longer than 25 characters', 'chef.thisisareallylongtldandwontwork'],
        ['has a TLD that is shorter than 2 characters', 'chef.i'],
        ['has numbers in the TLD', 'chef.017'],
        ['has a port number that is too high', 'https://chef.io:987274892'],
        ['has a colon but no port number', 'https://chef.io:'],
        ['has a letter in the port', 'https://chef.io:123a'],
        ['has no domain', 'http://'],
        ['has no secure domain', 'https://'],
        ['domain is dots', 'https://..'],
        ['domain is hash', 'http://#'],
        ['domain has a space', 'http:// shouldfail.net'],
        ['contains all numbers', 'http://10.1.1.0']
      ], function (description: string, input: string) {
        it(('when the fqdn ' + description), () => {
          createForm.controls['name'].setValue('test');
          createForm.controls['id'].setValue('test');
          ipForm.controls['ip_address'].setValue('1.2.3.4');

          fqdnForm.controls['fqdn'].setValue(input);
          errors = fqdnForm.controls['fqdn'].errors || {};

          expect(fqdnForm.valid).toBeFalsy();
          expect(errors['pattern']).toBeTruthy();
        });
      });
    });



    describe('the form should be valid', () => {
      it('when all 4 inputs are filled and valid', () => {
        expect(createForm.valid).toBeFalsy();
        createForm.controls['name'].setValue('test');
        createForm.controls['id'].setValue('test');
        fqdnForm.controls['fqdn'].setValue('chef.internal');
        ipForm.controls['ip_address'].setValue('1.2.3.4');
        expect(createForm.valid).toBeTruthy();
      });

      using([
        ['has a TLD that is longer than 2 and less than 25 characters', 'chef.internal'],
        ['uses https', 'https://chef.io'],
        ['uses http', 'http://chef.io'],
        ['omits http or https', 'chef.thisworks'],
        ['has a port number', 'https://chef.io:123'],
        ['contains hyphens in the domain', 'new-company-who-dis.nice'],
        ['contains underscores in the domain', 'new_company_who_dis.chef'],
        ['contains digits in the domain', '8675309.jenny'],
        ['contains all numbers in the domain', 'http://223.453.739.net'],
        ['contains a query', 'http://www.chef.io/events/#&product=automate'],
        ['contains unicode', 'http://foo.com/unicode_(✪)_in_parens'],
        ['is a citation url', 'http://foo.com/blah_(wikipedia)_blah#cite-1']
      ], function (description: string, input: string) {
        it(('when the fqdn ' + description), () => {
          createForm.controls['name'].setValue('test');
          createForm.controls['id'].setValue('test');
          ipForm.controls['ip_address'].setValue('1.2.3.4');

          fqdnForm.controls['fqdn'].setValue(input);
          errors = fqdnForm.controls['fqdn'].errors || {};

          expect(fqdnForm.valid).toBeTruthy();
          expect(errors['pattern']).toBeFalsy();
        });
      });

    });
  });
});
