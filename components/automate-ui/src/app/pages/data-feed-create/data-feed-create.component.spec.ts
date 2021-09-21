import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';

import { DataFeedCreateComponent } from './data-feed-create.component';
import { RouterTestingModule } from '@angular/router/testing';
import { FormBuilder, FormGroup, FormsModule, ReactiveFormsModule, Validators } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { StoreModule } from '@ngrx/store';
import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { Regex } from 'app/helpers/auth/regex';
import { Destination } from 'app/entities/destinations/destination.model';

describe('DataFeedCreateComponent', () => {
  let component: DataFeedCreateComponent;
  let fixture: ComponentFixture<DataFeedCreateComponent>;
  // let element;
  let createForm: FormGroup;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
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
        DataFeedCreateComponent
      ],
      providers: [
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
    fixture = TestBed.createComponent(DataFeedCreateComponent);
    component = fixture.componentInstance;
    component.createForm = new FormBuilder().group({
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      endpoint: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      url: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      tokenType: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      token: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      username: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      password: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      headers: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      bucketName: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      accessKey: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      secretKey: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]]
    });
    createForm = component.createForm;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('Data Feed Create', () => {
    const tokenType = 'Bearer';
    const token = 'test123';
    const bucketName = 'bar';
    const accessKey = 'test123';
    const secretKey = 'test123';
    const userName = 'test123';
    const password = 'test123';
    const header = '{“test”:”123”}';
    const region = 'region1';
    const destination = <Destination> {
      id: '1',
      name: 'new data feed',
      secret: 'testSecret',
      url: 'http://foo.com'
    };

    it('should be invalid when no fields are filled out', () => {
      expect(createForm.valid).toBeFalsy();
    });

    it('opening create slider resets name, url, token to empty string', () => {
      component.createForm.controls['name'].setValue('any');
      component.createForm.controls['url'].setValue('any');
      component.createForm.controls['tokenType'].setValue('Bearer');
      component.createForm.controls['token'].setValue('any');
      component.name = jasmine.createSpyObj('name', ['nativeElement']);
      component.name.nativeElement = { focus: () => { }};
      component.selectIntegration('Splunk');
      expect(component.createForm.controls['name'].value).toBe(null);
      expect(component.createForm.controls['url'].value).toBe(null);
      expect(component.createForm.controls['tokenType'].value).toBe('Splunk');
      expect(component.createForm.controls['token'].value).toBe(null);
    });

    it('should be valid when all fields are filled out for service-now', () => {
      component.name = jasmine.createSpyObj('name', ['nativeElement']);
      component.name.nativeElement = { focus: () => { }};
      component.selectIntegration('ServiceNow');
      // component.selectChangeHandlers('Access Token');
      component.createForm.controls['name'].setValue(destination.name);
      component.createForm.controls['url'].setValue(destination.url);
      component.createForm.controls['username'].setValue(userName);
      component.createForm.controls['password'].setValue(password);
      expect(component.validateForm()).toBeTruthy();
    });

    it('should be valid when all fields are filled out for ELK', () => {
      component.name = jasmine.createSpyObj('name', ['nativeElement']);
      component.name.nativeElement = { focus: () => { }};
      component.selectIntegration('ELK');
      // component.selectChangeHandlers('Access Token');
      component.createForm.controls['name'].setValue(destination.name);
      component.createForm.controls['url'].setValue(destination.url);
      component.createForm.controls['username'].setValue(userName);
      component.createForm.controls['password'].setValue(password);
      expect(component.validateForm()).toBeTruthy();
    });

    it('should be valid when all fields are filled out for splunk', () => {
      component.name = jasmine.createSpyObj('name', ['nativeElement']);
      component.name.nativeElement = { focus: () => { }};
      component.selectIntegration('Splunk');
      // component.selectChangeHandlers('Access Token');
      component.createForm.controls['name'].setValue(destination.name);
      component.createForm.controls['url'].setValue(destination.url);
      component.createForm.controls['tokenType'].setValue(tokenType);
      component.createForm.controls['token'].setValue(token);
      expect(component.validateForm()).toBeTruthy();
    });

    it('should be valid when all fields are filled out for minio', () => {
      component.name = jasmine.createSpyObj('name', ['nativeElement']);
      component.name.nativeElement = { focus: () => { }};
      component.selectIntegration('Minio');
      // component.selectChangeHandlers('Access Token');
      component.createForm.controls['name'].setValue(destination.name);
      component.createForm.controls['endpoint'].setValue(destination.url);
      component.createForm.controls['bucketName'].setValue(bucketName);
      component.createForm.controls['accessKey'].setValue(accessKey);
      component.createForm.controls['secretKey'].setValue(secretKey);
      expect(component.validateForm()).toBeTruthy();
    });

    it('should be valid when all fields are filled out for custom with Access token', () => {
      component.name = jasmine.createSpyObj('name', ['nativeElement']);
      component.name.nativeElement = { focus: () => { }};
      component.selectIntegration('Custom');
      component.createForm.controls['name'].setValue(destination.name);
      component.createForm.controls['url'].setValue(destination.url);
      component.createForm.controls['tokenType'].setValue(tokenType);
      component.createForm.controls['token'].setValue(token);
      expect(component.validateForm()).toBeTruthy();
    });

    it('should be valid when all fields are filled out for custom with UsernamePassword', () => {
      component.name = jasmine.createSpyObj('name', ['nativeElement']);
      component.name.nativeElement = { focus: () => { }};
      component.selectIntegration('Custom');
      component.selectChangeHandlers('Username and Password');
      component.createForm.controls['name'].setValue(destination.name);
      component.createForm.controls['url'].setValue(destination.url);
      component.createForm.controls['username'].setValue(userName);
      component.createForm.controls['password'].setValue(password);
      component.createForm.controls['headers'].setValue(header);
      expect(component.validateForm()).toBeTruthy();
    });

    it('slider resets name, url, username and password to empty string for servicenow', () => {
      component.createForm.controls['name'].setValue('any');
      component.createForm.controls['url'].setValue('any');
      component.createForm.controls['tokenType'].setValue(tokenType);
      component.createForm.controls['token'].setValue('any');
      component.slidePanel();
      component.name = jasmine.createSpyObj('name', ['nativeElement']);
      component.name.nativeElement = { focus: () => { }};
      component.selectIntegration('ServiceNow');
      expect(component.createForm.controls['name'].value).toBe(null);
      expect(component.createForm.controls['url'].value).toBe(null);
      expect(component.createForm.controls['tokenType'].value).toBe(tokenType);
      expect(component.createForm.controls['token'].value).toBe(null);
    });

    it('slider resets name, url, username and password to empty string for splunk', () => {
      component.createForm.controls['name'].setValue('any');
      component.createForm.controls['url'].setValue('any');
      component.createForm.controls['tokenType'].setValue(tokenType);
      component.createForm.controls['token'].setValue('any');
      component.slidePanel();
      component.name = jasmine.createSpyObj('name', ['nativeElement']);
      component.name.nativeElement = { focus: () => { }};
      component.selectIntegration('Splunk');
      expect(component.createForm.controls['name'].value).toBe(null);
      expect(component.createForm.controls['url'].value).toBe(null);
      expect(component.createForm.controls['tokenType'].value).toBe('Splunk');
      expect(component.createForm.controls['token'].value).toBe(null);
    });

    it('slider resets name, url, username and password to empty string for ELK', () => {
      component.createForm.controls['name'].setValue('any');
      component.createForm.controls['url'].setValue('any');
      component.createForm.controls['tokenType'].setValue(tokenType);
      component.createForm.controls['token'].setValue('any');
      component.slidePanel();
      component.name = jasmine.createSpyObj('name', ['nativeElement']);
      component.name.nativeElement = { focus: () => { }};
      component.selectIntegration('ELK');
      expect(component.createForm.controls['name'].value).toBe(null);
      expect(component.createForm.controls['url'].value).toBe(null);
      expect(component.createForm.controls['tokenType'].value).toBe('Bearer');
      expect(component.createForm.controls['token'].value).toBe(null);
    });

    it('slider resets name, url, username and password to empty string for Minio', () => {
      component.createForm.controls['name'].setValue('any');
      component.createForm.controls['endpoint'].setValue('any');
      component.createForm.controls['bucketName'].setValue('any');
      component.createForm.controls['accessKey'].setValue('any');
      component.createForm.controls['secretKey'].setValue('any');
      component.slidePanel();
      component.name = jasmine.createSpyObj('name', ['nativeElement']);
      component.name.nativeElement = { focus: () => { }};
      component.selectIntegration('Minio');
      expect(component.createForm.controls['name'].value).toBe(null);
      expect(component.createForm.controls['endpoint'].value).toBe(null);
      expect(component.createForm.controls['bucketName'].value).toBe(null);
      expect(component.createForm.controls['accessKey'].value).toBe(null);
      expect(component.createForm.controls['secretKey'].value).toBe(null);
    });

    it('should be valid when all fields are filled out for S3', () => {
      component.name = jasmine.createSpyObj('name', ['nativeElement']);
      component.name.nativeElement = { focus: () => {} };
      component.selectIntegration('S3');
      component.createForm.controls['name'].setValue(destination.name);
      component.createForm.controls['bucketName'].setValue(bucketName);
      component.createForm.controls['accessKey'].setValue(accessKey);
      component.createForm.controls['secretKey'].setValue(secretKey);
      expect(component.validateForm()).toBeTruthy();
    });

    it('Dropdown is populated for S3', () => {
      expect(component.dropDownVal).toBe(null);
      component.dropDownChangeHandlers(region);
      expect(component.dropDownVal).toBe(region);
    });

    it('slider resets name, bucketname , accessKey and secretkey to empty string for S3', () => {
      component.createForm.controls['name'].setValue('any');
      component.createForm.controls['bucketName'].setValue('any');
      component.createForm.controls['accessKey'].setValue('any');
      component.createForm.controls['secretKey'].setValue('any');
      component.slidePanel();
      component.name = jasmine.createSpyObj('name', ['nativeElement']);
      component.name.nativeElement = { focus: () => {} };
      component.selectIntegration('Amazon S3');
      expect(component.createForm.controls['name'].value).toBe(null);
      expect(component.createForm.controls['bucketName'].value).toBe(null);
      expect(component.createForm.controls['accessKey'].value).toBe(null);
      expect(component.createForm.controls['secretKey'].value).toBe(null);
    });



  });

  describe('create data feed form validation', () => {

    it('- url field validity', () => {
      component.integrationSelected = true;
      component.name = jasmine.createSpyObj('name', ['nativeElement']);
      component.name.nativeElement = { focus: () => { }};
      component.selectIntegration('ServiceNow');
      expect(component.createForm.controls['url'].value).toBe(null);

      let errors = {};
      const url = component.createForm.controls['url'];
      expect(url.valid).toBeFalsy();

      // url field is required
      errors = url.errors || {};
      expect(errors['required']).toBeTruthy();

      url.setValue('');
      errors = url.errors || {};
      expect(errors['required']).toBeTruthy();

      // Set url to invalid inputs
      url.setValue('  ');
      errors = url.errors || {};
      expect(errors['required']).toBeFalsy();
    });
  });
});
