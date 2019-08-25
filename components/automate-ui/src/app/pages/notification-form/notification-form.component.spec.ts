import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';
import { By } from '@angular/platform-browser';
import { Observable, of as observableOf } from 'rxjs';
import { MockComponent } from 'ng2-mock-component';

import { using } from 'app/testing/spec-helpers';
import { RulesService } from 'app/services/rules/rules.service';
import { ServiceActionType } from '../notifications/rule';
import { Rule, RuleType } from 'app/pages/notifications/rule';
import { NotificationFormComponent } from './notification-form.component';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { FeatureFlagsService } from '../../services/feature-flags/feature-flags.service';

describe('NotificationFormComponent', () => {
  let component: NotificationFormComponent;
  let fixture: ComponentFixture<NotificationFormComponent>;
  let ruleService: RulesService;
  const snapshot = { params: { id: 'id' }};

  class MockRulesService {
    fetchRules(): Observable<Rule[]> { return observableOf([]); }
    fetchRule(rule) { return observableOf(rule); }
    deleteRule(rule) { return observableOf(rule); }
    createRule(rule) { return observableOf(rule); }
    editRule(rule) { return observableOf(rule); }
    testHookWithUsernamePassword(_url,
      _username, _password): Observable<Object> { return observableOf({}); }
    testHookWithSecretId(_url, _secretId): Observable<Object> { return observableOf({}); }
    testHookWithNoCreds(_url): Observable<Object> { return observableOf({}); }
  }

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [
        NotificationFormComponent,
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-icon' }),
        MockComponent({ selector: 'app-settings-sidebar' }),
        MockComponent({ selector: 'chef-success-alert' }),
        MockComponent({ selector: 'chef-error-alert' }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        MockComponent({ selector: 'chef-modal', inputs: ['visible'] })
      ],
      providers: [
        { provide: RulesService, useClass: MockRulesService },
        { provide: ActivatedRoute, useValue: { snapshot: snapshot } },
        FeatureFlagsService
      ],
      imports: [
        FormsModule
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(NotificationFormComponent);
    component = fixture.componentInstance;
    ruleService = TestBed.get(RulesService);
    fixture.detectChanges();
  });

  it('creating a slack targetType rule should not save username and password', () => {
    const expectedUsername = 'bob';
    const expectedPassword = 'pass';

    spyOn(ruleService, 'createRule').and.callThrough();
    component.model.targetUsername = expectedUsername;
    component.model.targetPassword = expectedPassword;
    component.model.rule.targetType = ServiceActionType.SLACK;
    component.isEditRule = false;
    component.onSubmit();
    expect(ruleService.createRule).
      toHaveBeenCalledWith(jasmine.anything(), '', '');
  });

  it('creating a webhook targetType rule should not save username and password', () => {
    const expectedUsername = 'bob';
    const expectedPassword = 'pass';

    spyOn(ruleService, 'createRule').and.callThrough();
    component.model.targetUsername = expectedUsername;
    component.model.targetPassword = expectedPassword;
    component.model.rule.targetType = ServiceActionType.WEBHOOK;
    component.isEditRule = false;
    component.onSubmit();
    expect(ruleService.createRule).
      toHaveBeenCalledWith(jasmine.anything(), '', '');
  });

  it('editing a webhook targetType rule should not save username and password', () => {
    const expectedUsername = 'bob';
    const expectedPassword = 'pass';

    spyOn(ruleService, 'editRule').and.callThrough();
    component.model.targetUsername = expectedUsername;
    component.model.targetPassword = expectedPassword;
    component.model.rule.targetType = ServiceActionType.WEBHOOK;
    component.isEditRule = true;
    component.onSubmit();
    expect(ruleService.editRule).
      toHaveBeenCalledWith(jasmine.anything(), jasmine.anything(), '', '');
  });

  it('editing a slack targetType rule should not save username and password', () => {
    const expectedUsername = 'bob';
    const expectedPassword = 'pass';

    spyOn(ruleService, 'editRule').and.callThrough();
    component.model.targetUsername = expectedUsername;
    component.model.targetPassword = expectedPassword;
    component.model.rule.targetType = ServiceActionType.SLACK;
    component.isEditRule = true;
    component.onSubmit();
    expect(ruleService.editRule).
      toHaveBeenCalledWith(jasmine.anything(), jasmine.anything(), '', '');
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  it('renders form fields once webhook selection is made', () => {
    expect(component.targetTypeChosen()).toBe(false);
    component.onSelectionChange('slack');
    expect(component.targetTypeChosen()).toBe(true);
  });

  it('testHook with username and password set', () => {
    const expectedUsername = 'bob';
    const expectedPassword = 'pass';
    const expectedUrl = 'url';
    const event: Event = <Event>{
      preventDefault() { }
    };

    spyOn(ruleService, 'testHookWithUsernamePassword').and.callThrough();
    component.model.targetUsername = expectedUsername;
    component.model.targetPassword = expectedPassword;
    component.model.rule.targetUrl = expectedUrl;
    component.testHook(event);
    expect(ruleService.testHookWithUsernamePassword).
      toHaveBeenCalledWith(expectedUrl, expectedUsername, expectedPassword);
  });

  it('testHook with secretsID set', () => {
    const expectedSecretId = 'super_secret';
    const expectedUrl = 'url';
    const event: Event = <Event>{
      preventDefault() { }
    };

    spyOn(ruleService, 'testHookWithSecretId').and.callThrough();
    component.model.rule.targetSecretId = expectedSecretId;
    component.model.rule.targetUrl = expectedUrl;
    component.testHook(event);
    expect(ruleService.testHookWithSecretId).
      toHaveBeenCalledWith(expectedUrl, expectedSecretId);
  });

  it('testHook with no creds set', () => {
    const expectedUrl = 'url';
    const event: Event = <Event>{
      preventDefault() { }
    };

    spyOn(ruleService, 'testHookWithNoCreds').and.callThrough();
    component.model.rule.targetUrl = expectedUrl;
    component.testHook(event);
    expect(ruleService.testHookWithNoCreds).
      toHaveBeenCalledWith(expectedUrl);
  });

  using([
    ['non-empty name', false, 'non-empty name']
    // TODO: not sure why these are not working
    // ['', true, 'empty name'],
    // ['   ', true, 'spaces-only name']
  ],
    function (name: string, expectedValue: boolean, description: string) {
      it(`sets disabled state of save button to ${expectedValue} with ${description}`, () => {
        component.onSelectionChange('slack');
        fixture.detectChanges();
        const saveButton = getElementByCss('#save_button');
        expect(saveButton.disabled).toBe(true);

        component.model.rule.targetUrl = 'any';
        component.model.rule.ruleType = 'CCRFailure';
        component.model.rule.name = name;
        fixture.detectChanges();

        expect(saveButton.disabled).toBe(expectedValue);
      });
    });

  using([
    ['http://www.foo.com', false, 'non-empty url']
    // TODO: not sure why these are not working
    // ['', true, 'empty url'],
    // ['p://www.foo.com', true, 'invalid url']
  ],
    function (url: string, expectedValue: boolean, description: string) {
      it(`sets disabled state of save button to ${expectedValue} with ${description}`, () => {
        component.onSelectionChange('slack');
        fixture.detectChanges();
        const saveButton = getElementByCss('#save_button');
        expect(saveButton.disabled).toBe(true);

        component.model.rule.name = 'any';
        component.model.rule.ruleType = 'CCRFailure';
        component.model.rule.targetUrl = url;
        fixture.detectChanges();

        expect(saveButton.disabled).toBe(expectedValue);
      });
    });

  using([
    ['non-empty rule', false, 'non-empty rule'],
    ['', true, 'empty rule']
  ],
    function (ruleType: string, expectedValue: boolean, description: string) {
      it(`sets disabled state of save button to ${expectedValue} with ${description}`,
        () => {
          component.onSelectionChange('slack');
          fixture.detectChanges();
          const saveButton = getElementByCss('#save_button');
          expect(saveButton.disabled).toBe(true);

          component.model.rule.targetUrl = 'any';
          component.model.rule.name = 'any';
          component.model.rule.ruleType = <RuleType>ruleType;
          fixture.detectChanges();

          expect(saveButton.disabled).toBe(expectedValue);
        });
    });

  function getElementByCss(cssString: string) {
    return fixture
      .debugElement
      .query(By.css(cssString))
      .nativeElement;
  }

});
