import { TestBed } from '@angular/core/testing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { By } from '@angular/platform-browser';
import { Observable, of as observableOf } from 'rxjs';
import { Store, StoreModule } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';

import { Rule, ServiceActionType } from './rule';
import { RulesService } from '../../services/rules/rules.service';
import { TelemetryService } from '../../services/telemetry/telemetry.service';
import { NotificationsComponent } from './notifications.component';
import { RouterTestingModule } from '@angular/router/testing';
import { FeatureFlagsService } from '../../services/feature-flags/feature-flags.service';

describe('NotificationsComponent', () => {
  let store: Store<NgrxStateAtom>;
  const rules: Rule[] = [
    new Rule('id1', 'test rule1', 'ComplianceFailure',
      'http://foo.com', ServiceActionType.SLACK, '', false),
    new Rule('id2', 'test rule2', 'CCRFailure',
      'http://foo.com', ServiceActionType.WEBHOOK, '', false)
  ];

  // CSS identifiers
  const cardId = '#notifications-cards';
  const listId = '#notifications-list';

  class MockTelemetryService {
    track() { }
  }

  class MockRulesService {
    fetchRules(): Observable<Rule[]> {
      return observableOf(rules);
    }

    deleteRule(rule) {
    return observableOf(rule);
    }
  }

  let telemetryService: TelemetryService;
  let fixture, component;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      declarations: [
        NotificationsComponent
      ],
      providers: [
        { provide: TelemetryService, useClass: MockTelemetryService },
        { provide: RulesService, useClass: MockRulesService },
        FeatureFlagsService
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    }).compileComponents();
    store = TestBed.inject(Store);
    spyOn(store, 'dispatch').and.callThrough();
    fixture = TestBed.createComponent(NotificationsComponent);
    component = fixture.componentInstance;
    telemetryService = TestBed.inject(TelemetryService);
    fixture.detectChanges();
  });

  describe ('in list view', () => {
    beforeEach(() => {
      component.cardView = false;
      fixture.detectChanges();
    });

    it ('shows only the table', () => {
      const listViewElement = fixture.debugElement.query(By.css(listId));
      const cardViewElement = fixture.debugElement.query(By.css(cardId));
      expect(listViewElement.name).toBe('chef-table');
      expect(cardViewElement).toBeNull();
    });

    it('shows expected item count', () => {
      const tableBody = getElementByCss(`${listId} chef-tr`);
      expect(tableBody.children.length).toBe(rules.length + 1);
    });

  });

  describe('sending telemetry', () => {
    beforeEach(() => {
      spyOn(telemetryService, 'track');
    });

    it('ensure telemetry is sent on deleting a rule', () => {
      component.deleteNotification();

      expect(telemetryService.track).toHaveBeenCalled();
    });

  });

  function getElementByCss(cssString: string) {
    return fixture
      .debugElement
      .query(By.css(cssString))
      .nativeElement;
  }

});
