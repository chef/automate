import { TestBed } from '@angular/core/testing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { MatDialog, MatSnackBar } from '@angular/material';
import { By } from '@angular/platform-browser';
import { Observable, of as observableOf } from 'rxjs';
import { Store } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { Rule, ServiceActionType } from './rule';
import { RulesService } from '../../services/rules/rules.service';
import { TelemetryService } from '../../services/telemetry/telemetry.service';
import { NotificationsComponent } from './notifications.component';
import { RouterTestingModule } from '@angular/router/testing';
import { FeatureFlagsService } from '../../services/feature-flags/feature-flags.service';

describe('NotificationsComponent', () => {

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

  class MockDeleteDialog {
    afterClosed() {
      return observableOf('delete');
    }
  }

  class MockMdDialog {
    open(_dialogType) {
      return new MockDeleteDialog();
    }
  }

  class MockMdSnackBar {
    open(_message, _m, _time) { }
  }

  class MockStore {
    dispatch() { }
    select() { }
  }

  let telemetryService: TelemetryService;
  let fixture, component;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule
      ],
      declarations: [
        NotificationsComponent,
        MockComponent({ selector: 'app-settings-sidebar'})
      ],
      providers: [
        { provide: MatDialog, useClass: MockMdDialog },
        { provide: TelemetryService, useClass: MockTelemetryService },
        { provide: RulesService, useClass: MockRulesService },
        { provide: MatSnackBar, useClass: MockMdSnackBar},
        { provide: Store, useClass: MockStore },
        FeatureFlagsService
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    }).compileComponents();

    fixture = TestBed.createComponent(NotificationsComponent);
    component = fixture.componentInstance;
    telemetryService = TestBed.get(TelemetryService);
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
      component.deleteRule(new Rule('', '', null, '', ServiceActionType.SLACK, '', false));

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
