import { TestBed } from '@angular/core/testing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { MatDialog, MatSnackBar } from '@angular/material';
import { By } from '@angular/platform-browser';
import { Observable, of as observableOf } from 'rxjs';
import { Store } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { Destination } from './destination';
import { DatafeedService } from '../../services/data-feed/data-feed.service';
import { TelemetryService } from '../../services/telemetry/telemetry.service';
import { DatafeedComponent } from './data-feed.component';
import { RouterTestingModule } from '@angular/router/testing';

describe('DatafeedComponent', () => {

  const destinations: Destination[] = [
    new Destination(1, 'name1', 'http://foo.com', 'secret_id1'),
    new Destination(2, 'name2', 'http://bar.com', 'secret_id2'),
    new Destination(3, 'name3', 'http://bar.com', 'secret_id2')
  ];

  // CSS identifiers
  const cardId = '#destinations-cards';
  const listId = '#destinations-list';

  class MockTelemetryService {
    track() { }
  }

  class MockDatafeedService {
    fetchDestinations(): Observable<Destination[]> {
      return observableOf(destinations);
    }

    deleteDestination(destination) {
    return observableOf(destination);
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
        DatafeedComponent,
        MockComponent({ selector: 'app-settings-sidebar'})
      ],
      providers: [
        { provide: MatDialog, useClass: MockMdDialog },
        { provide: TelemetryService, useClass: MockTelemetryService },
        { provide: DatafeedService, useClass: MockDatafeedService },
        { provide: MatSnackBar, useClass: MockMdSnackBar},
        { provide: Store, useClass: MockStore }
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    }).compileComponents();

    fixture = TestBed.createComponent(DatafeedComponent);
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
      const tableBody = getElementByCss(`${listId}`);
      expect(tableBody.children.length).toBe(destinations.length + 1);
    });

  });

  describe('sending telemetry', () => {
    beforeEach(() => {
      spyOn(telemetryService, 'track');
    });

    it('ensure telemetry is sent on deleting a rule', () => {
      component.deleteDestination(new Destination(0, '', '', ''));

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
