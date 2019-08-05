import { TestBed } from '@angular/core/testing';
import { EventFeedTableComponent } from './event-feed-table.component';
import { MockComponent } from 'ng2-mock-component';
import { CapitalizePipe } from '../../pipes/capitalize.pipe';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { EventFeedService } from '../../services/event-feed/event-feed.service';
import { Observable, of as observableOf } from 'rxjs';
import {
  EventFeedFilter,
  ChefEventCollection
} from '../../types/types';

class MockEventFeedService {
  getEventFeed(_filters: EventFeedFilter): Observable<ChefEventCollection> {
    return observableOf(new ChefEventCollection([], 0));
  }
}

describe('SearchBarComponent', () => {
  let component: EventFeedTableComponent;
  let fixture;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [
        EventFeedTableComponent,
        MockComponent({ selector: 'chef-icon' }),
        MockComponent({ selector: 'chef-button' }),
        MockComponent({ selector: 'app-event-icon', inputs: ['group', 'type', 'task'] }),
        MockComponent({ selector: 'chef-click-outside'}),
        MockComponent({ selector: 'chef-side-panel', inputs: ['visible']}),
        CapitalizePipe
      ],
      providers: [
        { provide: EventFeedService, useClass: MockEventFeedService }
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(EventFeedTableComponent);
    component = fixture.componentInstance;
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
