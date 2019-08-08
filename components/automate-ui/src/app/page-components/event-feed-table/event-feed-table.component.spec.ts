import { TestBed } from '@angular/core/testing';
import { EventFeedTableComponent } from './event-feed-table.component';
import { MockComponent } from 'ng2-mock-component';
import { CapitalizePipe } from '../../pipes/capitalize.pipe';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { EventFeedService } from '../../services/event-feed/event-feed.service';
import { Observable, of as observableOf } from 'rxjs';
import {
  EventFeedFilter,
  ChefEventCollection,
  ChefEvent
} from '../../types/types';
import * as moment from 'moment';

class MockEventFeedService {
  lastRequestedGetEventFeedFilters: EventFeedFilter;

  getEventFeed(filters: EventFeedFilter): Observable<ChefEventCollection> {
    this.lastRequestedGetEventFeedFilters = filters;
    return observableOf(new ChefEventCollection([], 0));
  }
}

describe('SearchBarComponent', () => {
  let component: EventFeedTableComponent;
  let fixture;
  const mockEventFeedService = new MockEventFeedService();

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
        { provide: EventFeedService, useValue: mockEventFeedService }
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(EventFeedTableComponent);
    component = fixture.componentInstance;
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  describe('getGroupedEvents', () => {

    it('event with search bar filters', () => {
      const event = new ChefEvent({
        event_type: 'node',
        task: 'delete',
        start_time: new Date(1566457200000),
        entity_name: 'test',
        requestor_type: null,
        requestor_name: 'requestor_name',
        service_hostname: 'chef_server',
        parent_name: null,
        parent_type: null,
        event_count: 2,
        end_time: new Date(1566975540000),
        end_id: '570849d0-b96e-11e9-a2a3-2a2ae2dbcce4',
        start_id: '65c3b1da-b96e-11e9-a2a3-2a2ae2dbcce4'
      });

      component.searchBarFilters = [
        {text: 'cookbook', type: 'entity_type'},
        {text: 'scan_job', type: 'entity_type'}
      ];

      component.getGroupedEvents(event);

      expect(mockEventFeedService.lastRequestedGetEventFeedFilters.searchBar).toEqual(
        [{text: 'node', type: 'entity_type'}]);
      expect(mockEventFeedService.lastRequestedGetEventFeedFilters.collapse).toEqual(false);
      expect(mockEventFeedService.lastRequestedGetEventFeedFilters.task).toEqual(event.task);
      expect(mockEventFeedService.lastRequestedGetEventFeedFilters.requestorName).toEqual(
        event.requestorName);
      expect(mockEventFeedService.lastRequestedGetEventFeedFilters.pageSize).toEqual(
        event.eventCount);
      expect(mockEventFeedService.lastRequestedGetEventFeedFilters.endDate).toEqual(
        moment(event.endTime));
      expect(mockEventFeedService.lastRequestedGetEventFeedFilters.startDate).toEqual(
        moment(event.startTime));
    });

    it('no search bar filters', () => {
      const event = new ChefEvent({
        event_type: 'node',
        task: 'delete',
        start_time: new Date(1566457200000),
        entity_name: 'test',
        requestor_type: null,
        requestor_name: 'requestor_name',
        service_hostname: 'chef_server',
        parent_name: null,
        parent_type: null,
        event_count: 2,
        end_time: new Date(1566975540000),
        end_id: '570849d0-b96e-11e9-a2a3-2a2ae2dbcce4',
        start_id: '65c3b1da-b96e-11e9-a2a3-2a2ae2dbcce4'
      });

      component.searchBarFilters = [];

      component.getGroupedEvents(event);

      expect(mockEventFeedService.lastRequestedGetEventFeedFilters.searchBar).toEqual(
        [{text: 'node', type: 'entity_type'}]);
    });

    it('with Chef server and org search bar filters', () => {
      const event = new ChefEvent({
        event_type: 'node',
        task: 'delete',
        start_time: new Date(1566457200000),
        entity_name: 'test',
        requestor_type: null,
        requestor_name: 'requestor_name',
        service_hostname: 'chef_server',
        parent_name: null,
        parent_type: null,
        event_count: 2,
        end_time: new Date(1566975540000),
        end_id: '570849d0-b96e-11e9-a2a3-2a2ae2dbcce4',
        start_id: '65c3b1da-b96e-11e9-a2a3-2a2ae2dbcce4'
      });

      component.searchBarFilters = [
        { type: 'organization', text: 'demo'},
        { type: 'chef_server', text: 'chef.example.com'}
      ];

      component.getGroupedEvents(event);

      const searchbar = mockEventFeedService.lastRequestedGetEventFeedFilters.searchBar;
      expect(searchbar.length).toEqual(3);
      expect(searchbar).toContain({ type: 'organization', text: 'demo'});
      expect(searchbar).toContain({ type: 'chef_server', text: 'chef.example.com'});
      expect(searchbar).toContain({text: 'node', type: 'entity_type'});
    });
  });

  describe('getEventDescription', () => {
    it('node event type', () => {
      const event = new ChefEvent({
        event_type: 'node',
        task: 'delete',
        start_time: new Date(1566457200000),
        entity_name: 'test',
        requestor_type: null,
        requestor_name: 'requestor_name',
        service_hostname: 'chef_server',
        parent_name: null,
        parent_type: null,
        event_count: 2,
        end_time: new Date(1566975540000),
        end_id: '570849d0-b96e-11e9-a2a3-2a2ae2dbcce4',
        start_id: '65c3b1da-b96e-11e9-a2a3-2a2ae2dbcce4'
      });

      const label = component.getEventDescription(event);
      expect(label).toEqual('Nodes <b>test</b> deleted by <b>requestor_name</b>');
    });

    it('cookbook version event type', () => {
      const event = new ChefEvent({
        event_type: 'version',
        task: 'delete',
        start_time: new Date(1566457200000),
        entity_name: '1.2.3',
        requestor_type: null,
        requestor_name: 'requestor_name',
        service_hostname: 'chef_server',
        parent_name: 'nginx',
        parent_type: null,
        event_count: 2,
        end_time: new Date(1566975540000),
        end_id: '570849d0-b96e-11e9-a2a3-2a2ae2dbcce4',
        start_id: '65c3b1da-b96e-11e9-a2a3-2a2ae2dbcce4'
      });

      const label = component.getEventDescription(event);
      expect(label).toEqual(
        'Cookbooks <b>nginx</b>, version <b>1.2.3</b> deleted by <b>requestor_name</b>');
    });
  });

  describe('getEventGroupText', () => {
    it('node event type', () => {
      const event = new ChefEvent({
        event_type: 'node',
        task: 'delete',
        start_time: new Date(1566457200000),
        entity_name: 'test',
        requestor_type: null,
        requestor_name: 'requestor_name',
        service_hostname: 'chef_server',
        parent_name: null,
        parent_type: null,
        event_count: 2,
        end_time: new Date(1566975540000),
        end_id: '570849d0-b96e-11e9-a2a3-2a2ae2dbcce4',
        start_id: '65c3b1da-b96e-11e9-a2a3-2a2ae2dbcce4'
      });

      const label = component.getEventGroupText(event);
      expect(label).toEqual('test');
    });

    it('cookbook version event type', () => {
      const event = new ChefEvent({
        event_type: 'version',
        task: 'delete',
        start_time: new Date(1566457200000),
        entity_name: '1.2.3',
        requestor_type: null,
        requestor_name: 'requestor_name',
        service_hostname: 'chef_server',
        parent_name: 'nginx',
        parent_type: null,
        event_count: 2,
        end_time: new Date(1566975540000),
        end_id: '570849d0-b96e-11e9-a2a3-2a2ae2dbcce4',
        start_id: '65c3b1da-b96e-11e9-a2a3-2a2ae2dbcce4'
      });

      const label = component.getEventGroupText(event);
      expect(label).toEqual('nginx: v1.2.3');
    });
  });

  describe('getFormattedEventType', () => {
    it('delete task type', () => {
      const event = new ChefEvent({
        event_type: 'node',
        task: 'delete',
        start_time: new Date(1566457200000),
        entity_name: 'test',
        requestor_type: null,
        requestor_name: 'requestor_name',
        service_hostname: 'chef_server',
        parent_name: null,
        parent_type: null,
        event_count: 2,
        end_time: new Date(1566975540000),
        end_id: '570849d0-b96e-11e9-a2a3-2a2ae2dbcce4',
        start_id: '65c3b1da-b96e-11e9-a2a3-2a2ae2dbcce4'
      });

      const label = component.getFormattedEventType(event);
      expect(label).toEqual('deleted');
    });

    it('edit task type', () => {
      const event = new ChefEvent({
        event_type: 'node',
        task: 'edit',
        start_time: new Date(1566457200000),
        entity_name: 'test',
        requestor_type: null,
        requestor_name: 'requestor_name',
        service_hostname: 'chef_server',
        parent_name: null,
        parent_type: null,
        event_count: 2,
        end_time: new Date(1566975540000),
        end_id: '570849d0-b96e-11e9-a2a3-2a2ae2dbcce4',
        start_id: '65c3b1da-b96e-11e9-a2a3-2a2ae2dbcce4'
      });

      const label = component.getFormattedEventType(event);
      expect(label).toEqual('edited');
    });

    it('update task type', () => {
      const event = new ChefEvent({
        event_type: 'node',
        task: 'update',
        start_time: new Date(1566457200000),
        entity_name: 'test',
        requestor_type: null,
        requestor_name: 'requestor_name',
        service_hostname: 'chef_server',
        parent_name: null,
        parent_type: null,
        event_count: 2,
        end_time: new Date(1566975540000),
        end_id: '570849d0-b96e-11e9-a2a3-2a2ae2dbcce4',
        start_id: '65c3b1da-b96e-11e9-a2a3-2a2ae2dbcce4'
      });

      const label = component.getFormattedEventType(event);
      expect(label).toEqual('updated');
    });

    it('create task type', () => {
      const event = new ChefEvent({
        event_type: 'node',
        task: 'create',
        start_time: new Date(1566457200000),
        entity_name: 'test',
        requestor_type: null,
        requestor_name: 'requestor_name',
        service_hostname: 'chef_server',
        parent_name: null,
        parent_type: null,
        event_count: 2,
        end_time: new Date(1566975540000),
        end_id: '570849d0-b96e-11e9-a2a3-2a2ae2dbcce4',
        start_id: '65c3b1da-b96e-11e9-a2a3-2a2ae2dbcce4'
      });

      const label = component.getFormattedEventType(event);
      expect(label).toEqual('created');
    });

    it('unknown task type', () => {
      const event = new ChefEvent({
        event_type: 'node',
        task: 'unknown',
        start_time: new Date(1566457200000),
        entity_name: 'test',
        requestor_type: null,
        requestor_name: 'requestor_name',
        service_hostname: 'chef_server',
        parent_name: null,
        parent_type: null,
        event_count: 2,
        end_time: new Date(1566975540000),
        end_id: '570849d0-b96e-11e9-a2a3-2a2ae2dbcce4',
        start_id: '65c3b1da-b96e-11e9-a2a3-2a2ae2dbcce4'
      });

      const label = component.getFormattedEventType(event);
      expect(label).toEqual('unknown');
    });
  });

  describe('getEventTypeLabel', () => {
    it('node event type', () => {
      const event = new ChefEvent({
        event_type: 'node',
        task: 'delete',
        start_time: new Date(1566457200000),
        entity_name: 'test',
        requestor_type: null,
        requestor_name: 'requestor_name',
        service_hostname: 'chef_server',
        parent_name: null,
        parent_type: null,
        event_count: 2,
        end_time: new Date(1566975540000),
        end_id: '570849d0-b96e-11e9-a2a3-2a2ae2dbcce4',
        start_id: '65c3b1da-b96e-11e9-a2a3-2a2ae2dbcce4'
      });

      const label = component.getEventTypeLabel(event);
      expect(label).toEqual('nodes');
    });

    it('multiple key events', () => {
      const event = new ChefEvent({
        event_type: 'key',
        task: 'delete',
        start_time: new Date(1566457200000),
        entity_name: 'test',
        requestor_type: null,
        requestor_name: 'requestor_name',
        service_hostname: 'chef_server',
        parent_name: null,
        parent_type: null,
        event_count: 2,
        end_time: new Date(1566975540000),
        end_id: '570849d0-b96e-11e9-a2a3-2a2ae2dbcce4',
        start_id: '65c3b1da-b96e-11e9-a2a3-2a2ae2dbcce4'
      });

      const label = component.getEventTypeLabel(event);
      expect(label).toEqual('keys');
    });

    it('multiple berry events', () => {
      const event = new ChefEvent({
        event_type: 'berry',
        task: 'delete',
        start_time: new Date(1566457200000),
        entity_name: 'test',
        requestor_type: null,
        requestor_name: 'requestor_name',
        service_hostname: 'chef_server',
        parent_name: null,
        parent_type: null,
        event_count: 2,
        end_time: new Date(1566975540000),
        end_id: '570849d0-b96e-11e9-a2a3-2a2ae2dbcce4',
        start_id: '65c3b1da-b96e-11e9-a2a3-2a2ae2dbcce4'
      });

      const label = component.getEventTypeLabel(event);
      expect(label).toEqual('berries');
    });

    it('single node event type', () => {
      const event = new ChefEvent({
        event_type: 'node',
        task: 'delete',
        start_time: new Date(1566457200000),
        entity_name: 'test',
        requestor_type: null,
        requestor_name: 'requestor_name',
        service_hostname: 'chef_server',
        parent_name: null,
        parent_type: null,
        event_count: 1,
        end_time: new Date(1566975540000),
        end_id: '570849d0-b96e-11e9-a2a3-2a2ae2dbcce4',
        start_id: '65c3b1da-b96e-11e9-a2a3-2a2ae2dbcce4'
      });

      const label = component.getEventTypeLabel(event);
      expect(label).toEqual('node');
    });

    it('cookbook version event type', () => {
      const event = new ChefEvent({
        event_type: 'version',
        task: 'delete',
        start_time: new Date(1566457200000),
        entity_name: 'test',
        requestor_type: null,
        requestor_name: 'requestor_name',
        service_hostname: 'chef_server',
        parent_name: null,
        parent_type: null,
        event_count: 2,
        end_time: new Date(1566975540000),
        end_id: '570849d0-b96e-11e9-a2a3-2a2ae2dbcce4',
        start_id: '65c3b1da-b96e-11e9-a2a3-2a2ae2dbcce4'
      });

      const label = component.getEventTypeLabel(event);
      expect(label).toEqual('cookbooks');
    });

    it('item event type', () => {
      const event = new ChefEvent({
        event_type: 'item',
        task: 'delete',
        start_time: new Date(1566457200000),
        entity_name: 'test',
        requestor_type: null,
        requestor_name: 'requestor_name',
        service_hostname: 'chef_server',
        parent_name: null,
        parent_type: null,
        event_count: 2,
        end_time: new Date(1566975540000),
        end_id: '570849d0-b96e-11e9-a2a3-2a2ae2dbcce4',
        start_id: '65c3b1da-b96e-11e9-a2a3-2a2ae2dbcce4'
      });

      const label = component.getEventTypeLabel(event);
      expect(label).toEqual('data bag items');
    });

    it('bag event type', () => {
      const event = new ChefEvent({
        event_type: 'bag',
        task: 'delete',
        start_time: new Date(1566457200000),
        entity_name: 'test',
        requestor_type: null,
        requestor_name: 'requestor_name',
        service_hostname: 'chef_server',
        parent_name: null,
        parent_type: null,
        event_count: 2,
        end_time: new Date(1566975540000),
        end_id: '570849d0-b96e-11e9-a2a3-2a2ae2dbcce4',
        start_id: '65c3b1da-b96e-11e9-a2a3-2a2ae2dbcce4'
      });

      const label = component.getEventTypeLabel(event);
      expect(label).toEqual('data bags');
    });

    it('scanjobs event type', () => {
      const event = new ChefEvent({
        event_type: 'scanjobs',
        task: 'delete',
        start_time: new Date(1566457200000),
        entity_name: 'test',
        requestor_type: null,
        requestor_name: 'requestor_name',
        service_hostname: 'chef_server',
        parent_name: null,
        parent_type: null,
        event_count: 2,
        end_time: new Date(1566975540000),
        end_id: '570849d0-b96e-11e9-a2a3-2a2ae2dbcce4',
        start_id: '65c3b1da-b96e-11e9-a2a3-2a2ae2dbcce4'
      });

      const label = component.getEventTypeLabel(event);
      expect(label).toEqual('scan jobs');
    });
  });
});
