import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import {
  RespEventCounts,
  EventTypeCount,
  GuitarStringItem,
  EventFeedFilter,
  Chicklet,
  ChefEvent
} from '../../types/types';

import { EventFeedService } from './event-feed.service';
import * as moment from 'moment';
import { initialState } from './event-feed.reducer';

describe('EventFeedService', () => {
  let service: EventFeedService;
  let oneWeekAgo, today: moment.Moment;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpClientTestingModule
      ],
      providers: [
        EventFeedService
      ]
    });

    service = TestBed.get(EventFeedService);

    today = moment().endOf('day');
    oneWeekAgo = moment().subtract(6, 'days').startOf('day');
  });

  describe('buildEventFeedURLSearchParams', () => {
    it('full filter first page', () => {
      const start = moment('20190822-0000', 'YYYYMMDD-HHmm');
      const end = moment('20190827-2359', 'YYYYMMDD-HHmm');
      const filters: EventFeedFilter = {
        searchBar: [
          {text: 'node', type: 'entity_type'},
          {text: 'scan_job', type: 'entity_type'}
        ],
        hoursBetween: 4,
        collapse: false,
        task: 'delete',
        pageSize: 77,
        startDate: start,
        endDate: end
      };

      const httpParams = service.buildEventFeedURLSearchParams(filters, null);

      const httpFilters = httpParams.getAll('filter');
      expect(httpFilters.length).toEqual(3);
      expect(httpFilters).toContain('entity_type:scan_job');
      expect(httpFilters).toContain('entity_type:node');
      expect(httpFilters).toContain('task:delete');

      expect(httpParams.get('page_size')).toEqual('77');
      expect(httpParams.get('collapse')).toEqual('false');
      expect(httpParams.get('hours_between')).toBeNull();
      expect(httpParams.get('before')).toBeNull();
      expect(httpParams.get('cursor')).toBeNull();
      expect(httpParams.get('start')).toEqual(start.valueOf().toString());
      expect(httpParams.get('end')).toEqual(end.valueOf().toString());
    });

    it('full filter second page', () => {
      const filters: EventFeedFilter = {
        searchBar: [
          {text: 'node', type: 'entity_type'},
          {text: 'scan_job', type: 'entity_type'}
        ],
        hoursBetween: 4,
        collapse: false,
        task: 'delete',
        pageSize: 77,
        startDate: moment('20190822-0000', 'YYYYMMDD-HHmm'),
        endDate: moment('20190827-2359', 'YYYYMMDD-HHmm')
      };

      const event = new ChefEvent({
        event_type: 'node',
        task: 'delete',
        start_time: new Date(1566457200000),
        entity_name: 'test',
        requestor_type: null,
        requestor_name: null,
        service_hostname: 'chef_server',
        parent_name: null,
        parent_type: null,
        event_count: 1,
        end_time: new Date(1566975540000),
        end_id: '570849d0-b96e-11e9-a2a3-2a2ae2dbcce4',
        start_id: '65c3b1da-b96e-11e9-a2a3-2a2ae2dbcce4'
      });

      const httpParams = service.buildEventFeedURLSearchParams(filters, event);

      expect(httpParams.get('before')).toEqual('1566975540000');
      expect(httpParams.get('cursor')).toEqual('570849d0-b96e-11e9-a2a3-2a2ae2dbcce4');
    });

    it('collapse not set', () => {
      const filters: EventFeedFilter = {
        searchBar: [
          {text: 'node', type: 'entity_type'},
          {text: 'scan_job', type: 'entity_type'}
        ],
        hoursBetween: 4,
        task: 'delete',
        pageSize: 77,
        startDate: moment('20190822-0000', 'YYYYMMDD-HHmm'),
        endDate: moment('20190827-2359', 'YYYYMMDD-HHmm')
      };

      const httpParams = service.buildEventFeedURLSearchParams(filters, null);

      expect(httpParams.get('collapse')).toEqual('true');
    });

    it('page not set', () => {
      const filters: EventFeedFilter = {
        searchBar: [
          {text: 'node', type: 'entity_type'},
          {text: 'scan_job', type: 'entity_type'}
        ],
        hoursBetween: 4,
        collapse: false,
        task: 'delete',
        startDate: moment('20190822-0000', 'YYYYMMDD-HHmm'),
        endDate: moment('20190827-2359', 'YYYYMMDD-HHmm')
      };

      const httpParams = service.buildEventFeedURLSearchParams(filters, null);

      expect(httpParams.get('page_size')).toEqual('100');
    });
  });

  describe('buildEventStringsURLSearchParams', () => {
    it('with search bar selections, start date, and end date', () => {
      const start = moment('20190822-0000', 'YYYYMMDD-HHmm');
      const end = moment('20190827-2359', 'YYYYMMDD-HHmm');
      const filters: EventFeedFilter = {
        searchBar: [
          {text: 'node', type: 'entity_type'},
          {text: 'scan_job', type: 'entity_type'}
        ],
        hoursBetween: 4,
        startDate: start,
        endDate: end
      };

      const httpParams = service.buildEventStringsURLSearchParams(filters);

      expect(httpParams.get('timezone')).toEqual(Intl.DateTimeFormat().resolvedOptions().timeZone);

      const httpFilters = httpParams.getAll('filter');
      expect(httpFilters.length).toEqual(2);
      expect(httpFilters).toContain('entity_type:scan_job');
      expect(httpFilters).toContain('entity_type:node');

      expect(httpParams.get('hours_between')).toEqual('4');

      expect(httpParams.get('start')).toEqual(start.format('YYYY-MM-DD'));
      expect(httpParams.get('end')).toEqual(end.format('YYYY-MM-DD'));
    });

    it('no start date', () => {
      const filters: EventFeedFilter = {
        searchBar: [
          {text: 'node', type: 'entity_type'},
          {text: 'scan_job', type: 'entity_type'}
        ],
        hoursBetween: 4,
        endDate: moment('20190827-2359', 'YYYYMMDD-HHmm')
      };

      const httpParams = service.buildEventStringsURLSearchParams(filters);

      expect(httpParams.get('start')).toEqual(initialState.filters.startDate.format('YYYY-MM-DD'));
    });

    it('no end date', () => {
      const filters: EventFeedFilter = {
        searchBar: [
          {text: 'node', type: 'entity_type'},
          {text: 'scan_job', type: 'entity_type'}
        ],
        hoursBetween: 4,
        startDate: moment('20190822-0000', 'YYYYMMDD-HHmm')
      };

      const httpParams = service.buildEventStringsURLSearchParams(filters);

      expect(httpParams.get('end')).toEqual(moment().format('YYYY-MM-DD'));
    });

    it('no search bar selections', () => {
      const filters: EventFeedFilter = {
        startDate: moment('20190822-0000', 'YYYYMMDD-HHmm'),
        endDate: moment('20190827-2359', 'YYYYMMDD-HHmm'),
        hoursBetween: 4
      };

      const httpParams = service.buildEventStringsURLSearchParams(filters);
      expect(httpParams.getAll('filter')).toBeNull();
    });

    it('one search bar filter', () => {
      const filters: EventFeedFilter = {
        searchBar: [
          {text: 'node', type: 'entity_type'}
        ],
        startDate: moment('20190822-0000', 'YYYYMMDD-HHmm'),
        endDate: moment('20190827-2359', 'YYYYMMDD-HHmm'),
        hoursBetween: 4
      };

      const httpParams = service.buildEventStringsURLSearchParams(filters);

      const httpFilters = httpParams.getAll('filter');

      expect(httpFilters.length).toEqual(1);
      expect(httpFilters).toContain('entity_type:node');
    });

    it('no hours between filter', () => {
      const filters: EventFeedFilter = {
        searchBar: [
          {text: 'node', type: 'entity_type'}
        ],
        startDate: moment('20190822-0000', 'YYYYMMDD-HHmm'),
        endDate: moment('20190827-2359', 'YYYYMMDD-HHmm')
      };

      const httpParams = service.buildEventStringsURLSearchParams(filters);

      expect(httpParams.get('hours_between')).toEqual('1');
    });
  });

  describe('buildEventCountsURLSearchParams', () => {
    it('with search bar selections, start date, and end date', () => {
      const start = moment('20190822-0000', 'YYYYMMDD-HHmm');
      const end = moment('20190827-2359', 'YYYYMMDD-HHmm');
      const filters: EventFeedFilter = {
        searchBar: [
          {text: 'node', type: 'entity_type'},
          {text: 'scan_job', type: 'entity_type'}
        ],
        startDate: start,
        endDate: end
      };

      const httpParams = service.buildEventCountsURLSearchParams(filters);

      const httpFilters = httpParams.getAll('filter');

      expect(httpFilters.length).toEqual(2);
      expect(httpFilters).toContain('entity_type:scan_job');
      expect(httpFilters).toContain('entity_type:node');
      expect(httpParams.get('start')).toEqual(start.valueOf().toString());
      expect(httpParams.get('end')).toEqual(end.valueOf().toString());
    });

    it('no start date', () => {
      const filters: EventFeedFilter = {
        searchBar: [
          {text: 'node', type: 'entity_type'},
          {text: 'scan_job', type: 'entity_type'}
        ],
        endDate: moment('20190827-2359', 'YYYYMMDD-HHmm').utc()
      };

      const httpParams = service.buildEventCountsURLSearchParams(filters);

      expect(httpParams.get('start')).toEqual(initialState.filters.startDate.valueOf().toString());
    });

    it('no end date', () => {
      const filters: EventFeedFilter = {
        searchBar: [
          {text: 'node', type: 'entity_type'},
          {text: 'scan_job', type: 'entity_type'}
        ],
        startDate: moment('20190822-0000', 'YYYYMMDD-HHmm').utc()
      };

      const httpParams = service.buildEventCountsURLSearchParams(filters);

      expect(httpParams.get('end')).toBeNull();
    });

    it('no search bar selections', () => {
      const filters: EventFeedFilter = {
        startDate: moment('20190822-0000', 'YYYYMMDD-HHmm').utc(),
        endDate: moment('20190827-2359', 'YYYYMMDD-HHmm').utc()
      };

      const httpParams = service.buildEventCountsURLSearchParams(filters);

      expect(httpParams.getAll('filter')).toBeNull();
    });

    it('one search bar filter', () => {
      const filters: EventFeedFilter = {
        searchBar: [
          {text: 'node', type: 'entity_type'}
        ],
        startDate: moment('20190822-0000', 'YYYYMMDD-HHmm').utc(),
        endDate: moment('20190827-2359', 'YYYYMMDD-HHmm').utc()
      };

      const httpParams = service.buildEventCountsURLSearchParams(filters);

      const httpFilters = httpParams.getAll('filter');

      expect(httpFilters.length).toEqual(1);
      expect(httpFilters).toContain('entity_type:node');
    });
  });

  describe('addChildEventTypes', () => {
    it('no extra children filters', () => {
      const filters: Chicklet[] = [
        {text: 'node', type: 'entity_type'},
        {text: 'scan_job', type: 'entity_type'}
      ];

      const filtersWithChildren = service.addChildEventTypes(filters);

      expect(filters).toEqual(filtersWithChildren);
    });

    it('cookbook children filters added', () => {
      const filters: Chicklet[] = [
        {text: 'cookbook', type: 'entity_type'}
      ];

      const filtersWithChildren = service.addChildEventTypes(filters);

      expect(filtersWithChildren.length).toEqual(2);
      expect(filtersWithChildren).toContain({text: 'cookbook', type: 'entity_type'});
      expect(filtersWithChildren).toContain({text: 'version', type: 'entity_type'});
    });

    it('data bag children filters added', () => {
      const filters: Chicklet[] = [
        {text: 'bag', type: 'entity_type'}
      ];

      const filtersWithChildren = service.addChildEventTypes(filters);

      expect(filtersWithChildren.length).toEqual(2);
      expect(filtersWithChildren).toContain({text: 'bag', type: 'entity_type'});
      expect(filtersWithChildren).toContain({text: 'item', type: 'entity_type'});
    });

    it('data bag children filters added with filter without children', () => {
      const filters: Chicklet[] = [
        {text: 'node', type: 'entity_type'},
        {text: 'bag', type: 'entity_type'}
      ];

      const filtersWithChildren = service.addChildEventTypes(filters);

      expect(filtersWithChildren.length).toEqual(3);
      expect(filtersWithChildren).toContain({text: 'bag', type: 'entity_type'});
      expect(filtersWithChildren).toContain({text: 'item', type: 'entity_type'});
      expect(filtersWithChildren).toContain({text: 'node', type: 'entity_type'});
    });

    it('empty filters', () => {
      const filters: Chicklet[] = [];

      const filtersWithChildren = service.addChildEventTypes(filters);

      expect(filtersWithChildren.length).toEqual(0);
    });
  });

  describe('creating EventTypeCount object', () => {
    it('normal response from backend', () => {
      const response: RespEventCounts = {
        'total': 40,
        'counts': [
          {'name': 'node', 'count': 5},
          {'name': 'policyfile', 'count': 4},
          {'name': 'cookbook', 'count': 9},
          {'name': 'environment', 'count': 9},
          {'name': 'role', 'count': 7},
          {'name': 'bag', 'count': 6}
        ]
      };

      const eventTypeCount = new EventTypeCount(response);

      expect(eventTypeCount.total).toEqual(40);
      expect(eventTypeCount.cookbook).toEqual(9);
      expect(eventTypeCount.bag).toEqual(6);
      expect(eventTypeCount.environment).toEqual(9);
      expect(eventTypeCount.node).toEqual(5);
      expect(eventTypeCount.policyfile).toEqual(4);
      expect(eventTypeCount.profile).toEqual(0);
      expect(eventTypeCount.scanjobs).toEqual(0);
      expect(eventTypeCount.role).toEqual(7);
      expect(eventTypeCount.client).toEqual(0);
    });

    it('empty response from backend', () => {
      const response: RespEventCounts = {
        'total': 0,
        'counts': []
      };

      const eventTypeCount = new EventTypeCount(response);

      expect(eventTypeCount.total).toEqual(0);
      expect(eventTypeCount.cookbook).toEqual(0);
      expect(eventTypeCount.bag).toEqual(0);
      expect(eventTypeCount.environment).toEqual(0);
      expect(eventTypeCount.node).toEqual(0);
      expect(eventTypeCount.policyfile).toEqual(0);
      expect(eventTypeCount.profile).toEqual(0);
      expect(eventTypeCount.scanjobs).toEqual(0);
      expect(eventTypeCount.role).toEqual(0);
      expect(eventTypeCount.client).toEqual(0);
    });

    it('extra count in response from backend', () => {
      const response: RespEventCounts = {
        'total': 40,
        'counts': [
          {'name': 'node', 'count': 5},
          {'name': 'policyfile', 'count': 4},
          {'name': 'cookbook', 'count': 9},
          {'name': 'environment', 'count': 8},
          {'name': 'role', 'count': 7},
          {'name': 'bag', 'count': 6},
          {'name': 'fake', 'count': 1}
        ]
      };

      const eventTypeCount = new EventTypeCount(response);

      expect(eventTypeCount.total).toEqual(40);
      expect(eventTypeCount.cookbook).toEqual(9);
      expect(eventTypeCount.bag).toEqual(6);
      expect(eventTypeCount.environment).toEqual(8);
      expect(eventTypeCount.node).toEqual(5);
      expect(eventTypeCount.policyfile).toEqual(4);
      expect(eventTypeCount.profile).toEqual(0);
      expect(eventTypeCount.scanjobs).toEqual(0);
      expect(eventTypeCount.role).toEqual(7);
      expect(eventTypeCount.client).toEqual(0);
    });
  });

  describe('convertResponseToGuitarStringCollection()', () => {

    it('Missing start date', () => {
      const backendData = {
        end: today,
        hours_between: 12,
        strings: [
          {
            event_action: 'create',
            collection: [
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              }
            ]
          },
          {
            event_action: 'delete',
            collection: [
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              }
            ]
          },
          {
            event_action: 'update',
            collection: [
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              }
            ]
          }
        ]
      };

      expect( function() { service.convertResponseToGuitarStringCollection(backendData); }
        ).toThrow();
    });

    it('Missing end date', () => {
      const backendData = {
        start: today,
        end: oneWeekAgo,
        hours_between: 12,
        strings: [
          {
            event_action: 'create',
            collection: [
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              }
            ]
          },
          {
            event_action: 'delete',
            collection: [
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              }
            ]
          },
          {
            event_action: 'update',
            collection: [
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              }
            ]
          }
        ]
      };

      expect( function() { service.convertResponseToGuitarStringCollection(backendData); }
        ).toThrow();
    });

    it('hours_between is negative', () => {
      const backendData = {
        start: oneWeekAgo,
        end: today,
        hours_between: -12,
        strings: [
          {
            event_action: 'create',
            collection: [
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              }
            ]
          },
          {
            event_action: 'delete',
            collection: [
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              }
            ]
          },
          {
            event_action: 'update',
            collection: [
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              }
            ]
          }
        ]
      };

      expect( function() { service.convertResponseToGuitarStringCollection(backendData); }
        ).toThrow();
    });

    it('end date is before start date', () => {
      const backendData = {
        start: oneWeekAgo,
        hours_between: 12,
        strings: [
          {
            event_action: 'create',
            collection: [
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              }
            ]
          },
          {
            event_action: 'delete',
            collection: [
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              }
            ]
          },
          {
            event_action: 'update',
            collection: [
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              }
            ]
          }
        ]
      };

      expect( function() { service.convertResponseToGuitarStringCollection(backendData); }
        ).toThrow();
    });

    it('Missing hours between variable', () => {
      const backendData = {
        start: oneWeekAgo,
        end: today,
        strings: [
          {
            event_action: 'create',
            collection: [
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              }
            ]
          },
          {
            event_action: 'delete',
            collection: [
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              }
            ]
          },
          {
            event_action: 'update',
            collection: [
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              }
            ]
          }
        ]
      };

      expect( function() { service.convertResponseToGuitarStringCollection(backendData); }
        ).toThrow();
    });

    it('empty collections should create empty collections', () => {

      const backendData = {
        start: oneWeekAgo,
        end: today,
        hours_between: 3,
        strings: [
          {
            event_action: 'create',
            collection: []
          },
          {
            event_action: 'delete',
            collection: []
          },
          {
            event_action: 'update',
            collection: []
          }
        ]
      };

      const guitarStringCollection = service.convertResponseToGuitarStringCollection(backendData);

      expect(guitarStringCollection.start).toEqual(oneWeekAgo);
      expect(guitarStringCollection.strings.length).toEqual(3);

      expect(guitarStringCollection.strings[0].items.length).toEqual(56);
      guitarStringCollection.strings[0].items.forEach((item: GuitarStringItem) => {
        expect(item.isEmpty()).toEqual(true);
      });

      expect(guitarStringCollection.strings[1].items.length).toEqual(56);
      guitarStringCollection.strings[1].items.forEach((item: GuitarStringItem) => {
        expect(item.isEmpty()).toEqual(true);
      });
      expect(guitarStringCollection.strings[2].items.length).toEqual(56);
      guitarStringCollection.strings[2].items.forEach((item: GuitarStringItem) => {
        expect(item.isEmpty()).toEqual(true);
      });

      expect(guitarStringCollection.strings[0].eventAction).toEqual('create');
      expect(guitarStringCollection.strings[1].eventAction).toEqual('delete');
      expect(guitarStringCollection.strings[2].eventAction).toEqual('update');
    });

    it('out of order event action, should be placed in order', () => {
      const backendData = {
        start: oneWeekAgo,
        end: today,
        hours_between: 3,
        strings: [
          {
            event_action: 'update',
            collection: []
          },
          {
            event_action: 'delete',
            collection: []
          },
          {
            event_action: 'create',
            collection: []
          }
        ]
      };

      const guitarStringCollection = service.convertResponseToGuitarStringCollection(backendData);

      expect(guitarStringCollection.start).toEqual(oneWeekAgo);
      expect(guitarStringCollection.strings.length).toEqual(3);

      expect(guitarStringCollection.strings[0].eventAction).toEqual('create');
      expect(guitarStringCollection.strings[1].eventAction).toEqual('delete');
      expect(guitarStringCollection.strings[2].eventAction).toEqual('update');
    });

    it('original data should be in same order in final output', () => {
      const backendData = {
        start: oneWeekAgo,
        end: today,
        hours_between: 3,
        strings: [
          {
            event_action: 'create',
            collection: [
              {
                events_count: [
                  {
                    name: 'cookbook',
                    count: 1
                  }
                ]
              },
              {
                events_count: []
              },
              {
                events_count: [
                  {
                    name: 'bag',
                    count: 1
                  }
                ]
              },
              {
                events_count: []
              },
              {
                events_count: [
                  {
                    name: 'node',
                    count: 1
                  }
                ]
              }
            ]
          }
        ]
      };

      const guitarStringCollection = service.convertResponseToGuitarStringCollection(backendData);

      const items = guitarStringCollection.strings[0].items;
      expect(items[0].eventTypeCount[0].name).toEqual('cookbook');
      expect(items[1].eventTypeCount).toEqual([]);
      expect(items[2].eventTypeCount[0].name).toEqual('bag');
      expect(items[3].eventTypeCount).toEqual([]);
      expect(items[4].eventTypeCount[0].name).toEqual('node');
    });

    it('GuitarStringItem should have the correct dates', () => {
      const hoursBetween = 3;
      const backendData = {
        start: oneWeekAgo,
        end: today,
        hours_between: hoursBetween,
        strings: [
          {
            event_action: 'create',
            collection: [
              {
                events_count: [
                  {
                    name: 'cookbook',
                    count: 1
                  }
                ]
              },
              {
                events_count: [
                  {
                    name: 'item',
                    count: 1
                  }
                ]
              },
              {
                events_count: [
                  {
                    name: 'bag',
                    count: 1
                  }
                ]
              },
              {
                events_count: []
              },
              {
                events_count: [
                  {
                    name: 'node',
                    count: 1
                  }
                ]
              }
            ]
          }
        ]
      };

      const guitarStringCollection = service.convertResponseToGuitarStringCollection(backendData);

      const items = guitarStringCollection.strings[0].items;
      expect(items[0].start).toEqual(oneWeekAgo);
      expect(items[1].start).toEqual(oneWeekAgo.clone().
        add(hoursBetween, 'hours').startOf('hour'));
      expect(items[2].start).toEqual(oneWeekAgo.clone().
        add(hoursBetween * 2, 'hours').startOf('hour'));
      expect(items[4].start).toEqual(oneWeekAgo.clone().
        add(hoursBetween * 4, 'hours').startOf('hour'));
    });

    it('When there is not enough items in a string the data is filled in', () => {
      const oneDayAgo = moment().subtract(1, 'days').startOf('day');
      // update only has 3 items and should have 4.
      const backendData = {
        start: oneDayAgo,
        end: today,
        hours_between: 12,
        strings: [
          {
            event_action: 'create',
            collection: [
              {
                events_count: [
                  {
                    name: 'cookbook',
                    count: 1
                  }
                ]
              },
              {
                events_count: []
              },
              {
                events_count: [
                  {
                    name: 'bag',
                    count: 1
                  }
                ]
              },
              {
                events_count: []
              }
            ]
          },
          {
            event_action: 'delete',
            collection: [
              {
                events_count: [
                  {
                    name: 'cookbook',
                    count: 1
                  }
                ]
              },
              {
                events_count: []
              },
              {
                events_count: [
                  {
                    name: 'bag',
                    count: 1
                  }
                ]
              },
              {
                events_count: []
              }
            ]
          },
          {
            event_action: 'update',
            collection: [
              {
                events_count: [
                  {
                    name: 'cookbook',
                    count: 1
                  }
                ]
              },
              {
                events_count: []
              },
              {
                events_count: [
                  {
                    name: 'bag',
                    count: 1
                  }
                ]
              }
            ]
          }
        ]
      };

      const guitarStringCollection = service.convertResponseToGuitarStringCollection(backendData);

      expect(guitarStringCollection.strings[0].items.length).toEqual(4);

      expect(guitarStringCollection.strings[1].items.length).toEqual(4);

      expect(guitarStringCollection.strings[2].items.length).toEqual(4);

      expect(guitarStringCollection.strings[2].items[0].getEventType()).toEqual('cookbook');
      expect(guitarStringCollection.strings[2].items[1].isEmpty()).toEqual(true);
      expect(guitarStringCollection.strings[2].items[2].getEventType()).toEqual('bag');
      expect(guitarStringCollection.strings[2].items[3].isEmpty()).toEqual(true);
    });

    it('When there are extra items in a string the data is truncated', () => {
      const oneDayAgo = moment().subtract(1, 'days').startOf('day');
      // update only has 3 items and should have 4.
      const backendData = {
        start: oneDayAgo,
        end: today,
        hours_between: 12,
        strings: [
          {
            event_action: 'create',
            collection: [
              {
                events_count: [
                  {
                    name: 'cookbook',
                    count: 1
                  }
                ]
              },
              {
                events_count: []
              },
              {
                events_count: [
                  {
                    name: 'bag',
                    count: 1
                  }
                ]
              },
              {
                events_count: []
              }
            ]
          },
          {
            event_action: 'delete',
            collection: [
              {
                events_count: [
                  {
                    name: 'cookbook',
                    count: 1
                  }
                ]
              },
              {
                events_count: []
              },
              {
                events_count: [
                  {
                    name: 'bag',
                    count: 1
                  }
                ]
              },
              {
                events_count: []
              },
              {
                events_count: []
              },
              {
                events_count: []
              }
            ]
          },
          {
            event_action: 'update',
            collection: [
              {
                events_count: [
                  {
                    name: 'cookbook',
                    count: 1
                  }
                ]
              },
              {
                events_count: []
              },
              {
                events_count: [
                  {
                    name: 'bag',
                    count: 1
                  }
                ]
              },
              {
                events_count: []
              }
            ]
          }
        ]
      };

      const guitarStringCollection = service.convertResponseToGuitarStringCollection(backendData);

      expect(guitarStringCollection.strings[0].items.length).toEqual(4);

      expect(guitarStringCollection.strings[1].items.length).toEqual(4);

      expect(guitarStringCollection.strings[2].items.length).toEqual(4);
    });
  });
});
