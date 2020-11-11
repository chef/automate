import { CUSTOM_ELEMENTS_SCHEMA, DebugElement } from '@angular/core';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { Router, ActivatedRoute } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { CookieModule } from 'ngx-cookie';
import { Observable, of as observableOf } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { Store, StoreModule } from '@ngrx/store';
import { NgrxStateAtom, runtimeChecks, ngrxReducers } from 'app/ngrx.reducers';
import { ReportingComponent } from '../+reporting/reporting.component';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import {
  StatsService,
  SuggestionsService,
  ReportQueryService,
  ReportDataService,
  ReportQuery,
  ReportingSummary
} from '../shared/reporting';
import { TelemetryService } from '../../../services/telemetry/telemetry.service';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import * as moment from 'moment/moment';
import { DatetimePipe } from 'app/pipes/datetime.pipe';
import { using } from 'app/testing/spec-helpers';

class MockTelemetryService {
  track() { }
}
describe('ReportingComponent', () => {
  let store: Store<NgrxStateAtom>;
  let fixture: ComponentFixture<ReportingComponent>,
    component: ReportingComponent,
    element: DebugElement;
  let statsService: StatsService;
  let suggestionsService: SuggestionsService;
  let reportQueryService: ReportQueryService;
  let router: Router;
  let route: ActivatedRoute;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        CookieModule.forRoot(),
        HttpClientTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      declarations: [
        ReportingComponent,
        DatetimePipe
      ],
      providers: [
        { provide: TelemetryService, useClass: MockTelemetryService },
        ChefSessionService,
        StatsService,
        SuggestionsService,
        ReportQueryService,
        ReportDataService,
        FeatureFlagsService
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });
    store = TestBed.inject(Store);
    spyOn(store, 'dispatch').and.callThrough();
    fixture = TestBed.createComponent(ReportingComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
    statsService = element.injector.get(StatsService);
    suggestionsService = element.injector.get(SuggestionsService);
    reportQueryService = element.injector.get(ReportQueryService);
    router = TestBed.inject(Router);
    route = TestBed.inject(ActivatedRoute);
  });

  it('displays tabs', () => {
    const qe = element.query(By.css('.nav-tabs-list'));
    expect(qe).not.toBeNull();
  });

  using([
    ['01', '01', '23', 'an hour before end of day']
    // ['04', '22', '00', 'exactly end of day'],
    // ['06', '30', '01', 'an hour after end of day']
  ], function (month: string, day: string, hour: string, description: string) {
    xit(`displays the date correctly in UTC timestandard for ${description}`, () => {
      const originalDate = moment.utc(`2019-${month}-${day}-${hour}00`, 'YYYY-MM-DDZ');
      const start = moment.utc(originalDate).subtract(6, 'days');

      component.reportQuery.setState({
        startDate: start,
        endDate: originalDate,
        interval: 0,
        filters: [],
        last24h: false
      } as ReportQuery);
      fixture.detectChanges();

      component.endDate$.subscribe(d => {
        expect(originalDate.month()).toEqual(d.getUTCMonth());
        expect(originalDate.date()).toEqual(d.getUTCDate());
      });
    });
  });

  it('extracting end time from the URL', () => {
    const expectedDate = moment('20191023GMT+00:00', 'YYYYMMDDZ').startOf('day').add(12, 'hours');
    const beforeEndDate = component.getEndDate(
      [{type: 'end_time', text: '2019-10-23', type_key: ''}]
    );
    expect(expectedDate.hours()).toEqual(beforeEndDate.hours());
  });

  describe('sets availableFilterTypes', () => {
    it('to include profile, platform, environment, node, and control', () => {
      const availableFilterTypesNames = component.availableFilterTypes.map( type => type.name);

      const expected = [
        'chef_server', 'chef_tags', 'control', 'control_tag_key',
        'environment', 'node', 'organization', 'policy_group',
        'policy_name', 'recipe', 'role', 'inspec_version', 'profile_with_version',
        'platform_with_version'];

      expect(expected.sort()).toEqual(availableFilterTypesNames.sort());
    });

    it('to have the titles sorted', () => {
      // Ensure availableFilterTypes.Titles are sorted
      const availableFilterTypesTitles = component.availableFilterTypes.map( type => type.title);
      const expectedAvailableFilterTypesTitle =
        component.availableFilterTypes.map(type => type.title).sort();

      expect(expectedAvailableFilterTypesTitle).toEqual(availableFilterTypesTitles,
        'Filter Types are not sorted');
    });
  });

  describe('ngOnDestroy()', () => {
    it('notifies subscriptions to unsubscribe', () => {
      const source = new Observable(() => {});
      const subscription = source.pipe(
          takeUntil((component as any).isDestroyed))
        .subscribe(() => {});

      spyOn(subscription, 'unsubscribe');
      component.ngOnDestroy();

      expect(subscription.unsubscribe).toHaveBeenCalled();
    });
  });

  describe('onEndDateChanged', () => {
    it('specific string date', () => {
      spyOn(router, 'navigate');
      const event = {detail: '2017-10-23'};
      component.onEndDateChanged(event);
      expect(router.navigate).toHaveBeenCalledWith([], {queryParams: { end_time: '2017-10-23'}});
    });

    it('specific date object', () => {
      spyOn(router, 'navigate');
      const event = {detail: new Date('2017-10-23')};
      component.onEndDateChanged(event);
      expect(router.navigate).toHaveBeenCalledWith([], {queryParams: { end_time: '2017-10-23'}});
    });
  });

  describe('onLast24Selected', () => {
    it('no date set when last 24 hours is selected', () => {
      spyOn(router, 'navigate');
      component.onLast24Selected();
      expect(router.navigate).toHaveBeenCalledWith([], {queryParams: {}});
    });
  });

  describe('getSuggestions()', () => {
    describe('when the item has a version', () => {
      it('sets the title to text, version values to display to the user', () => {
        const type = 'profile_with_version';
        const text = 'dev';
        const reportQuery: ReportQuery = {
          endDate: moment(0).utc().startOf('day'),
          startDate: moment(0).utc().startOf('day'),
          interval: 0,
          filters: [],
          last24h: false
        };
        spyOn(suggestionsService, 'getSuggestions').and.returnValue(observableOf([
          {text: 'dev sec baseline, v2.0', version: '2.0'}
        ]));

        component.getSuggestions(type, text, reportQuery).subscribe(values => {
          expect(values).toEqual([
            {text: 'dev sec baseline, v2.0', version: '2.0', title: 'dev sec baseline, v2.0'}
          ]);
        });
      });
    });
    describe('when the item does not have a version', () => {
      it('sets the title to the text value', () => {
        const type = 'node';
        const text = 'tea';
        const reportQuery: ReportQuery = {
          endDate: moment(0).utc().startOf('day'),
          startDate: moment(0).utc().startOf('day'),
          interval: 0,
          filters: [],
          last24h: false
        };
        spyOn(suggestionsService, 'getSuggestions').and.returnValue(observableOf([
          {text: 'teal-spohn'}
        ]));

        component.getSuggestions(type, text, reportQuery).subscribe(values => {
          expect(values).toEqual([
            {text: 'teal-spohn', title: 'teal-spohn'}
          ]);
        });
      });
    });
  });

  describe('getData()', () => {
    const reportQuery: ReportQuery = {
      endDate: moment(0).utc().startOf('day'),
      startDate: moment(0).utc().startOf('day'),
      interval: 0,
      filters: [
        {type: {name: 'Node'}, value: { id: '1231' }},
        {type: {name: 'Platform'}, value: { id: 'ubuntu'}}
      ],
      last24h: false
    };
    const reportingSummaryData: ReportingSummary = {
      'stats': {
        'environments': 15,
        'nodes': 15,
        'platforms': 15,
        'profiles': 8
      },
      'status': 'failed'
    };

    beforeEach(() => {
      spyOn(statsService, 'getSummary').and.returnValue(observableOf(reportingSummaryData));
      component.getData(reportQuery);
    });

    it('sets the value for reportingSummary', () => {
      expect(component.reportData.reportingSummary).toEqual(reportingSummaryData);
    });
  });

  describe('toggleDownloadDropdown()', () => {
    it('toggles download button dropdown visibility', () => {
      component.downloadOptsVisible = false;
      component.toggleDownloadDropdown();
      expect(component.downloadOptsVisible).toEqual(true);
    });
  });

  describe('hideDownloadDropdown()', () => {
    it('hides download button dropdown', () => {
      component.downloadOptsVisible = true;
      component.hideDownloadDropdown();
      expect(component.downloadOptsVisible).toEqual(false);
    });
  });

  describe('onDownloadOptPressed', () => {
    it('calls downloadReport with correct format type', () => {
      spyOn(statsService, 'downloadReport').and.returnValue(new Observable(() => {}));
      component.onDownloadOptPressed('json');
      expect(statsService.downloadReport).toHaveBeenCalledWith('json', jasmine.any(Object));
    });
  });

  describe('applyParamFilters()', () => {
    beforeEach(() => spyOn(router, 'navigate'));

    it('parses multiple filters', () => {
      spyOn(reportQueryService, 'setState');
      const endDate = moment().utc().startOf('day').add(12, 'hours');
      const reportQuery: ReportQuery = {
        startDate: moment(endDate).subtract(10, 'days'),
        endDate: endDate,
        interval: 0,
        filters: [
          {type: {name: 'chef_tags'}, value: { text: '123' }},
          {type: {name: 'chef_tags'}, value: { text: '456'}}
        ],
        last24h: true
      };
      component.applyParamFilters([
        {type: 'chef_tags', text: '123', type_key: ''},
        {type: 'chef_tags', text: '456', type_key: ''}]);
      expect(reportQueryService.setState).toHaveBeenCalledWith(reportQuery);
    });

    it('parses single filters', () => {
      spyOn(reportQueryService, 'setState');
      const endDate = moment().utc().startOf('day').add(12, 'hours');
      const reportQuery: ReportQuery = {
        startDate: moment(endDate).subtract(10, 'days'),
        endDate: endDate,
        interval: 0,
        filters: [
          {type: {name: 'chef_tags'}, value: { text: '123' }}
        ],
        last24h: true
      };

      component.applyParamFilters([{type: 'chef_tags', text: '123', type_key: ''}]);
      expect(reportQueryService.setState).toHaveBeenCalledWith(
        reportQuery
      );
    });

    it('parse bad end date', () => {
      spyOn(reportQueryService, 'setState');
      // end date of three days ago end_time=2019-09-05
      const endDate = moment().utc().startOf('day').add(12, 'hours');
      const interval = 0;
      const startDate = reportQueryService.findTimeIntervalStartDate(interval, endDate);

      const reportQuery: ReportQuery = {
        startDate: startDate,
        endDate: endDate,
        interval: interval,
        filters: [
          {type: {name: 'chef_tags'}, value: { text: '123' }}
        ],
        last24h: false
      };

      component.applyParamFilters([
        {type: 'chef_tags', text: '123', type_key: ''},
        {type: 'end_time', text: 'bad-date', type_key: ''}]);
      expect(reportQueryService.setState).toHaveBeenCalledWith(
        reportQuery
      );
    });

    it('parse interval', () => {
      spyOn(reportQueryService, 'setState');
      const endDate = moment().utc().startOf('day').add(12, 'hours');
      const interval = 2;
      const startDate = reportQueryService.findTimeIntervalStartDate(interval, endDate);

      const reportQuery: ReportQuery = {
        startDate: startDate,
        endDate: endDate,
        interval: interval,
        filters: [
          {type: {name: 'chef_tags'}, value: { text: '123' }}
        ],
        last24h: true
      };

      component.applyParamFilters([
        {type: 'chef_tags', text: '123', type_key: ''},
        {type: 'date_interval', text: '2', type_key: ''}]);
      expect(reportQueryService.setState).toHaveBeenCalledWith(
        reportQuery
      );
    });

    it('parse bad interval', () => {
      spyOn(reportQueryService, 'setState');
      const endDate = moment().utc().startOf('day').add(12, 'hours');
      const interval = 0;
      const startDate = reportQueryService.findTimeIntervalStartDate(interval, endDate);

      const reportQuery: ReportQuery = {
        startDate: startDate,
        endDate: endDate,
        interval: interval,
        filters: [
          {type: {name: 'chef_tags'}, value: { text: '123' }}
        ],
        last24h: true
      };

      component.applyParamFilters([
        {type: 'chef_tags', text: '123', type_key: ''},
        {type: 'date_interval', text: 'bad-number', type_key: ''}]);
      expect(reportQueryService.setState).toHaveBeenCalledWith(
        reportQuery
      );
    });

    it('parse invalid interval 1', () => {
      spyOn(reportQueryService, 'setState');
      const endDate = moment().utc().startOf('day').add(12, 'hours');
      const interval = 0;
      const startDate = reportQueryService.findTimeIntervalStartDate(interval, endDate);

      const reportQuery: ReportQuery = {
        startDate: startDate,
        endDate: endDate,
        interval: interval,
        filters: [
          {type: {name: 'chef_tags'}, value: { text: '123' }}
        ],
        last24h: true
      };

      component.applyParamFilters([
        {type: 'chef_tags', text: '123', type_key: ''},
        {type: 'date_interval', text: '-1', type_key: ''}]);
      expect(reportQueryService.setState).toHaveBeenCalledWith(
        reportQuery
      );
    });

    it('parse invalid interval 2', () => {
      spyOn(reportQueryService, 'setState');
      const endDate = moment().utc().startOf('day').add(12, 'hours');
      const interval = 0;
      const startDate = reportQueryService.findTimeIntervalStartDate(interval, endDate);

      const reportQuery: ReportQuery = {
        startDate: startDate,
        endDate: endDate,
        interval: interval,
        filters: [
          {type: {name: 'chef_tags'}, value: { text: '123' }}
        ],
        last24h: true
      };

      component.applyParamFilters([
        {type: 'chef_tags', text: '123', type_key: ''},
        {type: 'date_interval', text: '10', type_key: ''}]);
      expect(reportQueryService.setState).toHaveBeenCalledWith(
        reportQuery
      );
    });
  });

  describe('onFiltersClear()', () => {
    it('with no filters', () => {
      spyOn(router, 'navigate');
      component.onFiltersClear({});
      expect(router.navigate).toHaveBeenCalledWith([], {queryParams: { }});
    });

    it('with one filter', () => {
      // Add an environment filter to the URL
      const queryParams = {...route.snapshot.queryParams};
      queryParams['environment'] = ['dev'];
      route.snapshot.queryParams = queryParams;

      spyOn(router, 'navigate');

      component.onFiltersClear({});

      // Ensure the environment filter is removed from the URL
      expect(router.navigate).toHaveBeenCalledWith([], {queryParams: { }});
    });

    it('with multiple filters', () => {
      // Add environment and organization filters to the URL
      const queryParams = {...route.snapshot.queryParams};
      queryParams['environment'] = ['dev'];
      queryParams['organization'] = ['org1'];
      route.snapshot.queryParams = queryParams;

      spyOn(router, 'navigate');

      component.onFiltersClear({});

      // Ensure all the filters are removed from the URL
      expect(router.navigate).toHaveBeenCalledWith([], {queryParams: { }});
    });

    it('with URL parameters that should not be removed', () => {
      // Add environment and end_time filters to the URL
      const queryParams = {...route.snapshot.queryParams};
      queryParams['environment'] = ['dev'];
      queryParams['end_time'] = ['2019-09-05'];
      route.snapshot.queryParams = queryParams;

      spyOn(router, 'navigate');

      component.onFiltersClear({});

      // Ensure only the environment filter is removed and the end_time filter remains in the URL
      expect(router.navigate).toHaveBeenCalledWith([],
        {queryParams: { end_time: [ '2019-09-05' ] }});
    });
  });
});
