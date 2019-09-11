import { CUSTOM_ELEMENTS_SCHEMA, DebugElement } from '@angular/core';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { Router, ActivatedRoute } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { CookieModule } from 'ngx-cookie';
import { Observable, of as observableOf } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { ReportingComponent } from '../+reporting/reporting.component';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import {
  StatsService,
  SuggestionsService,
  ReportQueryService,
  ReportDataService,
  ReportQuery
} from '../shared/reporting';
import { TelemetryService } from '../../../services/telemetry/telemetry.service';
import * as moment from 'moment';

class MockTelemetryService {
  track() { }
}
describe('ReportingComponent', () => {
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
        HttpClientTestingModule
      ],
      declarations: [
        ReportingComponent
      ],
      providers: [
        { provide: TelemetryService, useClass: MockTelemetryService },
        ChefSessionService,
        StatsService,
        SuggestionsService,
        ReportQueryService,
        ReportDataService
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(ReportingComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
    statsService = element.injector.get(StatsService);
    suggestionsService = element.injector.get(SuggestionsService);
    reportQueryService = element.injector.get(ReportQueryService);
    router = TestBed.get(Router);
    route = TestBed.get(ActivatedRoute);
  });

  it('displays tabs', () => {
    const qe = element.query(By.css('.nav-tabs-list'));
    expect(qe).not.toBeNull();
  });

  describe('sets availableFilterTypes', () => {
    it('to include profile, platform, environment, node, and control', () => {
      const availableFilterTypesNames = component.availableFilterTypes.map( type => type.name);

      const expected = [
        'chef_server', 'chef_tags', 'control', 'environment', 'node', 'organization', 'platform',
        'policy_group', 'policy_name', 'profile', 'recipe', 'role', 'inspec_version'];

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
    it('sets date range on report query', () => {
      spyOn(router, 'navigate');
      const endDate = new Date();
      const event = {detail: endDate};
      component.onEndDateChanged(event);
      expect(router.navigate).toHaveBeenCalled();
    });
  });

  describe('getSuggestions()', () => {
    describe('when the item has a version', () => {
      it('sets the title to text, version values to display to the user', () => {
        const type = 'profile';
        const text = 'dev';
        const reportQuery: ReportQuery = {
          endDate: moment(0).utc().startOf('day'),
          startDate: moment(0).utc().startOf('day'),
          interval: 0,
          filters: []
        };
        spyOn(suggestionsService, 'getSuggestions').and.returnValue(observableOf([
          {text: 'dev sec baseline', version: '2.0'}
        ]));

        component.getSuggestions(type, text, reportQuery).subscribe(values => {
          expect(values).toEqual([
            {text: 'dev sec baseline', version: '2.0', title: 'dev sec baseline, v2.0'}
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
          filters: []
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
      ]
    };
    const reportingSummaryData = {
      'duration': 49258.00239,
      'start_date': '2017-06-22T00:21:21Z',
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
        ]
      };
      component.applyParamFilters([
        {type: 'chef_tags', text: '123'},
        {type: 'chef_tags', text: '456'}]);
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
        ]
      };

      component.applyParamFilters([{type: 'chef_tags', text: '123'}]);
      expect(reportQueryService.setState).toHaveBeenCalledWith(
        reportQuery
      );
    });

    it('parse end date', () => {
      spyOn(reportQueryService, 'setState');
      // end date of three days ago end_time=2019-09-05
      const endDate = moment('2019-09-05', 'YYYY-MM-DD').utc().
        startOf('day').add(12, 'hours');
      const interval = 0;
      const startDate = reportQueryService.findTimeIntervalStartDate(interval, endDate);

      const reportQuery: ReportQuery = {
        startDate: startDate,
        endDate: endDate,
        interval: interval,
        filters: [
          {type: {name: 'chef_tags'}, value: { text: '123' }}
        ]
      };

      component.applyParamFilters([
        {type: 'chef_tags', text: '123'},
        {type: 'end_time', text: '2019-09-05'}]);
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
        ]
      };

      component.applyParamFilters([
        {type: 'chef_tags', text: '123'},
        {type: 'end_time', text: 'bad-date'}]);
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
        ]
      };

      component.applyParamFilters([
        {type: 'chef_tags', text: '123'},
        {type: 'date_interval', text: '2'}]);
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
        ]
      };

      component.applyParamFilters([
        {type: 'chef_tags', text: '123'},
        {type: 'date_interval', text: 'bad-number'}]);
      expect(reportQueryService.setState).toHaveBeenCalledWith(
        reportQuery
      );
    });

    it('parse invaild interval 1', () => {
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
        ]
      };

      component.applyParamFilters([
        {type: 'chef_tags', text: '123'},
        {type: 'date_interval', text: '-1'}]);
      expect(reportQueryService.setState).toHaveBeenCalledWith(
        reportQuery
      );
    });

    it('parse invaild interval 2', () => {
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
        ]
      };

      component.applyParamFilters([
        {type: 'chef_tags', text: '123'},
        {type: 'date_interval', text: '10'}]);
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
