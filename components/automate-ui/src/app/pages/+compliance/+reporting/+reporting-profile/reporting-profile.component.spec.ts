import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { Store, StoreModule } from '@ngrx/store';
import { NgrxStateAtom, runtimeChecks, ngrxReducers } from 'app/ngrx.reducers';
import { RouterTestingModule } from '@angular/router/testing';
import { ReportingProfileComponent } from '../+reporting-profile/reporting-profile.component';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { MockChefSessionService } from 'app/testing/mock-chef-session.service';
import { CookieModule } from 'ngx-cookie';
import { StatsService, ReportQueryService,
  ScanResultsService, ReportQuery } from '../../shared/reporting';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { of as observableOf } from 'rxjs';
import * as moment from 'moment/moment';

describe('ReportingProfileComponent', () => {
  let store: Store<NgrxStateAtom>;
  let fixture, component, element, statsService, reportQueryService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        CookieModule.forRoot(),
        HttpClientTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      declarations: [
        ReportingProfileComponent
      ],
      providers: [
        { provide: ChefSessionService, useClass: MockChefSessionService },
        StatsService,
        ReportQueryService,
        ScanResultsService,
        FeatureFlagsService
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });
    store = TestBed.inject(Store);
    spyOn(store, 'dispatch').and.callThrough();
    fixture = TestBed.createComponent(ReportingProfileComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
    statsService = element.injector.get(StatsService);
    reportQueryService = element.injector.get(ReportQueryService);
  });

  describe('ngOnInit()', () => {
    it('calls fetchProfile to set the data', () => {
      component.ngOnInit();
      spyOn(statsService, 'getProfileResults').and.returnValue(observableOf([]));
      spyOn(statsService, 'getProfileResultsSummary').and.returnValue(observableOf({}));
      fixture.detectChanges();
      expect(component.profile).toEqual({});
      expect(component.controls).toEqual([]);
    });

    it('calls formatReturnParams to set the reports breadcrumb href', () => {
      component.ngOnInit();
      spyOn(reportQueryService, 'formatReturnParams');
      fixture.detectChanges();

      expect(reportQueryService.formatReturnParams).toHaveBeenCalled();
    });
  });

  describe('hideScanResults', () => {
    it('sets displayscanresults to false', () => {
      component.hideScanResults();
      expect(component.displayScanResultsSidebar).toBe(false);
    });
  });

  describe('getNodes', () => {
    it('calls statsService.getNodes with the paginationOverride value', () => {
      spyOn(statsService, 'getNodes').and.returnValue(observableOf({items: []}));
      const endDate = moment().utc().startOf('day').add(12, 'hours');
      const reportQuery: ReportQuery = {
        startDate: moment(endDate).subtract(10, 'days'),
        endDate: endDate,
        interval: 0,
        filters: [ ],
        last24h: false
      };
      component.getNodes(reportQuery, {profileId: '123', controlId: '321'});

      const expectedReportQuery: ReportQuery = {
        startDate: moment(endDate).subtract(10, 'days'),
        endDate: endDate,
        interval: 0,
        filters: [
          {type: { name: 'profile_id' }, value: { text: '123'} },
          {type: { name: 'control_id' }, value: { text: '321'} }
        ],
        last24h: false
      };

      expect(statsService.getNodes).toHaveBeenCalledWith(
        expectedReportQuery,
        { perPage: 1000, page: 1, sort: 'latest_report.end_time', order: 'desc' });
    });
  });

  describe('statusIcon', () => {
    it('returns an empty string when no cases match', () => {
      expect(component.statusIcon('whoops')).toBe('');
      expect(component.statusIcon('')).toBe('');
      expect(component.statusIcon('not matching')).toBe('');
    });

    it('returns "report_problem" when status argument is "failed" ', () => {
      expect(component.statusIcon('failed')).toBe('report_problem');
    });

    it('returns "check_circle" when status argument is "passed" ', () => {
      expect(component.statusIcon('passed')).toBe('check_circle');
    });

    it('returns "help" when status argument is "skipped" ', () => {
      expect(component.statusIcon('skipped')).toBe('help');
    });
  });
});
