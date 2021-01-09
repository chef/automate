import { CUSTOM_ELEMENTS_SCHEMA, DebugElement } from '@angular/core';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { Store, StoreModule } from '@ngrx/store';
import { NgrxStateAtom, runtimeChecks, ngrxReducers } from 'app/ngrx.reducers';
import { CookieModule } from 'ngx-cookie';
import { ReportingNodeComponent } from './reporting-node.component';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { Observable, of as observableOf } from 'rxjs';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { StatsService, ReportQueryService, ScanResultsService } from '../../shared/reporting';
import { DatetimePipe } from 'app/pipes/datetime.pipe';

describe('ReportingNodeComponent', () => {
  let store: Store<NgrxStateAtom>;
  let fixture: ComponentFixture<ReportingNodeComponent>;
  let component: ReportingNodeComponent;
  let element: DebugElement;
  let statsService: StatsService;
  let reportQueryService: ReportQueryService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        CookieModule.forRoot(),
        HttpClientTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      declarations: [
        ReportingNodeComponent,
        DatetimePipe
      ],
      providers: [
        ChefSessionService,
        StatsService,
        ReportQueryService,
        ScanResultsService,
        FeatureFlagsService
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });
    store = TestBed.inject(Store);
    spyOn(store, 'dispatch').and.callThrough();
    fixture = TestBed.createComponent(ReportingNodeComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
    statsService = element.injector.get(StatsService);
    reportQueryService = element.injector.get(ReportQueryService);
  });

  it('sets reportLoading to false', () => {
    expect(component.reportLoading).toBe(false);
  });

  describe('ngOnInit', () => {
    beforeEach(() => {
      spyOn(statsService, 'getReportsWithPages').and.returnValue(observableOf(
        { reports: [ {id: '123'}, {id: '456'} ], totalReports: 2}));
      spyOn(statsService, 'getSingleReport').and.returnValue(observableOf(
        { node_name: 'teal' } ));
      spyOn(reportQueryService, 'formatReturnParams');
      component.ngOnInit();
    });

    it('calls getReportsWithPages', () => {
      expect(statsService.getReportsWithPages).toHaveBeenCalled();
    });
    it('calls getSingleReport with the first report id', () => {
      expect(statsService.getSingleReport).toHaveBeenCalledWith('123', jasmine.any(Object));
    });
    it('sets reportLoading to false', () => {
      expect(component.reportLoading).toBe(false);
    });
    it('sets the value for activeReport', () => {
      expect(component.activeReport).toEqual({ id: '123', node_name: 'teal' });
    });
    it('calls formatReturnParams to set the reports breadcrumb href', () => {
      expect(reportQueryService.formatReturnParams).toHaveBeenCalled();
    });
  });

  describe('filteredProfiles', () => {
    const profiles = [
      {id: 1, controls: [ {status: 'passed', impact: 0.3} ]},
      {id: 2, controls: [ {status: 'skipped', impact: 0.3} ]},
      {id: 3, controls: [ {status: 'failed', impact: 0.7} ]},
      {id: 4, controls: [ {status: 'failed', impact: 0.3} ]},
      {id: 5, controls: [ {status: 'passed', impact: 0.5} ]},
      {id: 6, controls: [ {status: 'failed', impact: 0.5} ]},
      {id: 7, controls: [ {status: 'waived', impact: 0.5} ]}
    ];

    it('returns controls with a matching passed status', () => {
      expect(component.filteredProfiles(profiles, 'passed')).toEqual(
        [
          {id: 1, controls: [ {status: 'passed', impact: 0.3} ]},
          {id: 5, controls: [ {status: 'passed', impact: 0.5} ]}
        ]);
    });

    it('returns controls with a matching skipped status', () => {
      expect(component.filteredProfiles(profiles, 'skipped')).toEqual(
        [
          {id: 2, controls: [ {status: 'skipped', impact: 0.3} ]}
        ]);
    });

    it('returns controls with a matching failed status', () => {
      expect(component.filteredProfiles(profiles, 'failed')).toEqual(
        [
          {id: 3, controls: [ {status: 'failed', impact: 0.7} ]},
          {id: 4, controls: [ {status: 'failed', impact: 0.3} ]},
          {id: 6, controls: [ {status: 'failed', impact: 0.5} ]}
        ]);
    });

    it('returns controls with a matching waived status', () => {
      expect(component.filteredProfiles(profiles, 'waived')).toEqual(
        [
          { id: 7, controls: [{ status: 'waived', impact: 0.5 }] }
        ]);
    });
  });

  describe('profilesByStatus', () => {
    const profiles = [
      {id: 1, status: 'passed'},
      {id: 2, status: 'skipped'},
      {id: 3, status: 'failed'},
      {id: 4, status: 'passed'},
      {id: 5, status: 'skipped'}
    ];

    it('returns profiles that match given status', () => {
      expect(component.profilesByStatus(profiles, 'passed')).toEqual([
        {id: 1, status: 'passed'},
        {id: 4, status: 'passed'}
      ]);
      expect(component.profilesByStatus(profiles, 'failed')).toEqual([
        {id: 3, status: 'failed'}
      ]);
      expect(component.profilesByStatus(profiles, 'skipped')).toEqual([
        {id: 2, status: 'skipped'},
        {id: 5, status: 'skipped'}
      ]);
    });
  });
  describe('onDownloadNodeReport', () => {
    it('calls downloadReport with correct format type', () => {
      spyOn(statsService, 'downloadNodeReport').and.returnValue(new Observable(() => {}));
      component.onDownloadNodeReport('json');
      expect(statsService.downloadNodeReport).toHaveBeenCalledWith('json', jasmine.any(Object));
    });
  });
});
