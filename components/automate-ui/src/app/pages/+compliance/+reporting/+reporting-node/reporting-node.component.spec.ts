import { CUSTOM_ELEMENTS_SCHEMA, DebugElement } from '@angular/core';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { CookieModule } from 'ngx-cookie';
import { ReportingNodeComponent } from './reporting-node.component';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { of as observableOf } from 'rxjs';
import { StatsService, ReportQueryService, ScanResultsService } from '../../shared/reporting';

describe('ReportingNodeComponent', () => {
  let fixture: ComponentFixture<ReportingNodeComponent>;
  let component: ReportingNodeComponent;
  let element: DebugElement;
  let statsService: StatsService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        CookieModule.forRoot(),
        HttpClientTestingModule
      ],
      declarations: [
        ReportingNodeComponent
      ],
      providers: [
        ChefSessionService,
        StatsService,
        ReportQueryService,
        ScanResultsService
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(ReportingNodeComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
    statsService = element.injector.get(StatsService);
  });

  it('sets reportLoading to false', () => {
    expect(component.reportLoading).toBe(false);
  });

  describe('ngOnInit', () => {
    beforeEach(() => {
      spyOn(statsService, 'getReports').and.returnValue(observableOf(
        [ {id: '123'}, {id: '456'} ]));
      spyOn(statsService, 'getSingleReport').and.returnValue(observableOf(
        { node_name: 'teal' } ));
      component.ngOnInit();
    });

    it('calls getReports', () => {
      expect(statsService.getReports).toHaveBeenCalled();
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
  });

  describe('filteredProfiles', () => {
    const profiles = [
      {id: 1, controls: [ {status: 'passed', impact: 0.3} ]},
      {id: 2, controls: [ {status: 'skipped', impact: 0.3} ]},
      {id: 3, controls: [ {status: 'failed', impact: 0.7} ]},
      {id: 4, controls: [ {status: 'failed', impact: 0.3} ]},
      {id: 5, controls: [ {status: 'passed', impact: 0.5} ]},
      {id: 6, controls: [ {status: 'failed', impact: 0.5} ]}
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

    it('returns controls with a matching critical status', () => {
      expect(component.filteredProfiles(profiles, 'critical')).toEqual(
        [
          {id: 3, controls: [ {status: 'failed', impact: 0.7} ]}
        ]);
    });

    it('returns controls with a matching major status', () => {
      expect(component.filteredProfiles(profiles, 'major')).toEqual(
        [
          {id: 6, controls: [ {status: 'failed', impact: 0.5} ]}
        ]);
    });

    it('returns controls with a matching minor status', () => {
      expect(component.filteredProfiles(profiles, 'minor')).toEqual(
        [
          {id: 4, controls: [ {status: 'failed', impact: 0.3} ]}
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
});
