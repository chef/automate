import { CUSTOM_ELEMENTS_SCHEMA, DebugElement } from '@angular/core';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { CookieModule } from 'ngx-cookie';
import { of as observableOf } from 'rxjs';
import * as moment from 'moment';
import { ReportingOverviewComponent } from './reporting-overview.component';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { StatsService, ReportQueryService, ReportDataService } from '../../shared/reporting';
import { TelemetryService } from '../../../../services/telemetry/telemetry.service';
import { ReportQuery } from '../../shared/reporting';

class MockTelemetryService {
  track() { }
}
describe('ReportingOverviewComponent', () => {
  let fixture: ComponentFixture<ReportingOverviewComponent>;
  let component: ReportingOverviewComponent;
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
        ReportingOverviewComponent
      ],
      providers: [
        { provide: TelemetryService, useClass: MockTelemetryService },
        ChefSessionService,
        StatsService,
        ReportQueryService,
        ReportDataService
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(ReportingOverviewComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
    statsService = element.injector.get(StatsService);
  });

  describe('ngOnInit()', () => {
    it('sets the default selection to "Node Status"', () => {
      component.ngOnInit();
      expect(component.selectedButtonTab).toBe('Node Status');
    });
  });

  describe('getData()', () => {
    const endDate = moment().utc().startOf('day').add(12, 'hours');
    const reportQuery: ReportQuery = {
      startDate: moment(endDate).subtract(10, 'days'),
      endDate: endDate,
      interval: 0,
      filters: [ {type: { name: 'node'}, value: { id: '1231'}},
          {type: { name: 'platform'}, value: { text: 'ubuntu'}} ]
    };

    describe('when selected tab is "Node Status"', () => {
      it('gets node status data', () => {
        spyOn(component, 'getNodeStatusData');
        component.selectedButtonTab = 'Node Status';
        component.getData(reportQuery);
        expect(component.getNodeStatusData).toHaveBeenCalledWith(reportQuery);
      });
    });

    describe('when selected tab is "Profile Status"', () => {
      it('gets profile status data', () => {
        spyOn(component, 'getProfileStatusData');
        component.selectedButtonTab = 'Profile Status';
        component.getData(reportQuery);
        expect(component.getProfileStatusData).toHaveBeenCalledWith(reportQuery);
      });
    });

    describe('getNodeStatusData()', () => {
      it('gets node status failure data', () => {
        spyOn(component, 'getNodeStatusFailures');
        component.getNodeStatusData(reportQuery);
        expect(component.getNodeStatusFailures).toHaveBeenCalledWith(reportQuery);
      });

      it('gets node status summary data', () => {
        spyOn(component, 'getNodeSummary');
        component.getNodeStatusData(reportQuery);
        expect(component.getNodeSummary).toHaveBeenCalledWith(reportQuery);
      });

      it('gets node status trend data', () => {
        spyOn(component, 'getNodeTrend');
        component.getNodeStatusData(reportQuery);
        expect(component.getNodeTrend).toHaveBeenCalledWith(reportQuery);
      });
    });

    describe('getProfileStatusData()', () => {
      it('gets profile status failure data', () => {
        spyOn(component, 'getProfileStatusFailures');
        component.getProfileStatusData(reportQuery);
        expect(component.getProfileStatusFailures).toHaveBeenCalledWith(reportQuery);
      });

      it('gets profile status summary data', () => {
        spyOn(component, 'getControlsSummary');
        component.getProfileStatusData(reportQuery);
        expect(component.getControlsSummary).toHaveBeenCalledWith(reportQuery);
      });

      it('gets profile status trend data', () => {
        spyOn(component, 'getControlsTrend');
        component.getProfileStatusData(reportQuery);
        expect(component.getControlsTrend).toHaveBeenCalledWith(reportQuery);
      });
    });

    describe('getNodeStatusFailures()', () => {
      beforeEach(() => {
        spyOn(statsService, 'getFailures').and.returnValue(observableOf({
          'platforms': [{'name': 'centos', 'failures': 5}],
          'environments': [{'name': 'DevSec Prod Alpha', 'failures': 2}]
        }));
      });

      it('sets the value for bubblePlatformFailures', () => {
        component.getNodeStatusFailures(reportQuery);
        expect(component.bubblePlatformFailures).toEqual([{'name': 'centos', 'failures': 5}]);
      });

      it('sets the value for bubbleEnvironmentFailures', () => {
        component.getNodeStatusFailures(reportQuery);
        expect(component.bubbleEnvFailures).toEqual([{'name': 'DevSec Prod Alpha', 'failures': 2}]);
      });
    });

    describe('getNodeSummary()', () => {
      beforeEach(() => {
        spyOn(statsService, 'getNodeSummary').and.returnValue(observableOf({
          'noncompliant': 4,
          'compliant': 10,
          'skipped': 6,
          'high_risk': 4,
          'medium_risk': 2,
          'low_risk': 8
        }));
      });

      it('sets the value for nodeRadialData', () => {
        component.getNodeSummary(reportQuery);
        expect(component.nodeRadialData).toEqual({
          'failed': 4,
          'passed': 10,
          'skipped': 6,
          'critical': 4,
          'major': 2,
          'minor': 8
        });
      });
    });

    describe('getNodeTrend()', () => {
      beforeEach(() => {
        spyOn(statsService, 'getNodeTrend').and.returnValue(observableOf([
          {
            'report_time': '2017-03-05T00:00:00+0000',
            'passed': 7,
            'failed': 10,
            'skipped': 9
          }
        ]));
      });

      it('sets the value for nodeTrendData', () => {
        component.getNodeTrend(reportQuery);
        expect(component.nodeTrendData).toEqual([
          {
            'report_time': '2017-03-05T00:00:00+0000',
            'failed': 10,
            'passed': 7,
            'skipped': 9
          }
        ]);
      });
    });

    describe('getProfileStatusFailures()', () => {
      beforeEach(() => {
        spyOn(statsService, 'getFailures').and.returnValue(observableOf({
          'profiles': [{'name': 'SSH Baseline', 'failures': 3}],
          'controls': [{'name': 'ssh-03', 'failures': 1}]
        }));
      });

      it('sets the value for bubbleProfileFailures', () => {
        component.getProfileStatusFailures(reportQuery);
        expect(component.bubbleProfileFailures).toEqual([{'name': 'SSH Baseline', 'failures': 3}]);
      });

      it('sets the value for bubbleControlFailures', () => {
        component.getProfileStatusFailures(reportQuery);
        expect(component.bubbleControlFailures).toEqual([{'name': 'ssh-03', 'failures': 1}]);
      });
    });

    describe('getControlsSummary()', () => {
      beforeEach(() => {
        spyOn(statsService, 'getControlsSummary').and.returnValue(observableOf({
          'failures': 9,
          'passed': 1,
          'skipped': 0,
          'criticals': 2,
          'majors': 2,
          'minors': 5
        }));
      });

      it('sets the value for profileRadialData', () => {
        component.getControlsSummary(reportQuery);
        expect(component.profileRadialData).toEqual({
          'failed': 9,
          'passed': 1,
          'skipped': 0,
          'critical': 2,
          'major': 2,
          'minor': 5
        });
      });
    });

    describe('getControlsTrend()', () => {
      beforeEach(() => {
        spyOn(statsService, 'getControlsTrend').and.returnValue(observableOf([
          {
            'report_time': '2017-03-05T00:00:00+0000',
            'passed': 3,
            'failed': 9,
            'skipped': 4
          }
        ]));
      });

      it('sets the value for profileTrendData', () => {
        component.getControlsTrend(reportQuery);
        expect(component.profileTrendData).toEqual([
          {
            'report_time': '2017-03-05T00:00:00+0000',
            'passed': 3,
            'failed': 9,
            'skipped': 4
          }
        ]);
      });
    });
  });

  describe('getNodeTrend()', () => {
    const dateRange = { start: moment().toDate(), end: moment().toDate() };
    // const filters = [
    //   {'end_time': '2017-01-31T00:00:00Z'},
    //   {'type': 'Node', 'value': '1231'}
    // ];
    const endDate = moment('2017-01-31T00:00:00Z').utc();
    const reportQuery: ReportQuery = {
      startDate: moment(endDate).subtract(10, 'days'),
      endDate: endDate,
      interval: 0,
      filters: [ {type: { name: 'node'}, value: { id: '1231'}}]
    };
    const data = [
      {
        'report_time': dateRange.end,
        'passed': 22,
        'failed': 33,
        'skipped': 10
      }
    ];

    beforeEach(() => {
      spyOn(statsService, 'getNodeTrend').and.returnValue(observableOf(data));
    });

    it('queries data from stats service', () => {
      fixture.detectChanges();
      component.getNodeTrend(reportQuery);

      expect(statsService.getNodeTrend).toHaveBeenCalledWith(reportQuery);
      expect(component.nodeTrendData).toEqual(data);
    });
  });

  describe('getControlsTrend()', () => {
    const dateRange = { start: moment().toDate(), end: moment().toDate() };
    const endDate = moment('2017-01-31T00:00:00Z').utc();
    const reportQuery: ReportQuery = {
      startDate: moment(endDate).subtract(10, 'days'),
      endDate: endDate,
      interval: 0,
      filters: [ {type: { name: 'node'}, value: { id: '1231'}}]
    };
    const data = [
      {
        'report_time': dateRange.end,
        'passed': 22,
        'failed': 33,
        'skipped': 0
      }
    ];

    beforeEach(() => {
      spyOn(statsService, 'getControlsTrend').and.returnValue(observableOf(data));
    });

    it('queries data from stats service', () => {
      component.getControlsTrend(reportQuery);

      expect(statsService.getControlsTrend).toHaveBeenCalledWith(reportQuery);
      expect(component.profileTrendData).toEqual(data);
    });
  });
});
