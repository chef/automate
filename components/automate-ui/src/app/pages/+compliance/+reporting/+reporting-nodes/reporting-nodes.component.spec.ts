import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { CookieModule } from 'ngx-cookie';
import { ReportingNodesComponent } from './reporting-nodes.component';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { StatsService, ReportQueryService, ReportDataService } from '../../shared/reporting';
import { TelemetryService } from '../../../../services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}
describe('ReportingNodesComponent', () => {
  let fixture: ComponentFixture<ReportingNodesComponent>;
  let component: ReportingNodesComponent;
  let reportQuery: ReportQueryService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        CookieModule.forRoot(),
        HttpClientTestingModule
      ],
      declarations: [
        ReportingNodesComponent
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

    fixture = TestBed.createComponent(ReportingNodesComponent);
    component = fixture.componentInstance;
    reportQuery = TestBed.get(ReportQueryService);
  });

  describe('addProfileStatus', () => {
    describe('if there are any failures', () => {
      const data = {items: [
        { failures: 23, passed: 29, skipped: 3, name: 'linux-baseline' },
        { failures: 1, passed: 20, skipped: 3, name: 'ssh-baseline' },
        { failures: 3, passed: 0, skipped: 24, name: 'apache-baseline' }
      ]};
      const expected = [
        { failures: 23, passed: 29, skipped: 3, name: 'linux-baseline', status: 'failed' },
        { failures: 1, passed: 20, skipped: 3, name: 'ssh-baseline', status: 'failed' },
        { failures: 3, passed: 0, skipped: 24, name: 'apache-baseline', status: 'failed' }
      ];

      it('sets the status for the profile to failed', () => {
        expect(component.addProfileStatus(data)).toEqual(expected);
      });
    });

    describe('if all are skipped', () => {
      const data = {items: [
        { failures: 0, passed: 0, skipped: 30, name: 'linux-baseline' }
      ]};
      const expected = [
        { failures: 0, passed: 0, skipped: 30, name: 'linux-baseline', status: 'skipped' }
      ];
      it('sets the status for the profile to skipped', () => {
        expect(component.addProfileStatus(data)).toEqual(expected);
      });
    });

    describe('if there are no failures and one or more passed', () => {
      const data = {items: [
        { failures: 0, passed: 29, skipped: 0, name: 'linux-baseline' },
        { failures: 0, passed: 20, skipped: 3, name: 'ssh-baseline' }
      ]};
      const expected = [
        { failures: 0, passed: 29, skipped: 0, name: 'linux-baseline', status: 'passed' },
        { failures: 0, passed: 20, skipped: 3, name: 'ssh-baseline', status: 'passed' }
      ];
      it('sets the status for the profile to passed', () => {
        expect(component.addProfileStatus(data)).toEqual(expected);
      });
    });
  });

  describe('onNodesListPageChanged()', () => {
    it('calls getData with applied filters', () => {
      const filters = reportQuery.filters.getValue();
      spyOn(component, 'getData');

      component.onNodesListPageChanged({detail: { value: 2}});

      expect(component.getData).toHaveBeenCalledWith(filters);
    });
  });

  describe('onNodesListSortToggled()', () => {
    it('calls getData with applied filters', () => {
      const filters = reportQuery.filters.getValue();
      spyOn(component, 'getData');

      component.onNodesListSortToggled({detail: {sort: 'name', order: 'asc'}});

      expect(component.getData).toHaveBeenCalledWith(filters);
    });
  });
});
