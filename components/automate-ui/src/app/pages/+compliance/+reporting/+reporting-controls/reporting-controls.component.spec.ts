import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';
import { ReportQueryService, ReportDataService, StatsService } from '../../shared/reporting';
import { ReportingControlsComponent } from './reporting-controls.component';

class MockTelemetryService {
  track() { }
}

describe('ReportingControlsComponent', () => {
  let fixture: ComponentFixture<ReportingControlsComponent>;
  let component: ReportingControlsComponent;
  let reportQueryService: ReportQueryService;
  let reportDataService: ReportDataService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpClientTestingModule,
        RouterTestingModule,
        ChefPipesModule
      ],
      declarations: [
        ReportingControlsComponent
      ],
      providers: [
        ReportQueryService,
        ReportDataService,
        StatsService,
        { provide: TelemetryService, useClass: MockTelemetryService }
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(ReportingControlsComponent);
    component = fixture.componentInstance;
    reportQueryService = TestBed.inject(ReportQueryService);
    reportDataService = TestBed.inject(ReportDataService);
  });

  describe('getData()', () => {
    it('calls getReportingControlsList with applied filters', () => {
      const reportQuery = reportQueryService.getReportQuery();
      spyOn(reportDataService, 'getReportingControlsList');

      component.getData(reportQuery);

      expect(reportDataService.getReportingControlsList).toHaveBeenCalledWith(reportQuery);
    });
  });

  describe('impactStatus()', () => {
    it('returns the status name of the impact severity', () => {
      [
        [1.0, 'critical'],
        [0.7, 'critical'],
        [0.5, 'major'],
        [0.4, 'major'],
        [0.3, 'minor'],
        [0.1, 'minor']
      ].forEach(([impact, name]: [number, string]) => {
        expect(component.impactStatus({ impact })).toEqual(name);
      });
    });
  });
});
