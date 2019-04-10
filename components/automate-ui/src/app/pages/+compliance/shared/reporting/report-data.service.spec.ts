import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { of as observableOf } from 'rxjs';
import { StatsService } from './stats.service';
import { ReportDataService } from './report-data.service';

describe('ReportDataService', () => {
  let service: ReportDataService;
  let statsService: StatsService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        StatsService,
        ReportDataService
      ],
      imports: [
        HttpClientTestingModule
      ]
    });

    service = TestBed.get(ReportDataService);
    statsService = TestBed.get(StatsService);
  });

  describe('by default', () => {
    it('sets nodesListParams to 100 per page', () => {
      expect(service.nodesListParams).toEqual(jasmine.objectContaining({
        perPage: 100,
        page: 1
      }));
    });

    it('sets nodesListParams to sort by `end_time` in `desc` order', () => {
      expect(service.nodesListParams).toEqual(jasmine.objectContaining({
        sort: 'latest_report.end_time',
        order: 'desc'
      }));
    });

    it('sets profilesListParams to 100 per page', () => {
      expect(service.profilesListParams).toEqual({
        perPage: 100,
        page: 1
      });
    });
    it('sets nodesListLoading to true', () => {
      expect(service.nodesListLoading).toEqual(true);
    });
    it('sets profilesListLoading to true', () => {
      expect(service.profilesListLoading).toEqual(true);
    });
  });

  describe('getReportingSummary()', () => {
    it('fetches reporting summary data', () => {
      const filters = [];
      const data = {stats: {}};
      spyOn(statsService, 'getSummary').and.returnValue(observableOf(data));

      service.getReportingSummary(filters);

      expect(statsService.getSummary).toHaveBeenCalledWith(filters);
    });
  });

  describe('getReportingNodesList()', () => {
    it('fetches nodes data', () => {
      const filters = [];
      const params = {};
      const data = [];
      spyOn(statsService, 'getNodes').and.returnValue(observableOf(data));

      service.getReportingNodesList(filters, params);

      expect(statsService.getNodes).toHaveBeenCalledWith(filters, params);
    });
    it('sets nodesListLoading to false', () => {
      expect(service.nodesListLoading).toEqual(true);
    });
  });

  describe('getReportingProfilesList()', () => {
    it('fetches profiles data', () => {
      const filters = [];
      const params = {};
      const data = [];
      spyOn(statsService, 'getProfiles').and.returnValue(observableOf(data));

      service.getReportingProfilesList(filters, params);

      expect(statsService.getProfiles).toHaveBeenCalledWith(filters, params);
    });
    it('sets nodesListLoading to false', () => {
      expect(service.profilesListLoading).toEqual(true);
    });
  });
});
