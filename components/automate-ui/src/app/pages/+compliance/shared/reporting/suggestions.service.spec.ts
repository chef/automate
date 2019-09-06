import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { CookieModule } from 'ngx-cookie';
import { SuggestionsService } from './suggestions.service';
import { StatsService } from './stats.service';
import { environment } from '../../../../../environments/environment';
import { ReportQuery } from './report-query.service';
import * as moment from 'moment';

const COMPLIANCE_URL = environment.compliance_url;

describe('SuggestionsService', () => {
  let httpTestingController: HttpTestingController;
  let service: SuggestionsService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        CookieModule.forRoot(),
        HttpClientTestingModule
      ],
      providers: [
        SuggestionsService,
        StatsService
      ]
    });

    service = TestBed.get(SuggestionsService);
    httpTestingController = TestBed.get(HttpTestingController);
  });

  describe('getSuggestions()', () => {

    it('fetches suggestions', () => {
      const type = 'platform';
      const text = 'win';
      const startDate = moment('2017-11-14T00:00:00Z').utc();
      const endDate = moment('2017-11-15T23:59:59Z').utc();
      const reportQuery: ReportQuery = {
        endDate: endDate,
        startDate: startDate,
        interval: 0,
        filters: []
      };

      const expectedUrl = `${COMPLIANCE_URL}/reporting/suggestions`;
      const expectedSuggestions = ['window-7', 'windows-8'];
      const mockResp = { suggestions: expectedSuggestions };

      service.getSuggestions(type, text, reportQuery).subscribe(suggestions => {
        expect(suggestions).toEqual(expectedSuggestions);
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('POST');
      expect(req.request.body).toEqual({type, text,
        filters: [
          { type: 'start_time', values: ['2017-11-14T00:00:00Z'] },
          { type: 'end_time', values: ['2017-11-15T23:59:59Z'] }
        ]
      });

      req.flush(mockResp);
    });

    it('returns maximum of 100 items', () => {
      const type = 'platform';
      const text = 'win';
      const startDate = moment('2017-11-14T00:00:00Z').utc();
      const endDate = moment('2017-11-15T23:59:59Z').utc();
      const reportQuery: ReportQuery = {
        endDate: endDate,
        startDate: startDate,
        interval: 0,
        filters: []
      };

      const expectedUrl = `${COMPLIANCE_URL}/reporting/suggestions`;
      const expectedSuggestions = Array.from({ length: 150 }).map(i => ({ text: `suggest-${i}` }));
      const mockResp = { suggestions: expectedSuggestions };

      service.getSuggestions(type, text, reportQuery).subscribe(suggestions => {
        expect(suggestions.length).toEqual(100);
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('POST');
      expect(req.request.body).toEqual({type, text,
        filters: [
          { type: 'start_time', values: ['2017-11-14T00:00:00Z'] },
          { type: 'end_time', values: ['2017-11-15T23:59:59Z'] }
        ]
      });

      req.flush(mockResp);
    });
  });
});
