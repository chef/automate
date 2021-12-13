import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { TestBed } from '@angular/core/testing';
import { DownloadReportsService } from './download-reports.service';
import { StoreModule } from '@ngrx/store';
import {
    ngrxReducers,
    runtimeChecks
  } from 'app/ngrx.reducers';
import { environment } from '../../../environments/environment';
import { ReportType } from './download-reports.model';

const REPORT_LIST_API_URL = environment.download_report_list_url;

describe('DownloadReportsService', () => {
  let service: DownloadReportsService;
  let httpTestingController: HttpTestingController;
  const initialState = {};

  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        DownloadReportsService
      ],
      imports: [
        StoreModule.forRoot(ngrxReducers, { initialState, runtimeChecks }),
        HttpClientTestingModule
      ]
    });
    service = TestBed.inject(DownloadReportsService);
    httpTestingController = TestBed.inject(HttpTestingController);
  });

  it('should create', () => {
    expect(service).toBeDefined();
  });

  it('should return file size with units ', () => {
    const fileSize = 1024; // bytes
    const size = service.byteConverter(fileSize);
    expect(size).toEqual('1kb');
  });

  it('should get the file name of the report', () => {
    const created_at = '2021-11-23T05:39:30.406778Z';
    const fileName = service.getFilename(created_at);
    expect(fileName).toEqual('2021-11-23');
  });

  it('should open the report panel', () => {
    service.showOpenReport = false;
    service.onReportOpenClick();
    expect(service.showOpenReport).toBe(true);
  });

  it('should close the report panel', () => {
    service.showOpenReport = true;
    service.onReportCloseClick();
    expect(service.showOpenReport).toBe(false);
  });

  it('should initiate the long polling', () => {
    service.isLongPollRunning = false;
    service.initiateLongPolling();

    expect(service.isLongPollRunning).toBe(true);

    const expectedUrl = `${REPORT_LIST_API_URL}/requests`;
    const reportPanelMockData = [];

    const req = httpTestingController.expectOne(expectedUrl);

    expect(req.request.method).toEqual('GET');

    req.flush(reportPanelMockData); // return empty data

    expect(service.reportList.length).toEqual(0);
    expect(service.isLongPollRunning).toBe(false);
  });

  it('should fetch the report list with status', () => {
    const expectedUrl = `${REPORT_LIST_API_URL}/requests`;
    const expectedData = [{
      'acknowledgement_id': '32f9a9c6-fe1f-456e-86de-6f5e308bab7e',
      'status': 'success',
      'report_size': '5000',
      'err_message': '',
      'created_at': '2021-11-23T05:39:30.406778Z',
      'ended_at': '2021-11-23T05:39:30.502190Z',
      'duration': '30m',
      'report_type': 'json'
    }];

    service.fetchReportList().subscribe((data) => {
      expect(data).toEqual(expectedData);
    });

    const req = httpTestingController.expectOne(expectedUrl);

    expect(req.request.method).toEqual('GET');

    req.flush(expectedData);
  });

  it('should verify the download report status', () => {
    service.reportList = [{
      'acknowledgement_id': '32f9a9c6-fe1f-456e-86de-6f5e308bab7e',
      'status': 'success',
      'report_size': '5000',
      'err_message': '',
      'created_at': '2021-11-23T05:39:30.406778Z',
      'ended_at': '2021-11-23T05:39:30.502190Z',
      'duration': '30m',
      'report_type': 'json'
    }];
    service.isLongPollRunning = true;
    service.checkReportStatus();

    expect(service.isLongPollRunning).toBe(false);
  });

  it('should fetch the downloaded report', () => {
    const expectedData = 'report';
    const ackId = '32f9a9c6-fe1f-456e-86de-6f5e308bab7e';
    const expectedUrl = `${REPORT_LIST_API_URL}/export/${ackId}`;

    service.downloadReport(ackId).subscribe((data) => {
      expect(data).toEqual(expectedData);
    });

    const req = httpTestingController.expectOne(expectedUrl);

    expect(req.request.method).toEqual('GET');
    expect(req.request.responseType).toEqual('text');

    req.flush(expectedData);
  });

  it('should click the download link', () => {
    const report: ReportType = {
      'acknowledgement_id': '32f9a9c6-fe1f-456e-86de-6f5e308bab7e',
      'status': 'success',
      'report_size': '5000',
      'err_message': '',
      'created_at': '2021-11-23T05:39:30.406778Z',
      'ended_at': '2021-11-23T05:39:30.502190Z',
      'duration': '30m',
      'report_type': 'json'
    };
    service.onLinkToDownload(report);

    const expectedData = 'report';
    const ackId = '32f9a9c6-fe1f-456e-86de-6f5e308bab7e';
    const expectedUrl = `${REPORT_LIST_API_URL}/export/${ackId}`;

    const req = httpTestingController.expectOne(expectedUrl);

    expect(req.request.method).toEqual('GET');
    expect(req.request.responseType).toEqual('text');

    req.flush(expectedData);
  });
});
