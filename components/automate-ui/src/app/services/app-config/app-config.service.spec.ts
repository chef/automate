import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { AppConfigService } from './app-config.service';

const EXPECTED_URL = '/custom_settings.js';

describe('AppConfigService', () => {
  let service: AppConfigService;
  let httpTestingController: HttpTestingController;

  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [ AppConfigService ],
      imports: [ HttpClientTestingModule ]
    });

    service = TestBed.inject(AppConfigService);
    httpTestingController = TestBed.inject(HttpTestingController);
  });

  it('should create', () => {
    expect(service).toBeDefined();
  });

  describe('#loadAppConfig', () => {
    it('should make a call get configuration', () => {
      service.loadAppConfig();

      const req = httpTestingController.expectOne(EXPECTED_URL);
      expect(req.request.method).toEqual('GET');
    });
  });
});
