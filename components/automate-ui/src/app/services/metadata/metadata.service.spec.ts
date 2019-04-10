import { TestBed, inject } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';

import { MetadataService } from './metadata.service';

describe('MetadataService', () => {
  let metadata_service: MetadataService;
  let httpTestingController: HttpTestingController;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpClientTestingModule
      ],
      providers: [
        MetadataService
      ]
    });

    metadata_service = TestBed.get(MetadataService);
    httpTestingController = TestBed.get(HttpTestingController);
  });

  it('should be created', inject([MetadataService], (service: MetadataService) => {
    expect(service).toBeTruthy();
  }));

  describe('#getBuildVersion', () => {

    it('fetches current build version', () => {
      const expectedUrl = '/api/v0/version';
      const expectedVersion = '20180416135645';
      const body = { build_timestamp: expectedVersion };

      metadata_service.getBuildVersion().subscribe((version) => {
        expect(version).toEqual(expectedVersion);
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('GET');

      req.flush(body);
    });
  });
});
