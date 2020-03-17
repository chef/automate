import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { ConfigService } from './config.service';


import { environment } from 'environments/environment';
const GATEWAY_URL = environment.gateway_url;

describe('ConfigService', () => {
  let service: ConfigService;
  let httpTestingController: HttpTestingController;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpClientTestingModule
      ],
      providers: [
        ConfigService
      ]
    });

    service = TestBed.inject(ConfigService);
    httpTestingController = TestBed.inject(HttpTestingController);
  });

  describe('when no config file available', () => {
    it('telemetry defaults are returned', () => {
      const expectedUrl = `${GATEWAY_URL}/telemetry/config`;
      const errorMsg = '404 error';
      const expectedConfig = {
        telemetryEnabled: false,
        telemetryUrl: 'https://telemetry-acceptance.chef.io',
        licenseId: '00000000-0000-0000-0000-111111111111',
        licenseType: 'Unavailable',
        customerId: '',
        customerName: '',
        maxNodes: 0,
        deploymentId: '00000000-0000-0000-0000-111111111111'
      };

      service.getConfig().subscribe((config) => {
        expect(config).toEqual(expectedConfig);
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('GET');

      // Note 2019/06/27 (sr): When using HTTP/2, statusText will always be "OK"
      // so our logic shouldn't depend on it.
      req.flush(errorMsg, { status: 404, statusText: 'OK' });
    });
  });
});
