import { TestBed, fakeAsync, tick } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { BehaviorSubject } from 'rxjs';
import { LicenseUsageService } from './license-usage.service';
import { ComplianceStatsService } from '../telemetry/compliance-stats/compliance-stats.service';
import { ClientRunsStatsService } from '../telemetry/client-runs-stats/client-runs-stats.service';
import { ApplicationStatsService } from '../telemetry/application-stats/application-stats.service';
import { ConfigService } from '../config/config.service';

(window as any).postAnalyticsUsageData = function () { };

describe('LicenseUsageService', () => {
  let service: LicenseUsageService;
  let configServiceSpy: jasmine.SpyObj<ConfigService>;
  let complianceStatsServiceSpy: jasmine.SpyObj<ComplianceStatsService>;
  let clientRunsStatsServiceSpy: jasmine.SpyObj<ClientRunsStatsService>;
  let applicationStatsServiceSpy: jasmine.SpyObj<ApplicationStatsService>;
  const sampleGetConfig = {
    telemetryEnabled: false,
    telemetryUrl: 'https://telemetry-acceptance.chef.io',
    licenseId: '00000000-0000-0000-0000-111111111111',
    licenseType: 'Unavailable',
    customerId: 'cust1',
    customerName: 'test',
    maxNodes: 0,
    deploymentId: '00000000-0000-0000-0000-111111111111',
    deploymentType: 'test'
  };
  const sampleGetComplianceStats = {
    days_since_last_post: 2,
    node_cnt: 10
  };
  const sampleGetClientRunsStats = {
    days_since_last_post: 2,
    node_cnt: 10
  };
  const sampleGetApplicationStats = {
    days_since_last_post: 2,
    total_services: 10
  };


  beforeEach(() => {
    const configServiceSpyObj = jasmine.createSpyObj('ConfigService', {
      'getConfig': new BehaviorSubject(sampleGetConfig)
    });
    const complianceStatsServiceSpyObj = jasmine.createSpyObj('ComplianceStatsService', {
      'getComplianceStats': new Promise((resolve) => {
        resolve(sampleGetComplianceStats);
      })
    });
    const clientRunsStatsServiceSpyObj = jasmine.createSpyObj('ClientRunsStatsService', {
      'getClientRunsStats': new Promise((resolve) => {
        resolve(sampleGetClientRunsStats);
      })
    });
    const applicationStatsServiceSpyObj = jasmine.createSpyObj('ApplicationStatsService', {
      'getApplicationStats': new Promise((resolve) => {
        resolve(sampleGetApplicationStats);
      })
    });
    TestBed.configureTestingModule({
      imports: [
        HttpClientTestingModule
      ],
      providers: [
        LicenseUsageService,
        {
          provide: ComplianceStatsService,
          useValue: complianceStatsServiceSpyObj
        },
        {
          provide: ClientRunsStatsService,
          useValue: clientRunsStatsServiceSpyObj
        },
        {
          provide: ApplicationStatsService,
          useValue: applicationStatsServiceSpyObj
        },
        {
          provide: ConfigService,
          useValue: configServiceSpyObj
        }
      ]
    });
    service = TestBed.inject(LicenseUsageService);
    configServiceSpy = TestBed.inject(ConfigService) as jasmine.SpyObj<ConfigService>;
    complianceStatsServiceSpy =
      TestBed.inject(ComplianceStatsService) as jasmine.SpyObj<ComplianceStatsService>;
    clientRunsStatsServiceSpy =
      TestBed.inject(ClientRunsStatsService) as jasmine.SpyObj<ClientRunsStatsService>;
    applicationStatsServiceSpy =
      TestBed.inject(ApplicationStatsService) as jasmine.SpyObj<ApplicationStatsService>;
  });


  it('should be created', () => {
    expect(service).toBeTruthy();
  });


  describe('postAnalyticsUsageDataCall', () => {

    it('should call configService.getConfig', fakeAsync(() => {
      service.postAnalyticsUsageDataCall();
      tick();
      expect(configServiceSpy.getConfig).toHaveBeenCalledTimes(1);
    }));

    it('should call complianceStatsService.getComplianceStats', fakeAsync(() => {
      service.postAnalyticsUsageDataCall();
      tick();
      expect(complianceStatsServiceSpy.getComplianceStats).toHaveBeenCalledTimes(1);
    }));

    it('should call clientRunsStatsService.getClientRunsStats', fakeAsync(() => {
      service.postAnalyticsUsageDataCall();
      tick();
      expect(clientRunsStatsServiceSpy.getClientRunsStats).toHaveBeenCalledTimes(1);
    }));

    it('should call applicationStatsService.getApplicationStats', fakeAsync(() => {
      service.postAnalyticsUsageDataCall();
      tick();
      expect(applicationStatsServiceSpy.getApplicationStats).toHaveBeenCalledTimes(1);
    }));

    it('should assign values received from service', fakeAsync(() => {
      service.postAnalyticsUsageDataCall();
      tick();
      expect(service['deploymentId']).toBe('00000000-0000-0000-0000-111111111111');
      expect(service['deploymentType']).toBe('test');
      expect(service['licenseId']).toBe('00000000-0000-0000-0000-111111111111');
      expect(service['customerId']).toBe('cust1');
      expect(service['customerName']).toBe('test');
      expect(service['isConfigDataLoaded']).toBe(true);
      expect(service['totalNodes']).toBe(10);
      expect(service['daysSinceLasPost']).toBe(2);
      expect(service['totalScans']).toBe(10);
      expect(service['totalService']).toBe(10);
      expect(service['isSummaryDataLoaded']).toBe(true);
    }));

  });


  describe('constructPayload', () => {

    it('should return undefined', () => {
      const output = service.constructPayload();
      expect(output).toBe(undefined);
    });

    it('should assign object to payload', () => {
      const output = service.constructPayload();
      expect(output).toBe(undefined);
      expect(typeof service['payload']).toBe('object');
    });

  });


  describe('postData', () => {

    it('should call constructPayload', () => {
      const handleSpy = spyOn(LicenseUsageService.prototype as any, 'isAllDataLoaded');
      handleSpy.and.callFake(() => {
        return true;
      });
      spyOn(service, 'constructPayload');
      const output = service.postData();
      expect(output).toBe(undefined);
      expect(service.constructPayload).toHaveBeenCalledTimes(1);
    });

    it('should call setInterval', fakeAsync(() => {
      spyOn(window, 'setInterval');
      service.postData();
      tick();
      expect(window.setInterval).toHaveBeenCalledTimes(1);
    }));

  });


  describe('isAllDataLoaded', () => {

    it('should return false', () => {
      const output = service['isAllDataLoaded']();
      expect(output).toBe(false);
    });

    it('should return true', () => {
      service['isExpirationLoaded'] = true;
      service['isProductVersionLoaded'] = true;
      service['isConfigDataLoaded'] = true;
      service['isSummaryDataLoaded'] = true;
      const output = service['isAllDataLoaded']();
      expect(output).toBe(true);
    });

  });


  describe('getCurrentDateTime', () => {

    it('should return a string', () => {
      const output = service['getCurrentDateTime']();
      expect(typeof output).toBe('string');
    });

  });

});
