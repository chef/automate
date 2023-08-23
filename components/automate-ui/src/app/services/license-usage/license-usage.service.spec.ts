import { TestBed, fakeAsync, tick } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { BehaviorSubject } from 'rxjs';
import { LicenseUsageService } from './license-usage.service';
import { ComplianceStatsService } from '../telemetry/compliance-stats/compliance-stats.service';
import { ClientRunsStatsService } from '../telemetry/client-runs-stats/client-runs-stats.service';
import { ApplicationStatsService } from '../telemetry/application-stats/application-stats.service';
import { ConfigService } from '../config/config.service';

(window as any).postAnalyticsUsageData = function(){};

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
    deploymentType: ''
  };
  const sampleGetComplianceStats = {
    days_since_last_post: 2,
    node_cnt: 10
  }
  const sampleGetClientRunsStats = {
    days_since_last_post: 2,
    node_cnt: 10
  }
  const sampleGetApplicationStats = {
    days_since_last_post: 2,
    total_services: 10
  }


  beforeEach(() => {
    let configServiceSpyObj = jasmine.createSpyObj('ConfigService', {
      'getConfig': new BehaviorSubject(sampleGetConfig)
    });
    let complianceStatsServiceSpyObj = jasmine.createSpyObj('ComplianceStatsService', {
      'getComplianceStats': new Promise((resolve)=> {resolve(sampleGetComplianceStats)})
    });
    let clientRunsStatsServiceSpyObj = jasmine.createSpyObj('ClientRunsStatsService', {
      'getClientRunsStats': new Promise((resolve)=> {resolve(sampleGetClientRunsStats)})
    });
    let applicationStatsServiceSpyObj = jasmine.createSpyObj('ApplicationStatsService', {
      'getApplicationStats': new Promise((resolve)=> {resolve(sampleGetApplicationStats)})
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
    complianceStatsServiceSpy = TestBed.inject(ComplianceStatsService) as jasmine.SpyObj<ComplianceStatsService>;
    clientRunsStatsServiceSpy = TestBed.inject(ClientRunsStatsService) as jasmine.SpyObj<ClientRunsStatsService>;
    applicationStatsServiceSpy = TestBed.inject(ApplicationStatsService) as jasmine.SpyObj<ApplicationStatsService>;
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

    it('should call sendData', fakeAsync(() => {
      spyOn(service, 'sendData');
      service.postAnalyticsUsageDataCall()
      tick();
      expect(service.sendData).toHaveBeenCalledTimes(1);
    }));

    it('should assign values received from service', fakeAsync(() => {
      service.postAnalyticsUsageDataCall();
      tick();
      expect(service['licenseId']).toBe('00000000-0000-0000-0000-111111111111');
      expect(service['customerId']).toBe('cust1');
      expect(service['customerName']).toBe('test');
      expect(service['totalNodes']).toBe(10);
      expect(service['days_since_last_post']).toBe(2);
      expect(service['totalScans']).toBe(10);
      expect(service['totalService']).toBe(10);
    }));

  });


  describe('sendData', () => {
    
    it('should return undefined', () => {
      let output = service.sendData();
      expect(output).toBe(undefined);
    });

  });


  describe('getCurrentDateTime', () => {
    
    it('should return a string', () => {
      let output = service['getCurrentDateTime']();
      expect(typeof output).toBe('string');
    });

  });

});
