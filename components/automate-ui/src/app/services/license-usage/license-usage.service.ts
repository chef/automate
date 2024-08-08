import { Injectable } from '@angular/core';
import { ComplianceStatsService } from '../telemetry/compliance-stats/compliance-stats.service';
import { ClientRunsStatsService } from '../telemetry/client-runs-stats/client-runs-stats.service';
import { ApplicationStatsService } from '../telemetry/application-stats/application-stats.service';
import { ConfigService } from '../config/config.service';
import { LicenseStatus } from '../../entities/license/license.model';
import { HttpClient } from '@angular/common/http';
import { environment as env } from '../../../environments/environment';
import { BehaviorSubject } from 'rxjs';

declare var postAnalyticsUsageData: any;

@Injectable({
  providedIn: 'root'
})
export class LicenseUsageService {

  private licenseId;
  private customerId;
  private customerName;
  private totalNodes;
  private totalScans;
  private totalService;
  public totalNodesSubject = new BehaviorSubject(null);
  public totalScansSubject = new BehaviorSubject(null);
  public totalServiceSubject = new BehaviorSubject(null);
  public initCountFetch = new BehaviorSubject<any>(null);
  private expiration;
  private daysSinceLasPost;
  private periodStartDate;
  private periodEndDate;
  private deploymentId;
  private deploymentType;
  private productVersion;
  private payload;
  private retryPostDataInterval;

  private isExpirationLoaded = false;
  private isProductVersionLoaded = false;
  private isConfigDataLoaded = false;
  private isSummaryDataLoaded = false;
  

  constructor(
    private complianceStatsService: ComplianceStatsService,
    private clientRunsStatsService: ClientRunsStatsService,
    private applicationStatsService: ApplicationStatsService,
    private configService: ConfigService,
    private http: HttpClient
  ) {}

  async postAnalyticsUsageDataCall() {
    this.initCountFetch.next(true);
    this.http.get<LicenseStatus>(`${env.gateway_url}/license/status`).subscribe(data => {
      this.expiration = data.licensed_period.end;
      this.isExpirationLoaded = true;
    });

    this.http.get(`${env.gateway_url}/version`).subscribe(data => {
      this.productVersion = data['build_timestamp'];
      this.isProductVersionLoaded = true;
    });

    this.configService.getConfig().subscribe(data => {
      this.deploymentId = data.deploymentId;
      this.deploymentType = data.deploymentType;
      this.licenseId = data.licenseId;
      this.customerId = data.customerId;
      this.customerName = data.customerName;

      this.isConfigDataLoaded = true;
    });

    const complianceUsageStats = await this.complianceStatsService.getComplianceStats();
    this.totalScans = complianceUsageStats['node_cnt'];
    this.totalScansSubject.next(this.totalScans);

    // ignore complianceUsageStats['days_since_last_post'] to align with analytics cli
    // set report period to 30 days
    this.daysSinceLasPost = 30; //complianceUsageStats['days_since_last_post'];


    const nodeUsageStats = await this.clientRunsStatsService.getClientRunsStats();
    this.totalNodes = nodeUsageStats['node_cnt'];
    this.totalNodesSubject.next(this.totalNodes);
    
    const applicationUsageStats = await this.applicationStatsService.getApplicationStats();
    this.totalService = applicationUsageStats['total_services'];
    this.totalServiceSubject.next(this.totalService);

    const start = new Date();
    start.setDate(start.getDate() - this.daysSinceLasPost);
    start.setHours(0, 0, 0, 0);
    this.periodStartDate = start.toISOString();

    const end = new Date();
    end.setHours(11, 59, 0, 0);
    end.setDate(end.getDate() - 1);
    this.periodEndDate = end.toISOString();

    this.isSummaryDataLoaded = true;
  }

  constructPayload() {
    const data = {
      'licenseId': this.licenseId,
      'customerId': this.customerId,
      'expiration': this.expiration,
      'customerName': this.customerName,
      'metadata': {
        'Automate': {
          'instanceId': this.deploymentId,
          'deploymentType': this.deploymentType
        }
      },
      'periods': [{
        'version': this.productVersion,
        'date': this.getCurrentDateTime(),
        'period': {
          'start': this.periodStartDate,
          'end': this.periodEndDate
        },
        'summary': {
          'nodes': {
            'total': parseInt(this.totalNodes, 10)
          },
          'scans': {
            'targets': parseInt(this.totalScans, 10)
          },
          'services': {
            'targets': parseInt(this.totalService, 10)
          }
        }
      }],
      'source': 'Automate',
      'scannerVersion': '0.1.0',
      'scannedOn': this.getCurrentDateTime()
    };
    this.payload = data;
  }

  postData() {
    clearInterval(this.retryPostDataInterval);
    if (this.isAllDataLoaded()) {
      this.constructPayload();
      if ( postAnalyticsUsageData !== null || postAnalyticsUsageData !== undefined ) {
        postAnalyticsUsageData(this.payload);
      }
    } else {
      this.retryPostDataInterval = setInterval(() => {
        this.postData();
      }, 2000);
    }
  }

  private isAllDataLoaded() {
    return (this.isExpirationLoaded && this.isProductVersionLoaded
      && this.isConfigDataLoaded && this.isSummaryDataLoaded);
  }

  private getCurrentDateTime() {
    return (new Date).toISOString();
  }
}
