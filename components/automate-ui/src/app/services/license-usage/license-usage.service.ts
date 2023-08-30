import { Injectable } from '@angular/core';
import { ComplianceStatsService } from '../telemetry/compliance-stats/compliance-stats.service';
import { ClientRunsStatsService } from '../telemetry/client-runs-stats/client-runs-stats.service';
import { ApplicationStatsService } from '../telemetry/application-stats/application-stats.service';
import { ConfigService } from '../config/config.service';
import { LicenseStatus } from 'app/entities/license/license.model';
import { HttpClient } from '@angular/common/http';
import { environment as env } from '../../../environments/environment';

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
  private expiration;
  private daysSinceLasPost;
  private periodStartDate;
  private periodEndDate;
  private deploymentId;
  private deploymentType;
  private productVersion;
  private payload;
  private retryPostDataInterval;

  private isLicenseIdLoaded = false;
  private isCustomerIdLoaded = false;
  private isCustomerNameLoaded = false;
  private isTotalNodesLoaded = false;
  private isTotalScansLoaded = false;
  private isTotalServiceLoaded = false;
  private isExpirationLoaded = false;
  private isDaysSinceLasPostLoaded = false;
  private isPeriodStartDateLoaded = false;
  private isPeriodEndDateLoaded = false;
  private isDeploymentIdLoaded = false;
  private isDeploymentTypeLoaded = false;
  private isProductVersionLoaded = false;

  constructor(
    private complianceStatsService: ComplianceStatsService,
    private clientRunsStatsService: ClientRunsStatsService,
    private applicationStatsService: ApplicationStatsService,
    private configService: ConfigService,
    private http: HttpClient
  ) {}

  async postAnalyticsUsageDataCall() {

    this.http.get<LicenseStatus>(`${env.gateway_url}/license/status`).subscribe(data => {
      this.expiration = data.licensed_period.end;
      this.isExpirationLoaded = true;
    })

    this.http.get(`${env.gateway_url}/version`).subscribe(data => {
      this.productVersion = data['build_timestamp'];
      this.isProductVersionLoaded = true;
    })

    this.configService.getConfig().subscribe(data => {
      this.deploymentId = data.deploymentId;
      this.deploymentType = data.deploymentType;
      this.licenseId = data.licenseId;
      this.customerId = data.customerId;
      this.customerName = data.customerName;

      this.isDeploymentIdLoaded = true;
      this.isDeploymentTypeLoaded = true;
      this.isLicenseIdLoaded = true;
      this.isCustomerIdLoaded = true;
      this.isCustomerNameLoaded = true;
    })

    const complianceUsageStats = await this.complianceStatsService.getComplianceStats();
    this.totalNodes = complianceUsageStats['node_cnt']
    this.daysSinceLasPost = complianceUsageStats['days_since_last_post'];

    let start = new Date();
    start.setDate(start.getDate() - this.daysSinceLasPost);
    start.setHours(0,0,0,0);
    this.periodStartDate = start.toISOString()

    let end = new Date();
    end.setHours(11,59,0,0);
    end.setDate(end.getDate() - 1);
    this.periodEndDate = end.toISOString();

    this.isTotalNodesLoaded = true;
    this.isDaysSinceLasPostLoaded = true;
    this.isPeriodStartDateLoaded = true;
    this.isPeriodEndDateLoaded = true;


    const nodeUsageStats = await this.clientRunsStatsService.getClientRunsStats();
    this.totalScans = nodeUsageStats['node_cnt'];
    this.isTotalScansLoaded = true;

    const applicationUsageStats = await this.applicationStatsService.getApplicationStats();
    this.totalService = applicationUsageStats['total_services'];
    this.isTotalServiceLoaded = true;
  }

  constructPayload() {
    const data = {
      "license_id": this.licenseId,
      "customerId": this.customerId,
      "expiration": this.expiration,
      "customerName": this.customerName,
      "metaData": {
        "Automate": {
             "instanceId":this.deploymentId,
             "deploymentType":this.deploymentType,
        }
      },
      "periods": [{
        "version": this.productVersion,
        "date": this.getCurrentDateTime(),
        "period": {
          "start": this.periodStartDate,
          "end": this.periodEndDate
        },
        "summary": {
          "nodes": {
            "total": this.totalNodes
          },
          "scans": {
            "targets": this.totalScans,
          },
          "service": {
            "targets": this.totalService
          }
        }
      }],
      "source": "Automate",
      "scannerVersion": "0.1.0",
      "scannedOn": this.getCurrentDateTime()
    };
    this.payload = data;
  }

  postData() {
    clearInterval(this.retryPostDataInterval);
    if(this.isAllDataLoaded()) {
      this.constructPayload()
      if(postAnalyticsUsageData != null || postAnalyticsUsageData != undefined) {
        postAnalyticsUsageData(this.payload)
      }
    }else {
      this.retryPostDataInterval = setInterval(() => {
        this.postData();
      }, 2000);
    }
  }

  private isAllDataLoaded() {
    return (this.isLicenseIdLoaded && this.isCustomerIdLoaded && this.isExpirationLoaded 
      && this.isTotalNodesLoaded && this.isTotalScansLoaded && this.isDaysSinceLasPostLoaded
      && this.isPeriodStartDateLoaded && this.isPeriodEndDateLoaded && this.isDeploymentIdLoaded
      && this.isTotalServiceLoaded && this.isDeploymentTypeLoaded && this.isCustomerNameLoaded 
      && this.isProductVersionLoaded);
  }

  private getCurrentDateTime() {
    return (new Date).toISOString();
  }
}
