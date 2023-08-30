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
  private days_since_last_post;
  private periodStartDate;
  private periodEndDate;
  private deploymentId;
  private deploymentType;
  private productVersion;
  private payload;

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
    })

    this.http.get(`${env.gateway_url}/version`).subscribe(data => {
      this.productVersion = data['build_timestamp'];
    })

    this.configService.getConfig().subscribe(data => {
      this.deploymentId = data.deploymentId;
      this.deploymentType = data.deploymentType;
      this.licenseId = data.licenseId;
      this.customerId = data.customerId;
      this.customerName = data.customerName;
    })

    const complianceUsageStats = await this.complianceStatsService.getComplianceStats();

    if (complianceUsageStats && Number(complianceUsageStats['days_since_last_post']) > 0) {
      this.totalNodes = complianceUsageStats['node_cnt']
      this.days_since_last_post = complianceUsageStats['days_since_last_post'];

      let start = new Date();
      start.setDate(start.getDate() - this.days_since_last_post);
      start.setHours(0,0,0,0);
      this.periodStartDate = start.toISOString()

      let end = new Date();
      end.setHours(11,59,0,0);
      end.setDate(end.getDate() - 1);
      this.periodEndDate = end.toISOString();
    }

    const nodeUsageStats = await this.clientRunsStatsService.getClientRunsStats();
    if (nodeUsageStats && Number(nodeUsageStats['days_since_last_post']) > 0) {
      this.totalScans = nodeUsageStats['node_cnt'];
    }

    const applicationUsageStats = await this.applicationStatsService.getApplicationStats();
    if (applicationUsageStats && Number(applicationUsageStats['days_since_last_post']) > 0) {
      this.totalService = applicationUsageStats['total_services'];
    }
    this.sendData()
  }

  sendData() {
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
    try {
      if(postAnalyticsUsageData != null || postAnalyticsUsageData != undefined) {
        postAnalyticsUsageData(this.payload)
      } 
    } catch(error){
      console.log("First attempt to push data failed");
    }
  }

  // pushData function called from app component
  pushData(){
    if(this.payload){
      if(postAnalyticsUsageData != null || postAnalyticsUsageData != undefined) {
        postAnalyticsUsageData(this.payload)
      } 
    }
  }

  private getCurrentDateTime() {
    return (new Date).toISOString();
  }
}
