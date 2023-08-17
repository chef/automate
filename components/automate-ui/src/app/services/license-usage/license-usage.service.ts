import { Injectable } from '@angular/core';
import { ComplianceStatsService } from '../telemetry/compliance-stats/compliance-stats.service';
import { ClientRunsStatsService } from '../telemetry/client-runs-stats/client-runs-stats.service';
import { NodeUsageStats } from '../telemetry/client-runs-stats/client-runs-stats.model';
import { ApplicationUsageStats } from '../telemetry/application-stats/application-stats.model';
import { ApplicationStatsService } from '../telemetry/application-stats/application-stats.service';
import { ConfigService } from '../config/config.service';
import { LicenseStatus } from 'app/entities/license/license.model';
import { HttpClient } from '@angular/common/http';
import { environment as env } from '../../../environments/environment';

declare var postAnalyticsUsageData:any;

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
  complianceStatsSubscription: any;
  

  constructor(
    private complianceStatsService: ComplianceStatsService,
    private clientRunsStatsService: ClientRunsStatsService,
    private applicationStatsService : ApplicationStatsService,
    private configService: ConfigService,
    private http: HttpClient
  ) {}

  async postAnalyticsUsageDataCall() {


    this.http.get<LicenseStatus>(`${env.gateway_url}/license/status`).subscribe(data => {
      this.expiration = data.licensed_period.end
    })

    try {    
      await this.configService.getConfig().subscribe(data => {
      this.licenseId = data.licenseId,
      this.customerId = data.customerId,
      this.customerName = data.customerName
      })
    } catch (error) {
      console.log(error);
    }

    try {
      const complianceUsageStats: NodeUsageStats = await this.complianceStatsService
      .getComplianceStats();
      if (complianceUsageStats && Number(complianceUsageStats['days_since_last_post']) > 0) {
        this.totalNodes = complianceUsageStats['node_cnt']
        this.days_since_last_post = complianceUsageStats['days_since_last_post'];

        var start = new Date();
        start.setDate(start.getDate() - this.days_since_last_post);
        this.periodStartDate = start.toISOString()

        var end = new Date();
        end.setDate(end.getDate() - 1);
        this.periodEndDate = end.toISOString();
        
      }
    } catch (error) {
      console.log(error);
    }
    

    try {
      const nodeUsageStats: NodeUsageStats = await this.clientRunsStatsService
      .getClientRunsStats();
      if (nodeUsageStats && Number(nodeUsageStats['days_since_last_post']) > 0) {
        this.totalScans = nodeUsageStats['node_cnt'];
      }
    } catch (error) {
      console.log(error);
    } 

    try {
      const applicationUsageStats: ApplicationUsageStats = await this.applicationStatsService
        .getApplicationStats();
        if (applicationUsageStats && Number(applicationUsageStats['days_since_last_post']) > 0) {
          this.totalService = applicationUsageStats['total_services'];
        }
    } catch (error) {
      console.log(error);
    }
    
    this.sendData()
  }
  
  sendData(){
    const data = {
      "license_id": this.licenseId,
      "customerId": this.customerId,
      "expiration": this.expiration,
      "customerName": this.customerName,
      "periods": [{
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
              "service":{
                "targets": this.totalService
              }
          },
      }]
    };

    console.log(data)

    postAnalyticsUsageData(data)
  }

  private getCurrentDateTime() {
    return (new Date).toISOString();
  }
}
