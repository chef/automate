import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';

@Injectable({ providedIn: 'root'})
export class DownloadReportsService {
  count = 0;
  showOpenReport = false;
  reportList = [];
  private units = ['bytes', 'kb', 'mb', 'gb']; 
  constructor(private httpClient: HttpClient) {}

  onReportOpenClick() {
    this.handleReportList();
    this.showOpenReport = true;
  }

  onReportCloseClick() {
    this.showOpenReport = false;
  }

  handleReportList() {
    this.fetchReportList().subscribe((responseData)=> {
      if (responseData && responseData['data'] && responseData['data'].length > 0) {
        //this.reportList = responseData['data'];
        this.count++;
        if (this.count >= 6) {
          this.reportList = responseData['data'];
        } else this.reportList = this.getMockData();
        this.checkReportStatus();
      } else {
        this.reportList = [];
      }
      console.log(responseData);
    });
  }

  fetchReportList() {
    const url = 'https://a2-dev.test/api/v0/reportmanager/requests';
    return this.httpClient.get(url);
  }

  checkReportStatus() {
    const reportLength = this.reportList.length;
    if (reportLength > 0) {
      for(let i = 0;i < reportLength;i++) {
        if (this.reportList[i].status === 'running') {
          setTimeout(()=> {
            this.handleReportList();
          }, 10000);
          break;
        }
      }
    }
  }

  byteConverter(value) {
    value = value * 1; // convert string to number
    let sizeIndex = 0;
    let rem = value;
    do {
      if (rem >= 1024) {
        sizeIndex++;
      }
      rem = rem / 1024;
    } while(rem >= 1024);
    return rem.toFixed(2) + this.units[sizeIndex];
  }

  getMockData() {
    return [{
        "acknowledgement_id": "32f9a9c6-fe1f-456e-86de-6f5e308bab7e",
        "status": "success",
        "report_size": "5000",
        "err_message": "error in unmarshalling the report content: The specified key does not exist.",
        "created_at": "2021-11-23T05:39:30.406778Z",
        "ended_at": "2021-11-23T05:39:30.502190Z",
        "duration": "30m"
      }, {
        "acknowledgement_id": "70f5f8c6-de40-4d27-b20d-9fa57fec9292",
        "status": "success",
        "report_size": "1048576",
        "err_message": "error in unmarshalling the report content: The specified key does not exist.",
        "created_at": "2021-11-23T05:39:27.244057Z",
        "ended_at": "2021-11-23T05:39:27.395335Z",
        "duration": "5s"
      }, {
        "acknowledgement_id": "558b1a8b-8643-4f6c-85b3-d493e6403bdb",
        "status": "running",
        "report_size": "0",
        "err_message": "error in unmarshalling the report content: The specified key does not exist.",
        "created_at": "2021-11-23T05:39:24.450684Z",
        "ended_at": "2021-11-23T05:39:24.623221Z"
      }, {
        "acknowledgement_id": "0f32c7cb-58c9-4025-94bd-fa7cfacf6c0e",
        "status": "failed",
        "report_size": "0",
        "err_message": "error in unmarshalling the report content: The specified key does not exist.",
        "created_at": "2021-11-23T05:39:20.371624Z",
        "ended_at": "2021-11-23T05:39:20.690904Z",
        "duration": "10s"
      }, {
        "acknowledgement_id": "002cf207-8cbf-4420-83a1-3913eae23bb5",
        "status": "running",
        "report_size": "0",
        "err_message": "error in unmarshalling the report content: The specified key does not exist.",
        "created_at": "2021-11-22T12:17:54.373470Z",
        "ended_at": "2021-11-22T12:17:54.577124Z"
      }, {
        "acknowledgement_id": "a8822729-525e-4e91-8007-1cde93d0d14e",
        "status": "failed",
        "report_size": "0",
        "err_message": "error in unmarshalling the report content: The specified key does not exist.",
        "created_at": "2021-11-22T12:17:45.449732Z",
        "ended_at": "2021-11-22T12:17:45.762517Z",
        "duration": "1h"
      }];
  }

  getMockData1() {
    return [{
        "acknowledgement_id": "32f9a9c6-fe1f-456e-86de-6f5e308bab7e",
        "status": "success",
        "report_size": "5000",
        "err_message": "error in unmarshalling the report content: The specified key does not exist.",
        "created_at": "2021-11-23T05:39:30.406778Z",
        "ended_at": "2021-11-23T05:39:30.502190Z"
      }, {
        "acknowledgement_id": "70f5f8c6-de40-4d27-b20d-9fa57fec9292",
        "status": "success",
        "report_size": "1048576",
        "err_message": "error in unmarshalling the report content: The specified key does not exist.",
        "created_at": "2021-11-23T05:39:27.244057Z",
        "ended_at": "2021-11-23T05:39:27.395335Z"
      }, {
        "acknowledgement_id": "558b1a8b-8643-4f6c-85b3-d493e6403bdb",
        "status": "running",
        "report_size": "0",
        "err_message": "error in unmarshalling the report content: The specified key does not exist.",
        "created_at": "2021-11-23T05:39:24.450684Z",
        "ended_at": "2021-11-23T05:39:24.623221Z"
      },  {
        "acknowledgement_id": "002cf207-8cbf-4420-83a1-3913eae23bb5",
        "status": "running",
        "report_size": "0",
        "err_message": "error in unmarshalling the report content: The specified key does not exist.",
        "created_at": "2021-11-22T12:17:54.373470Z",
        "ended_at": "2021-11-22T12:17:54.577124Z"
      }];
  }

}
