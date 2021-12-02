import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../environments/environment';

const REPORT_LIST_API_URL = environment.download_report_list_url;

@Injectable({ providedIn: 'root'})
export class DownloadReportsService {
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
    this.fetchReportList().subscribe((responseData) => {
      if (responseData && responseData['data'] && responseData['data'].length > 0) {
        this.reportList = responseData['data'];
        this.checkReportStatus();
      } else {
        this.reportList = [];
      }
    });
  }

  fetchReportList() {
    const url = `${REPORT_LIST_API_URL}/requests`;
    return this.httpClient.get(url);
  }

  checkReportStatus() {
    const reportLength = this.reportList.length;
    if (reportLength > 0) {
      for (let i = 0; i < reportLength; i++) {
        if (this.reportList[i].status === 'running') {
          setTimeout(() => {
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
    if (rem === 0) {
      return 0;
    }
    do {
      if (rem >= 1024) {
        sizeIndex++;
      }
      rem = rem / 1024;
    } while (rem >= 1024);
    if (Math.floor(rem) === rem) {
      return rem + this.units[sizeIndex];
    }
    return rem.toFixed(2) + this.units[sizeIndex];
  }
}
