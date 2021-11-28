import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { saveAs } from 'file-saver';
import { environment } from '../../../environments/environment';
import { ReportType } from './download-reports.model';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

const REPORT_LIST_API_URL = environment.download_report_list_url;
const CC_API_URL = environment.compliance_url;

@Injectable({ providedIn: 'root'})
export class DownloadReportsService {
  showOpenReport = false;
  reportList = [];
  private units = ['bytes', 'kb', 'mb', 'gb'];
  constructor(private httpClient: HttpClient, 
    private store: Store<NgrxStateAtom>) {}

  onReportOpenClick() {
    this.handleReportList();
    this.showOpenReport = true;
  }

  onReportCloseClick() {
    this.showOpenReport = false;
  }

  handleReportList() {
    this.fetchReportList().subscribe((responseData) => {
      if (responseData && responseData['data'] && responseData['data'].length == 0) {
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

  onLinkToDownload(report: ReportType) {
    const filename = ''; // need to implement
    const format = 'json' || 'csv'; // need to implement

    this.downloadReport(report.acknowledgement_id).subscribe((data) => {
      console.log(data);
      const types = { 'json': 'application/json', 'csv': 'text/csv' };
      const type = types[format]; // use report.format
      const blob = new Blob([data], { type });
      saveAs(blob, filename);
    }, (error) => {
      console.log(error);
      this.store.dispatch(new CreateNotification({
        type: Type.error,
        message: 'Download failed.'
        }));
    });
  }

  downloadReport(ack_id: string): Observable<string> {
    console.log(ack_id);
    let url = `${CC_API_URL}/reporting/export`;
    url = url + '/' + ack_id;
    return this.httpClient.get<string>(url);
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
