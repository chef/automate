import { Injectable, OnDestroy } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment } from '../../../environments/environment';
import { ReportType } from './download-reports.model';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';
import { downloadNotificationList } from './download-reports.selector';
import { ClearNotificationReport } from './download-reports.actions';
import { Subscription } from 'rxjs';
import { saveAs } from 'file-saver';

const REPORT_LIST_API_URL = environment.download_report_list_url;

@Injectable({ providedIn: 'root'})
export class DownloadReportsService implements OnDestroy {
  showOpenReport = false;
  reportList = [];
  isLongPollRunning = false;
  downloadSubscription: Subscription;
  reportListSubscription: Subscription;
  notificationItems = {};
  retryLongPoll = 0;
  private units = ['bytes', 'kb', 'mb', 'gb'];
  constructor(private httpClient: HttpClient, 
    private store: Store<NgrxStateAtom>) {
      this.onInit();
    }

  onInit() {
    this.downloadSubscription = this.store.select(downloadNotificationList).subscribe((notificationItems) => {
      this.notificationItems = notificationItems; // download ack_id list are read from store
    });
  }  

  onReportOpenClick() {
    this.showOpenReport = true;
    if (this.isLongPollRunning) {
      return;
    }
    this.initiateLongPolling();
  }

  onReportCloseClick() {
    this.showOpenReport = false;
  }

  initiateLongPolling() {
    this.isLongPollRunning = true;
    this.retryLongPoll = 0;
    this.handleReportList();
  }

  handleReportList() {
    this.retryLongPoll = this.retryLongPoll + 1;
    if (this.reportListSubscription) {
      this.reportListSubscription.unsubscribe();
    }
    this.reportListSubscription = this.fetchReportList().subscribe((responseData) => {
      if (responseData && responseData['data'] && responseData['data'].length > 0) {
        this.reportList = responseData['data'];
        this.checkReportStatus();
      } else {
        this.reportList = [];
        this.isLongPollRunning = false;
      }
    }, (error) => {
      console.log(error);
      if (this.retryLongPoll > 5) {
        return;
      }
      this.handleReportList();
    });
  }

  fetchReportList() {
    const url = `${REPORT_LIST_API_URL}/requests`;
    return this.httpClient.get(url);
  }

  checkReportStatus() {
    const reportLength = this.reportList.length;
    if (reportLength > 0) {
      let isLongPollNeededNextTime = false;
      for (let report of this.reportList) {
        if (report.status === 'running') {
          isLongPollNeededNextTime = true;
        } else {
          const status = report.status;
          const format = report.report_type.toUpperCase(); 
          if (status === 'success') {
            if (this.notificationItems['ack_' + report.acknowledgement_id]) {
              this.store.dispatch(new ClearNotificationReport(report.acknowledgement_id));
              this.store.dispatch(new CreateNotification({
                type: Type.info,
                message: format + ' report is ready for download.'
              }));
            }
          } else if (status === 'failed') {
            if (this.notificationItems['ack_' + report.acknowledgement_id]) {
              this.store.dispatch(new ClearNotificationReport(report.acknowledgement_id));
              this.store.dispatch(new CreateNotification({
                type: Type.error,
                message: format + ' report is failed for download.'
              }));
            }
          }
        }
      }
      if (isLongPollNeededNextTime) {
        setTimeout(() => {
          this.handleReportList();
        }, 10000);
        return;
      } 
    }
    this.isLongPollRunning = false;
  }

  onLinkToDownload(report: ReportType) {
    const format = report.report_type;
    const filename = report.created_at;
    this.downloadReport(report.acknowledgement_id).subscribe((data) => {
      console.log(data);
      const types = { 'json': 'application/json', 'csv': 'text/csv' };
      const type = types[format];
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
    let url = `${REPORT_LIST_API_URL}/export`;
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

  ngOnDestroy() {
    if (this.downloadSubscription) {
      this.downloadSubscription.unsubscribe();
    }
  }

}
