import { Component, OnInit, OnDestroy } from '@angular/core';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from '../../../../ngrx.reducers';
import { EntityStatus } from '../../../../entities/entities';
import { isNil } from 'lodash/fp';
import { ActivatedRoute } from '@angular/router';
import { StatsService, ReportCollection, reportFormat } from '../../shared/reporting/stats.service';
import { combineLatest, Subject, Observable } from 'rxjs';
import { ReportQueryService, ReturnParams, ReportQuery } from '../../shared/reporting/report-query.service';
import moment from 'moment';
import { DateTime } from '../../../../helpers/datetime/datetime';
import { LayoutFacadeService, Sidebar } from '../../../../entities/layout/layout.facade';
import { takeUntil, first, finalize } from 'rxjs/operators';
import { saveAs } from 'file-saver';
import { GetControlDetail } from '../../../../entities/control-details/control-details.action';
import { controlDetailStatus, controlDetailList, controlsList } from '../../../../entities/control-details/control-details.selectors';

@Component({
  standalone: false,
  selector: 'app-reporting-node',
  templateUrl: './reporting-node.component.html',
  styleUrls: ['./reporting-node.component.scss']
})

export class ReportingNodeComponent implements OnInit, OnDestroy {

  reports = [];
  activeReport: any = {};
  activeStatusFilter = 'all';
  activeReportStatus = 'all';
  showScanHistory = false;
  reportLoading = false;
  RFC2822 = DateTime.RFC2822;
  returnParams: ReturnParams = {};
  page$ = new Subject<number>();
  page = 1;
  pageSize = 10;
  totalReports = 0;
  firstReportIsLoaded = false;
  downloadList: Array<string> = [];
  downloadOptsVisible = false;
  downloadStatusVisible = false;
  downloadInProgress = false;
  downloadFailed = false;
  openControls = {};
  controlList: any = {};
  pageIndex = 1;
  perPage = 100;
  controlsLoading = false;
  controlDetails = {};
  controlDetailsLoading = false;
  isError = false;
  reportId: string;
  reportIdArray: Array<string | number> = [];
  allControlList = [];
  index: number;

  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  constructor(
    private route: ActivatedRoute,
    private statsService: StatsService,
    private reportQueryService: ReportQueryService,
    private layoutFacade: LayoutFacadeService,
    private store: Store<NgrxStateAtom>
  ) {
  }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Compliance);
    this.reportLoading = true;
    this.layoutFacade.ShowPageLoading(true);
    this.returnParams = this.reportQueryService.formatReturnParams();
    const id: string = this.route.snapshot.params['id'];
    const reportQuery = this.reportQueryService.getReportQuery();
    reportQuery.filters = reportQuery.filters.concat([{type: {name: 'node_id'}, value: {id}}]);

    this.page$.pipe(
      takeUntil(this.isDestroyed)).
      subscribe(page => {
      this.page = page;
      this.getReports(reportQuery).pipe(first()).subscribe(reportCollection => {
        this.totalReports = reportCollection.totalReports;
        this.reports = reportCollection.reports;

        // load the first report only when first loading the page
        if (!this.firstReportIsLoaded) {
          this.firstReportIsLoaded = true;
          this.setActiveReport(this.reports[0]);
        }
      });
    });

    combineLatest([
      this.store.select(controlDetailStatus),
      this.store.select(controlDetailList)
    ]).pipe(takeUntil(this.isDestroyed))
    .subscribe(([detailsStatusSt, detailsListState]) => {
      if (detailsStatusSt === EntityStatus.loadingSuccess && !isNil(detailsListState)
      && this.controlList && this.controlList.control_elements) {
        this.controlList.control_elements[this.index].controlDetailsLoading = false;
        this.isError = false;
        this.controlDetails = detailsListState;
        const res = this.controlDetails['profiles'][0].controls[0].results;
        const code = this.controlDetails['profiles'][0].controls[0].code;
        const desc = this.controlDetails['profiles'][0].controls[0].desc;
        const waiver_data = this.controlDetails['profiles'][0].controls[0].waiver_data;
        this.controlList.control_elements[this.index].result = res;
        this.controlList.control_elements[this.index].code = code;
        this.controlList.control_elements[this.index].desc = desc;
        this.controlList.control_elements[this.index].waiver_data = waiver_data;
      } else if (detailsStatusSt === EntityStatus.loadingFailure) {
        this.isError = true;
        this.reportIdArray = this.reportIdArray.slice(0, -1);
      }
    });

    this.onPageChanged(1);
  }

  onPageChanged(page: number | any) {
    this.page$.next(page);
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  onReportItemClick(_event: any, report: any) {
    this.reportLoading = true;
    this.layoutFacade.ShowPageLoading(true);

    this.setActiveReport(report);
  }

  onHistoryOpenClick(_event: any) {
    this.showScanHistory = true;
  }

  onHistoryCloseClick(_event: any) {
    this.showScanHistory = false;
  }

  onHistoryFilterClick(_event: any, status: string) {
    this.activeReportStatus = status;
  }

  onFilterControlStatusClick(_event: any, status: string) {
    this.pageIndex = 1;
    this.activeStatusFilter = status;
    this.openControls = {};
    this.getControlData(this.activeReport);
  }

  onViewSourceClick(_event: any, control: { showMetaData: boolean; }) {
    control.showMetaData = !control.showMetaData;
  }

  filteredReports(reports: any[], status: string) {
    if (status === 'all') {
      return reports;
    }

    return reports.filter((r: { status: any; }) => r.status === status);
  }

  filteredProfiles(profiles: any[], status: string) {
    return profiles.filter((p: { controls: any; }) => {
      return this.filteredControls(p.controls, status).length > 0;
    });
  }

  filteredControls(controls: any[], status: string) {
    return controls.filter((c: { status: any; }) => status === 'all' || c.status === status);
  }

  profilesByStatus(profiles: any[], status: string) {
    return profiles.filter((p: { status: any; }) => p.status === status);
  }

  isHistoryFilterSelected(status: string): boolean {
    return this.activeReportStatus === status;
  }

  isReportSelected(report: { id: any; }): boolean {
    return this.activeReport.id === report.id;
  }

  isControlStatusSelected(status: string): boolean {
    return this.activeStatusFilter === status;
  }

  isOpenControl(control: { id: string | number, profile_id: string; }) {
    const key = this.toggleKey(control);
    return this.openControls[key] && this.openControls[key].open;
  }

  toggleControl(i: number, ctrl: any) {
    const control = ctrl;
    this.index = i;
    const key = this.toggleKey(control);
    this.controlList.control_elements[this.index].controlDetailsLoading = true;
    const state = this.openControls[key];
    const toggled = state ? ({...state, open: !state.open}) : ({open: true, pane: 'results'});
    this.openControls[key] = toggled;

    // to fetch the past control details we need to pass the start and end time
    const start_time = this.activeReport.end_time;
    const end_time = moment(start_time).endOf('day').toISOString();
    if (toggled.open === true) {
      if (!this.reportIdArray.includes(key)) {
        this.reportIdArray.push(key);
        const payload = {
          report_id : this.activeReport.id,
          filters : [
            {'type': 'start_time', 'values': [start_time]},
            {'type': 'end_time', 'values': [end_time]},
            {'type': 'profile_id', 'values': [`${control.profile_id}`]},
            {'type': 'control', 'values': [`${control.id}`]}]
        };
        this.store.dispatch(new GetControlDetail(payload));
      } else {
        this.store.select(controlsList).subscribe(data => {
          this.allControlList = data;
        });

        this.allControlList.forEach((data) => {
          data.profiles.forEach(p => {
            p.controls.forEach(c => {
              if (c.id === control.id && p.sha256 === control.profile_id) {
                this.controlList.control_elements[this.index].controlDetailsLoading = false;
                this.controlDetails = data;
                const res = this.controlDetails['profiles'][0].controls[0].results;
                const code = this.controlDetails['profiles'][0].controls[0].code;
                const desc = this.controlDetails['profiles'][0].controls[0].desc;
                const waiver_data = this.controlDetails['profiles'][0].controls[0].waiver_data;
                this.controlList.control_elements[this.index].result = res;
                this.controlList.control_elements[this.index].code = code;
                this.controlList.control_elements[this.index].desc = desc;
                this.controlList.control_elements[this.index].waiver_data = waiver_data;
              }
            });
          });
        });
      }
    } else {
      this.controlList.control_elements[this.index].controlDetailsLoading = false;
    }
  }

  toggleKey(control: any) {
    const key = control.id + ' ' + control.profile_id;
    return key;
  }

  openControlPane(control: { id: string | number, profile_id: string; }) {
    const key = this.toggleKey(control);
    return this.openControls[key].pane;
  }

  showControlPane(control: { id: string | number, profile_id: string; }, pane: any ) {
    const key = this.toggleKey(control);
    this.openControls[key].pane = pane;
  }

  statusIcon(status: any) {
    switch (status) {
      case ('failed'):
        return 'report_problem';
      case ('passed'):
        return 'check_circle';
      case ('skipped'):
        return 'help';
      default:
        return '';
    }
  }

  impactStatus({impact}) {
    if (impact >= 0.7) {
      return 'critical';
    }
    if (impact >= 0.4) {
      return 'major';
    }
    return 'minor';
  }

  formatDuration(duration: moment.DurationInputArg1) {
    const d = moment.duration(duration);
    return d.humanize();
  }

  formatDaysAgo(timestamp: moment.MomentInput) {
    return moment(timestamp).fromNow();
  }

  private getReports(reportQuery: ReportQuery): Observable<ReportCollection>  {
    return this.statsService.getReportsWithPages(reportQuery,
      {sort: 'latest_report.end_time', order: 'DESC'}, this.page, this.pageSize);
  }

  private setActiveReport(report: any) {
    const reportQuery = this.reportQueryService.getReportQueryForReport(report);
    this.statsService.getNodeHeader(report.id, reportQuery)
      .pipe(first())
      .subscribe(data => {
        this.reportLoading = false;
        this.layoutFacade.ShowPageLoading(false);
        this.activeReport = Object.assign(report, data);
      });

    this.getControlData(report);
  }

  getFilterCount(report: any) {
    if (!(report && (typeof report === 'object') && report.controls)) {
      return 0;
    }
    switch (this.activeStatusFilter) {
      case ('failed'):
        return report.controls.failed.total;
      case ('passed'):
        return report.controls.passed.total;
      case ('skipped'):
        return report.controls.skipped.total;
      case ('waived'):
        return report.controls.waived.total;
      default:
        return report.controls.total;
    }
  }
  getControlData(report: any) {
    const filterCount = this.getFilterCount(report);
    if (filterCount > 0) {
      this.controlsLoading = true;
      const reportQuery = this.reportQueryService.getReportQueryForReport(report);
      this.statsService.getControlsList(report.id, reportQuery, this.pageIndex,
        this.perPage, this.activeStatusFilter)
      .pipe(first())
      .subscribe(data => {
        this.controlsLoading = false;
        this.layoutFacade.ShowPageLoading(false);
        if (this.pageIndex === 1) {
          this.controlList = Object.assign(data);
        } else {
          this.controlList.control_elements.push(...data.control_elements);
        }
        this.pageIndex++;
        this.controlsLoading = false;
      });
    } else {
      this.controlList = {};
    }
  }

  toggleDownloadDropdown() {
    this.downloadOptsVisible = !this.downloadOptsVisible;
  }
  hideDownloadStatus() {
    this.downloadStatusVisible = false;
    this.downloadInProgress = false;
    this.downloadFailed = false;
  }

  private showDownloadStatus() {
    this.downloadStatusVisible = true;
    this.downloadInProgress = true;
    this.downloadFailed = false;
  }

  onDownloadNodeReport(fileFormat: reportFormat) {
    this.downloadOptsVisible = false;
    const id: string = this.route.snapshot.params['id'];
    const reportQuery = this.reportQueryService.getReportQuery();
    reportQuery.filters = reportQuery.filters.concat([{type: {name: 'node_id'}, value: {id}}]);
    const filename = `${reportQuery.endDate.format('YYYY-M-D')}.${fileFormat}`;

    const onComplete = () => this.downloadInProgress = false;
    const onError = (_e: any) => this.downloadFailed = true;
    const types = { 'json': 'application/json', 'csv': 'text/csv' };
    const onNext = (data: BlobPart) => {
      const type = types[fileFormat];
      const blob = new Blob([data], { type });
      saveAs(blob, filename);
      this.hideDownloadStatus();
    };

    this.downloadList = [filename];
    this.showDownloadStatus();
    this.statsService.downloadNodeReport(fileFormat, reportQuery).pipe(
      finalize(onComplete))
      .subscribe(onNext, onError);
  }

  onScrollDown() {
    this.getControlData(this.activeReport);
  }
}
