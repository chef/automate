import { Component, OnInit, OnDestroy } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { StatsService, ReportCollection, reportFormat } from '../../shared/reporting/stats.service';
import { Subject, Observable } from 'rxjs';
import { ReportQueryService, ReturnParams, ReportQuery } from '../../shared/reporting/report-query.service';
import * as moment from 'moment/moment';
import { DateTime } from 'app/helpers/datetime/datetime';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { takeUntil, first, finalize } from 'rxjs/operators';
import { saveAs } from 'file-saver';

@Component({
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

  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  constructor(
    private route: ActivatedRoute,
    private statsService: StatsService,
    private reportQueryService: ReportQueryService,
    private layoutFacade: LayoutFacadeService
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

    this.onPageChanged(1);
  }

  onPageChanged(page: number) {
    this.page$.next(page);
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  onReportItemClick(_event, report) {
    this.reportLoading = true;
    this.layoutFacade.ShowPageLoading(true);

    this.setActiveReport(report);
  }

  onHistoryOpenClick(_event) {
    this.showScanHistory = true;
  }

  onHistoryCloseClick(_event) {
    this.showScanHistory = false;
  }

  onHistoryFilterClick(_event, status) {
    this.activeReportStatus = status;
  }

  onFilterControlStatusClick(_event, status) {
    this.activeStatusFilter = status;
  }

  onViewSourceClick(_event, control) {
    control.showMetaData = !control.showMetaData;
  }

  filteredReports(reports, status) {
    if (status === 'all') {
      return reports;
    }

    return reports.filter(r => r.status === status);
  }

  filteredProfiles(profiles, status) {
    return profiles.filter(p => {
      return this.filteredControls(p.controls, status).length > 0;
    });
  }

  filteredControls(controls, status) {
    return controls.filter(c => status === 'all' || c.status === status);
  }

  profilesByStatus(profiles, status) {
    return profiles.filter(p => p.status === status);
  }

  isHistoryFilterSelected(status): boolean {
    return this.activeReportStatus === status;
  }

  isReportSelected(report): boolean {
    return this.activeReport.id === report.id;
  }

  isControlStatusSelected(status): boolean {
    return this.activeStatusFilter === status;
  }

  isOpenControl({ id }) {
    return this.openControls[id] && this.openControls[id].open;
  }

  toggleControl(control) {
    const state = this.openControls[control.id];
    const toggled = state ? ({...state, open: !state.open}) : ({open: true, pane: 'results'});
    this.openControls[control.id] = toggled;
  }

  openControlPane(control) {
    return this.openControls[control.id].pane;
  }

  showControlPane(control, pane) {
    this.openControls[control.id].pane = pane;
  }

  statusIcon(status) {
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

  formatDuration(duration) {
    const d = moment.duration(duration);
    return d.humanize();
  }

  formatDaysAgo(timestamp) {
    return moment(timestamp).fromNow();
  }

  private getReports(reportQuery: ReportQuery): Observable<ReportCollection>  {
    return this.statsService.getReportsWithPages(reportQuery,
      {sort: 'latest_report.end_time', order: 'DESC'}, this.page, this.pageSize);
  }

  private setActiveReport(report: any) {
    const reportQuery = this.reportQueryService.getReportQueryForReport(report);
    this.statsService.getSingleReport(report.id, reportQuery)
      .pipe(first())
      .subscribe(data => {
        this.reportLoading = false;
        this.layoutFacade.ShowPageLoading(false);
        this.activeReport = Object.assign(report, data);
      });
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
    const onError = _e => this.downloadFailed = true;
    const types = { 'json': 'application/json', 'csv': 'text/csv' };
    const onNext = data => {
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
}
