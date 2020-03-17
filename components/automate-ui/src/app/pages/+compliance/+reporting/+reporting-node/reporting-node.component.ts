import { Component, OnInit, OnDestroy } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { StatsService } from '../../shared/reporting/stats.service';
import { Subject } from 'rxjs';
import { ReportQueryService } from '../../shared/reporting/report-query.service';
import * as moment from 'moment';
import { DateTime } from 'app/helpers/datetime/datetime';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { takeUntil } from 'rxjs/operators';

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

  openControls = {};

  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  constructor(
    private route: ActivatedRoute,
    private statsService: StatsService,
    private reportQuery: ReportQueryService,
    private layoutFacade: LayoutFacadeService
  ) {
  }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Compliance);
    this.reportLoading = true;
    this.layoutFacade.ShowPageLoading(true);
    const id: string = this.route.snapshot.params['id'];
    const reportQuery = this.reportQuery.getReportQuery();
    reportQuery.filters = reportQuery.filters.concat([{type: {name: 'node_id'}, value: {id}}]);

    this.statsService.getReports(reportQuery, {sort: 'latest_report.end_time', order: 'DESC'})
      .pipe(takeUntil(this.isDestroyed))
      .subscribe(reports => {
        this.reports = reports;
        const queryForReport = this.reportQuery.getReportQueryForReport(reports[0]);
        this.statsService.getSingleReport(reports[0].id, queryForReport)
          .pipe(takeUntil(this.isDestroyed))
          .subscribe(data => {
            this.reportLoading = false;
            this.layoutFacade.ShowPageLoading(false);
            this.activeReport = Object.assign(reports[0], data);
          });
      });
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  onReportItemClick(_event, report) {
    this.reportLoading = true;
    this.layoutFacade.ShowPageLoading(true);
    const reportQuery = this.reportQuery.getReportQueryForReport(report);
    this.statsService.getSingleReport(report.id, reportQuery)
      .pipe(takeUntil(this.isDestroyed))
      .subscribe(data => {
        this.reportLoading = false;
        this.layoutFacade.ShowPageLoading(false);
        this.activeReport = Object.assign(report, data);
      });
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
}
