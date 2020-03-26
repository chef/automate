import { Component, OnInit, OnDestroy } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { DateTime } from 'app/helpers/datetime/datetime';

import { ReportQueryService, ReportDataService, ReportQuery } from '../../shared/reporting';

@Component({
  selector: 'app-reporting-controls',
  templateUrl: './reporting-controls.component.html',
  styleUrls: [ './reporting-controls.component.scss' ]
})
export class ReportingControlsComponent implements OnInit, OnDestroy {

  public Datetime = DateTime;
  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  constructor(
    public reportQuery: ReportQueryService,
    public reportData: ReportDataService,
    private router: Router,
    private route: ActivatedRoute
  ) {}

  ngOnInit() {
    this.reportQuery.state.pipe(
      takeUntil(this.isDestroyed))
      .subscribe(this.getData.bind(this));
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  getData(reportQuery: ReportQuery) {
    this.reportData.getReportingControlsList(reportQuery);
  }

  impactStatus({ impact }) {
    if (impact >= 0.7) { return 'critical'; }
    if (impact >= 0.4) { return 'major'; }
    return 'minor';
  }

  filterFor(control) {
    return this.reportQuery.getReportQuery().filters.filter(f => {
      return f.type && f.type.name === 'control_id' && f.value.id === control.id;
    })[0];
  }

  hasFilter(control) {
    return this.filterFor(control) !== undefined;
  }

  addFilter(control) {
    const controlTypeName = 'control_id';
    const profileTypeName = 'profile_id';
    const {queryParamMap} = this.route.snapshot;
    const queryParams = {...this.route.snapshot.queryParams};

    queryParams[profileTypeName] = queryParamMap.getAll(profileTypeName)
      .filter(v => v !== control.profile.id).concat(control.profile.id);
    queryParams[controlTypeName] = queryParamMap.getAll(controlTypeName)
      .filter(v => v !== control.id).concat(control.id);

    this.reportQuery.setFilterTitle(controlTypeName, control.id, control.title);

    this.router.navigate([], {queryParams});
  }

  removeFilter(control) {
    const filter = this.filterFor(control);
    const typeName = filter.type.name;

    const {queryParamMap} = this.route.snapshot;
    const queryParams = {...this.route.snapshot.queryParams};
    const values = queryParamMap.getAll(typeName).filter(v => v !== filter.value.id);

    if (values.length === 0) {
      delete queryParams[typeName];
    } else {
      queryParams[typeName] = values;
    }

    this.router.navigate([], {queryParams});
  }
}
