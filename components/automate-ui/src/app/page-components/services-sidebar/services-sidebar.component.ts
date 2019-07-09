import { Component, OnDestroy, OnInit, Input } from '@angular/core';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import * as moment from 'moment';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { HttpErrorResponse } from '@angular/common/http';
import { Observable, Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import {
  servicesStatus,
  serviceGroupState,
  servicesErrorResp
} from '../../entities/service-groups/service-groups.selector';
import { createSelector } from '@ngrx/store';
import { EntityStatus } from '../../entities/entities';
import {
  Service, ServicesFilters, HealthSummary
} from '../../entities/service-groups/service-groups.model';
import { includes, getOr } from 'lodash/fp';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

@Component({
  selector: 'app-services-sidebar',
  templateUrl: './services-sidebar.component.html',
  styleUrls: ['./services-sidebar.component.scss']
})

export class ServicesSidebarComponent implements OnInit, OnDestroy {
  @Input() serviceGroupId: string;
  @Input() visible: boolean;

  // RFC2822 format like: Wed, 03 Jul 2019 17:08:53 UTC
  //
  // TODO @afiune we should move this to a common place where other
  // components can usee this time format
  readonly RFC2822 = 'ddd, DD MMM YYYY, hh:mm:ss [UTC]';

  public services$: Observable<Service[]>;
  public servicesStatus$: Observable<EntityStatus>;
  public servicesError$: Observable<HttpErrorResponse>;
  public serviceGroupName$: Observable<string>;
  public selectedHealth = 'total';
  public currentPage = 1;
  public pageSize = 25;
  public totalServices = 0;
  public servicesHealthSummary: HealthSummary;

  // The collection of allowable status
  private allowedStatus = ['ok', 'critical', 'warning', 'unknown'];
  private svcHealthSummary$: Observable<HealthSummary>;
  private currentServicesFilters$: Observable<ServicesFilters>;

  // Has this component been destroyed
  private isDestroyed: Subject<boolean> = new Subject();

  constructor(
    private store: Store<NgrxStateAtom>,
    private router: Router,
    private telemetryService: TelemetryService
  ) { }

  ngOnInit() {
    this.services$ = this.store.select(createSelector(serviceGroupState,
      (state) => state.servicesList));
    this.servicesStatus$ = this.store.select(servicesStatus);
    this.servicesError$ = this.store.select(servicesErrorResp);
    this.serviceGroupName$ = this.store.select(createSelector(serviceGroupState,
      (state) => state.selectedServiceGroupName));

    this.svcHealthSummary$ = this.store.select(createSelector(serviceGroupState,
      (state) => state.servicesHealthSummary));
    this.svcHealthSummary$.pipe(takeUntil(this.isDestroyed)).subscribe((servicesHealthSummary) => {
      this.servicesHealthSummary = servicesHealthSummary;
      this.totalServices = getOr(0, this.selectedHealth, this.servicesHealthSummary);
    });

    this.currentServicesFilters$ = this.store.select(createSelector(serviceGroupState,
      (state) => state.servicesFilters));
    this.currentServicesFilters$.pipe(takeUntil(this.isDestroyed)).subscribe((servicesFilters) => {
      this.selectedHealth = getOr('total', 'health', servicesFilters);
      this.currentPage    = getOr(1, 'page', servicesFilters);
      this.totalServices  = getOr(0, this.selectedHealth, this.servicesHealthSummary);
      this.telemetryService.track('applicationsServiceCount', {
         serviceGroupId: this.serviceGroupId,
         totalServices: this.totalServices,
         statusFilter: this.selectedHealth
      });
    });
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public updateHealthFilter(health: string): void {
    if ( includes(health, this.allowedStatus) ) {
      this.selectedHealth = health;
    } else {
      this.selectedHealth = 'total';
    }

    this.currentPage = 1;
    this.telemetryService.track('applicationsStatusFilter',
     { entity: 'service', statusFilter: this.selectedHealth});
    this.updateServicesFilters();
  }

  public updatePageNumber(pageNumber: number) {
    this.currentPage = pageNumber;
    const totalPages = Math.ceil(this.totalServices / this.pageSize) || 1;
    this.telemetryService.track('applicationsPageChange',
     { entity: 'service', pageNumber: pageNumber, totalPages: totalPages});
    this.updateServicesFilters();
  }

  // healthCheckStatus formats the provided health check to display
  // TODO: @afiune here is where we can inject an error message from the health check
  public healthCheckStatus(health: string): string {
    switch (health) {
      case 'OK':
        return 'Ok';
      case 'CRITICAL':
        return 'Critical';
      case 'WARNING':
        return 'Warning';
      case 'UNKNOWN':
        return 'Unknown';
      default:
        return health;
    }
  }

  // TODO @afiune: Add links when they work
  public tooltipMessageFor(field: string): string {
    switch (field) {
      case 'channel':
        return 'Add channel data. Learn more in Continuous Deployment Using Channels.';
      default:
      return '--';
    }
  }

  // format a timestamp to standardized RFC2822
  public formatTimestamp(time: Date): string {
    // Forcing UTC with custom RFC format
    return moment.utc(time).format(this.RFC2822);

    // @afiune Other formats/conversion methods
    // ISO format
    // return moment.utc(time).toISOString();

    // UTC format (displaying GMT)
    // return moment.utc(time).toDate().toUTCString();
  }

  // returns a timewizard message for the provided current and previous health checks
  public timewizardMessage(currentHealth: string, previousHealth: string): string {
    if (previousHealth === 'NONE') {
      return 'Since the service was loaded,';
    }

    return 'Changed from ' + this.formatHealthStatusForTimewizard(previousHealth) +
      ' to ' + this.formatHealthStatusForTimewizard(currentHealth);
  }

  // display all health check status in lower case, except for 'OK' (upper case)
  private formatHealthStatusForTimewizard(health: string): string {
    if (health !== 'OK') {
      return health.toLowerCase();
    }
    return health;
  }

  private updateServicesFilters(): void {
    const queryParams = {
      'sgStatus': this.selectedHealth,
      'sgPage': this.currentPage
    };
    this.router.navigate([], { queryParams, queryParamsHandling: 'merge' });
  }
}
