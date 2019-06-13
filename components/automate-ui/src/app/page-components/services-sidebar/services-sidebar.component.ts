import { Component, OnDestroy, OnInit, Input } from '@angular/core';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Observable, Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { serviceGroupState, serviceGroupStatus
} from '../../entities/service-groups/service-groups.selector';
import { createSelector } from '@ngrx/store';
import { EntityStatus } from '../../entities/entities';
import {
  Service, ServicesFilters, HealthSummary
} from '../../entities/service-groups/service-groups.model';
import { includes, getOr } from 'lodash/fp';

@Component({
  selector: 'app-services-sidebar',
  templateUrl: './services-sidebar.component.html',
  styleUrls: ['./services-sidebar.component.scss']
})

export class ServicesSidebarComponent implements OnInit, OnDestroy {
  @Input() serviceGroupId: string;
  @Input() visible: boolean;

  public services$: Observable<Service[]>;
  public serviceGroupStatus$: Observable<EntityStatus>;
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
    private router: Router
  ) { }

  ngOnInit() {
    this.services$ = this.store.select(createSelector(serviceGroupState,
      (state) => state.servicesList));
    this.serviceGroupStatus$ = this.store.select(serviceGroupStatus);
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
    this.updateServicesFilters();
  }

  public updatePageNumber(pageNumber: number) {
    this.currentPage = pageNumber;
    this.updateServicesFilters();
  }

  // healthCheckStatus returns the formated health_check status from the provided service
  // TODO: @afiune here is where we can inject an error message from the health check
  public healthCheckStatus(service: Service): string {
    switch (service.health_check) {
      case 'OK':
        return 'Ok';
      case 'CRITICAL':
        return 'Critical';
      case 'WARNING':
        return 'Warning';
      case 'UNKNOWN':
        return 'Unknown';
      default:
        return service.health_check;
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

  private updateServicesFilters(): void {
    const queryParams = {
      'sgStatus': this.selectedHealth,
      'sgPage': this.currentPage
    };
    this.router.navigate([], { queryParams, queryParamsHandling: 'merge' });
  }
}
