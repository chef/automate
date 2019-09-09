import { Injectable } from '@angular/core';
import { Router } from '@angular/router';
import { Action, Store } from '@ngrx/store';
import { HttpErrorResponse } from '@angular/common/http';
import { Observable } from 'rxjs';
import { includes } from 'lodash/fp';
import * as moment from 'moment';

import { EntityStatus } from '../../entities/entities';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';
import * as fromServiceGroups from './service-groups.reducer';
import {
  ServiceGroup,
  ServiceGroupsHealthSummary,
  GroupService,
  GroupServicesFilters
} from '../../entities/service-groups/service-groups.model';
import {
  serviceGroupsStatus,
  serviceGroupsError,
  selectedServiceGroupHealth,
  selectedServiceGroupName,
  selectedServiceGroupList,
  selectedServiceGroupFilters
} from './service-groups.selector';

@Injectable({
  providedIn: 'root'
})
export class ServiceGroupsFacadeService {
  // RFC2822 format like: Wed, 03 Jul 2019 17:08:53 UTC
  // TODO @afiune we should move this to a common place where other
  // components can use this time format
  readonly RFC2822 = 'ddd, DD MMM YYYY, HH:mm:ss [UTC]';

  public serviceGroups$: Observable<ServiceGroup>;
  public serviceGroupsList$: Observable<ServiceGroup>;

  public services$: Observable<GroupService[]>;
  public serviceGroupsStatus$: Observable<EntityStatus>;
  public serviceGroupsError$: Observable<HttpErrorResponse>;
  public serviceGroupsName$: Observable<string>;
  public svcHealthSummary$: Observable<ServiceGroupsHealthSummary>;
  public currentServicesFilters$: Observable<GroupServicesFilters>;

  // The collection of allowable status
  public allowedStatus = ['ok', 'critical', 'warning', 'unknown'];

  constructor(
      private store: Store<fromServiceGroups.ServiceGroupsEntityState>,
      private router: Router,
      private telemetryService: TelemetryService
    ) {
    // this.allBooks$ = store.pipe(select(fromBooks.getAllBooks));
    this.services$ = store.select(selectedServiceGroupList);
    this.serviceGroupsStatus$ = store.select(serviceGroupsStatus);
    this.serviceGroupsError$ = store.select(serviceGroupsError);
    this.serviceGroupsName$ = store.select(selectedServiceGroupName);
    this.svcHealthSummary$ = this.store.select(selectedServiceGroupHealth);
    this.currentServicesFilters$ = this.store.select(selectedServiceGroupFilters);
  }

  dispatch(action: Action) {
    this.store.dispatch(action);
  }

  public updateServicesFilters(selectedHealth, currentPage): void {
    const queryParams = {
      'sgStatus': selectedHealth,
      'sgPage': currentPage
    };
    this.router.navigate([], { queryParams, queryParamsHandling: 'merge' });
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

  public updatePageNumber(pageNumber: number, total: number, pageSize: number, trackEvent: string) {
    const totalPages = Math.ceil(total / pageSize) || 1;
    this.telemetryService.track(trackEvent, {
        entity: 'service', pageNumber: pageNumber, totalPages: totalPages
    });
  }

  public updateHealthFilter(health: string, trackEvent: string): string {
    health = includes(health, this.allowedStatus) ? health : 'total';
    this.telemetryService.track(trackEvent, {
        entity: 'service', statusFilter: health
    });
    return health;
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

  // format a timestamp to standardized RFC2822
  public formatTimestamp(time: Date): string {
    // Forcing UTC with custom RFC format
    return moment.utc(time).format(this.RFC2822);
  }
}
