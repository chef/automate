import { Injectable } from '@angular/core';
import { Store } from '@ngrx/store';
import { HttpErrorResponse } from '@angular/common/http';
import { Observable } from 'rxjs';
import * as moment from 'moment/moment';

import { EntityStatus } from '../../entities/entities';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

import * as fromServiceGroups from './service-groups.reducer';
import {
  ServiceGroup,
  ServiceGroupsHealthSummary,
  GroupService,
  GroupServicesFilters,
  AllowedStatus
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

  constructor(
      private store: Store<fromServiceGroups.ServiceGroupsEntityState>,
      private telemetryService: TelemetryService
    ) {
    this.services$ = store.select(selectedServiceGroupList);
    this.serviceGroupsStatus$ = store.select(serviceGroupsStatus);
    this.serviceGroupsError$ = store.select(serviceGroupsError);
    this.serviceGroupsName$ = store.select(selectedServiceGroupName);
    this.svcHealthSummary$ = this.store.select(selectedServiceGroupHealth);
    this.currentServicesFilters$ = this.store.select(selectedServiceGroupFilters);
  }

  // returns a timewizard message for the provided current and previous health checks
  public timewizardMessage(currentHealth: string, previousHealth: string): string {
    if (previousHealth === 'NONE') {
      return 'Since the service was loaded,';
    }

    if (currentHealth !== 'OK') {
      currentHealth = currentHealth.toLowerCase();
    }

    if (previousHealth !== 'OK') {
      previousHealth = previousHealth.toLowerCase();
    }

    return 'Changed from ' + previousHealth + ' to ' + currentHealth;
  }

  public updatePageNumber(pageNumber: number, total: number, pageSize: number, trackEvent: string) {
    const totalPages = Math.ceil(total / pageSize) || 1;
    this.telemetryService.track(trackEvent, {
        entity: 'service', pageNumber: pageNumber, totalPages: totalPages
    });
  }

  public updateHealthFilter(health: AllowedStatus, trackEvent: string): AllowedStatus {
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
        return 'OK';
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
