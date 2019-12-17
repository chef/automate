import { Component, OnDestroy, OnInit, Input } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Observable, Subject } from 'rxjs';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { takeUntil } from 'rxjs/operators';
import { getOr } from 'lodash/fp';

import { EntityStatus } from '../../entities/entities';
import { ServiceGroupsFacadeService } from '../../entities/service-groups/service-groups.facade';
import {
  ServiceGroupsHealthSummary,
  GroupService,
  GroupServicesFilters
} from '../../entities/service-groups/service-groups.model';
import { UpdateSelectedSG } from 'app/entities/service-groups/service-groups.actions';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';
import { DateTime } from 'app/helpers/datetime/datetime';

@Component({
  selector: 'app-services-sidebar',
  templateUrl: './services-sidebar.component.html',
  styleUrls: ['./services-sidebar.component.scss']
})

export class ServicesSidebarComponent implements OnInit, OnDestroy {
  @Input() serviceGroupsId: string;
  @Input() visible: boolean;

  public selectedHealth = 'total';
  public currentPage = 1;
  public pageSize = 25;
  public totalServices = 0;
  public RFC2822 = DateTime.RFC2822;
  public selectedSearchBarFilters = [];

  public services$: Observable<GroupService[]>;
  public serviceGroupsStatus$: Observable<EntityStatus>;
  public serviceGroupsError$: Observable<HttpErrorResponse>;
  public serviceGroupsName$: Observable<string>;
  public serviceGroupsHealthSummary: ServiceGroupsHealthSummary;

  private svcHealthSummary$: Observable<ServiceGroupsHealthSummary>;
  private currentServicesFilters$: Observable<GroupServicesFilters>;
  private isDestroyed: Subject<boolean> = new Subject();

  constructor(
    private serviceGroupsFacade: ServiceGroupsFacadeService,
    private telemetryService: TelemetryService,
    public store: Store<NgrxStateAtom>
  ) {
    this.services$ = this.serviceGroupsFacade.services$;
    this.serviceGroupsStatus$ = this.serviceGroupsFacade.serviceGroupsStatus$;
    this.serviceGroupsError$ = this.serviceGroupsFacade.serviceGroupsError$;
    this.serviceGroupsName$ = this.serviceGroupsFacade.serviceGroupsName$;
    this.svcHealthSummary$ = this.serviceGroupsFacade.svcHealthSummary$;
    this.currentServicesFilters$ = this.serviceGroupsFacade.currentServicesFilters$;
  }

  public tooltipMessageFor = this.serviceGroupsFacade.tooltipMessageFor;
  public healthCheckStatus = this.serviceGroupsFacade.healthCheckStatus;
  public formatTimestamp = this.serviceGroupsFacade.formatTimestamp;
  public timewizardMessage = this.serviceGroupsFacade.timewizardMessage;

  ngOnInit() {
    this.svcHealthSummary$.pipe(takeUntil(this.isDestroyed))
        .subscribe((serviceGroupsHealthSummary) => {
          this.serviceGroupsHealthSummary = serviceGroupsHealthSummary;
          this.totalServices = getOr(0, this.selectedHealth, this.serviceGroupsHealthSummary);
        });

    this.currentServicesFilters$.pipe(takeUntil(this.isDestroyed)).subscribe((servicesFilters) => {
      this.selectedHealth = getOr('total', 'health', servicesFilters);
      this.currentPage    = getOr(1, 'page', servicesFilters);
      this.totalServices  = getOr(0, this.selectedHealth, this.serviceGroupsHealthSummary);
      this.selectedSearchBarFilters = getOr([], 'searchBar', servicesFilters);
      this.telemetryService.track('applicationsServiceCount', {
         serviceGroupsId: this.serviceGroupsId,
         totalServices: this.totalServices,
         statusFilter: this.selectedHealth
      });
    });
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public updateHealthFilter(health: string) {
    this.currentPage = 1;
    this.selectedHealth = this.serviceGroupsFacade
      .updateHealthFilter(health, 'applicationsStatusFilter');
    this.refresh();
  }

  public updatePageNumber(pageNumber: number) {
    this.currentPage = pageNumber;
    this.serviceGroupsFacade
      .updatePageNumber(pageNumber, this.totalServices, this.currentPage, 'applicationsPageChange');
    this.refresh();
  }

  refresh() {
    if (this.serviceGroupsId) {

      const paramsForDispatch = {
          service_group_id: this.serviceGroupsId,
          page: this.currentPage,
          pageSize: this.pageSize,
          health: this.selectedHealth,
          searchBar: this.selectedSearchBarFilters
      };

      this.store.dispatch(new UpdateSelectedSG(paramsForDispatch));
      document.querySelector<HTMLElement>('app-services-sidebar').focus();
    }
  }

}
