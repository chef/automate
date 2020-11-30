import { Component, OnDestroy, OnInit, Input } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Observable, Subject } from 'rxjs';
import { Store, select } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { takeUntil } from 'rxjs/operators';
import { getOr } from 'lodash/fp';

import { EntityStatus } from '../../entities/entities';
import { ServiceGroupsFacadeService } from '../../entities/service-groups/service-groups.facade';
import {
  ServiceGroupsHealthSummary,
  GroupService,
  GroupServicesFilters,
  AllowedStatus
} from '../../entities/service-groups/service-groups.model';
import { UpdateSelectedSG, DeleteServicesById } from 'app/entities/service-groups/service-groups.actions';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';
import { DateTime } from 'app/helpers/datetime/datetime';
import { serviceDeletionStatus } from 'app/entities/service-groups/service-groups.selector';

@Component({
  selector: 'app-services-sidebar',
  templateUrl: './services-sidebar.component.html',
  styleUrls: ['./services-sidebar.component.scss']
})

export class ServicesSidebarComponent implements OnInit, OnDestroy {
  @Input() serviceGroupsId: string;
  @Input() visible: boolean;

  public allowedStatus = AllowedStatus;
  public selectedHealth = 'total';
  public currentPage = 1;
  public pageSize = 25;
  public totalServices = 0;
  public RFC2822 = DateTime.RFC2822;
  public selectedSearchBarFilters = [];

  // Health Check Accordions
  public LINES_OUTPUT_3 = 138;  // provides max of 3 lines of output visible by default;
  public activeErrorAccordions: number[] = [];
  public activeHealthAccordions: number[] = [];

  public services$: Observable<GroupService[]>;
  public serviceGroupsStatus$: Observable<EntityStatus>;
  public serviceGroupsError$: Observable<HttpErrorResponse>;
  public serviceGroupsName$: Observable<string>;
  public serviceGroupsHealthSummary: ServiceGroupsHealthSummary;

  private svcHealthSummary$: Observable<ServiceGroupsHealthSummary>;
  private currentServicesFilters$: Observable<GroupServicesFilters>;
  private isDestroyed: Subject<boolean> = new Subject();

  // Manual Deletion of Services
  public servicesList: GroupService[] = [];
  public checkedServices: number[] = [];
  public hasAllSelected = false;
  public isIndeterminate = false;
  public checkedServicesDisplay = '';
  public deleteModalVisible = false;

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

    this.services$.pipe(takeUntil(this.isDestroyed))
      .subscribe((services) => {
          this.resetServiceSelections();
          this.servicesList = services;
      });

    this.store.pipe(
      select(serviceDeletionStatus),
      takeUntil(this.isDestroyed)
    )
      .subscribe((status) => {
        if (status === EntityStatus.loadingSuccess) {
          this.resetServiceSelections();
        } else if (status === EntityStatus.loadingFailure) {
          this.deleteModalVisible = false;
        }
      });

    this.updateCheckedServicesDisplay();
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public updateHealthFilter(health: AllowedStatus) {
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

      const paramsForDispatch: GroupServicesFilters = {
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

  public toggleMoreErrorMsg(index: number) {
    if (this.activeErrorAccordions.includes(index)) {
      this.activeErrorAccordions = this.activeErrorAccordions.filter(n => n !== index); // close
    } else {
      this.activeErrorAccordions.push(index); // open accordion
    }
  }

  public toggleMoreHealthMsg(index: number) {
    if (this.activeHealthAccordions.includes(index)) {
      this.activeHealthAccordions = this.activeHealthAccordions.filter(n => n !== index); // close
    } else {
      this.activeHealthAccordions.push(index); // open accordion
    }
  }

  public handleToggleCheckbox(id: number, checked: boolean): void {
    if (checked) {
      this.checkedServices.push(id);
    } else {
      this.checkedServices = this.checkedServices.filter(n => n !== id);
    }
    this.updateCheckedServicesDisplay();
  }

  public handleSelectAll(checked: boolean): void {
    if (checked) {
      this.checkedServices = this.servicesList.map(service => service.id);
    } else {
      this.checkedServices = [];
    }
    this.updateCheckedServicesDisplay();
  }

  private updateCheckedServicesDisplay(): void {
    // Must reset values for indeterminate to function properly
    this.hasAllSelected = false;
    this.isIndeterminate = false;

    const numOfCheckedServices = this.checkedServices.length;
    if (numOfCheckedServices === 0) {
    this.checkedServicesDisplay = 'Services';
    } else if (numOfCheckedServices === 1) {
      this.checkedServicesDisplay = '1 Service';
    } else {
      this.checkedServicesDisplay = `${numOfCheckedServices} Services`;
    }

    if (this.checkedServices.length === this.servicesList.length && this.servicesList.length > 0) {
      this.hasAllSelected = true;
    } else if ( this.checkedServices.length < 1 ) {
      this.hasAllSelected = false;
    } else {
      this.isIndeterminate = true;
    }
  }

  public beginServicesDelete(): void {
    this.deleteModalVisible = true;
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }

  public deleteServices(): void {
    this.store.dispatch( new DeleteServicesById({servicesToDelete: this.checkedServices}) );
  }

  private resetServiceSelections(): void {
    this.checkedServices = [];
    this.updateCheckedServicesDisplay();
    this.closeDeleteModal();
  }

}
