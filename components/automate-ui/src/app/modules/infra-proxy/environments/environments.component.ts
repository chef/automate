import { Component, Input, OnInit, OnDestroy, EventEmitter, Output } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { filter, takeUntil } from 'rxjs/operators';
import { isNil } from 'lodash/fp';
import { GetEnvironments, DeleteEnvironment } from 'app/entities/environments/environment.action';
import { Environment } from 'app/entities/environments/environment.model';
import { getAllStatus, deleteStatus, environmentList } from 'app/entities/environments/environment.selectors';
import { EntityStatus } from 'app/entities/entities';
import { Regex } from 'app/helpers/auth/regex';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

@Component({
  selector: 'app-environments',
  templateUrl: './environments.component.html',
  styleUrls: ['./environments.component.scss']
})

export class EnvironmentsComponent implements OnInit, OnDestroy {
  @Input() serverId: string;
  @Input() orgId: string;
  @Output() resetKeyRedirection = new EventEmitter<boolean>();

  public authFailure = false;
  public deleteModalVisible = false;
  public environmentsListLoading = true;
  public loading = false;
  public deleting = true;

  public current_page = 1;
  public environments: Environment[] = [];
  public per_page = 100;
  public searchValue = '';
  public total: number;

  public environmentToDelete: Environment;
  public environmentListState: { items: Environment[], total: number };
  public openEnvironmentModal = new EventEmitter<void>();
  private isDestroyed = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private telemetryService: TelemetryService
  ) {
  }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

    this.getEnvironmentData();

    combineLatest([
        this.store.select(getAllStatus),
        this.store.select(environmentList)
      ]).pipe(takeUntil(this.isDestroyed))
      .subscribe(([getEnvironmentsSt, EnvironmentsState]) => {
        if (getEnvironmentsSt === EntityStatus.loadingSuccess && !isNil(EnvironmentsState)) {
          this.environmentListState = EnvironmentsState;
          this.environments = EnvironmentsState?.items;
          this.total = EnvironmentsState?.total;
          this.environmentsListLoading = false;
          this.loading = false;
          this.deleting = false;
        } else if (getEnvironmentsSt === EntityStatus.loadingFailure) {
          this.environmentsListLoading = false;
          this.authFailure = true;
        }
      });

    this.store.select(deleteStatus).pipe(
      filter(status => status === EntityStatus.loadingSuccess),
      takeUntil(this.isDestroyed))
      .subscribe(() => {
        this.loading = true;
        if (this.environments && this.environments.length === 0 &&
          this.current_page !== 1) {
          this.current_page = this.current_page - 1;
        }
        this.getEnvironmentData();
      });
  }

  resetKeyTabRedirection(resetLink: boolean) {
    this.resetKeyRedirection.emit(resetLink);
  }

  searchEnvironment(currentText: string) {
    this.current_page = 1;
    this.loading = true;
    this.searchValue = currentText;
    if ( currentText !== ''  && !Regex.patterns.NO_WILDCARD_ALLOW_HYPHEN.test(currentText)) {
      this.loading = false;
      this.environments.length = 0;
      this.total = 0;
    } else {
      this.getEnvironmentData();
    }
    this.telemetryService.track('InfraServer_Environments_Search');
  }

  onPageChange(event: number): void {
    this.current_page = event;
    this.loading = true;
    this.getEnvironmentData();
  }

  getEnvironmentData() {
    const payload = {
      environmentName: this.searchValue,
      page: this.current_page,
      per_page: this.per_page,
      server_id: this.serverId,
      org_id: this.orgId
    };

    this.store.dispatch(new GetEnvironments(payload));
  }

  public openCreateModal(): void {
    this.openEnvironmentModal.emit();
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public startEnvironmentDelete(environment: Environment): void {
    this.environmentToDelete = environment;
    this.deleteModalVisible = true;
  }

  public deleteEnvironment(): void {
    this.closeDeleteModal();
    this.store.dispatch(new DeleteEnvironment({
      server_id: this.serverId, org_id: this.orgId, name: this.environmentToDelete.name
    }));
    this.telemetryService.track('InfraServer_Environments_Delete');
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
    this.deleting = true;
  }

  onUpdatePage($event: { pageIndex: number; pageSize: number; }) {
    this.current_page = $event.pageIndex + 1;
    this.per_page = $event.pageSize;
    this.loading = true;
    this.getEnvironmentData();
  }
}

