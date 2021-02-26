import { Component, Input, OnInit, OnDestroy, EventEmitter, Output } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { takeUntil } from 'rxjs/operators';
import { isNil } from 'lodash/fp';
import { GetEnvironments, DeleteEnvironment } from 'app/entities/environments/environment.action';
import { Environment } from 'app/entities/environments/environment.model';
import { getAllStatus, environmentList } from 'app/entities/environments/environment.selectors';

@Component({
  selector: 'app-environments',
  templateUrl: './environments.component.html',
  styleUrls: ['./environments.component.scss']
})

export class EnvironmentsComponent implements OnInit, OnDestroy {
  @Input() serverId: string;
  @Input() orgId: string;
  @Output() resetKeyRedirection = new EventEmitter<boolean>();

  public environments: Environment[] = [];
  public environmentsListLoading = true;
  public environmentListState: { items: Environment[], total: number };
  public authFailure = false;
  public per_page = 9;
  public page = 1;
  public searching = false;
  public searchValue = '';
  public total: number;
  public environmentToDelete: Environment;
  public deleteModalVisible = false;
  private isDestroyed = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) { }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

    const payload = {
        environmentName: '',
        page: this.page,
        per_page: this.per_page,
        server_id: this.serverId,
        org_id: this.orgId
      };

      this.store.dispatch(new GetEnvironments(payload));

    combineLatest([
        this.store.select(getAllStatus),
        this.store.select(environmentList)
      ]).pipe(
        takeUntil(this.isDestroyed))
      .subscribe(([_getEnvironmentsSt, EnvironmentsState]) => {
        if (!isNil(EnvironmentsState)) {
          this.environmentListState = EnvironmentsState;
          if (this.environmentListState.items.length === 0 && this.environmentListState.total !== 0) {
            this.store.dispatch(new GetEnvironments(payload));
            this.environmentsListLoading = true;
          } else {
            this.environments = EnvironmentsState?.items;
            this.total = EnvironmentsState?.total;
            this.environmentsListLoading = false;
            this.searching = false;
          }
        }
      });
  }

  resetKeyTabRedirection(resetLink: boolean) {
    this.resetKeyRedirection.emit(resetLink);
  }

  searchEnvironment(currentText: string) {
    this.page = 1;
    this.searching = true;
    this.searchValue = currentText;
    this.getEnvironmentData();
  }

  onPageChange(event: number): void {
    this.page = event;
    this.searching = true;
    this.getEnvironmentData();
  }

  getEnvironmentData() {
    const payload = {
      environmentName: this.searchValue,
      page: this.page,
      per_page: this.per_page,
      server_id: this.serverId,
      org_id: this.orgId
    };

    this.store.dispatch(new GetEnvironments(payload));
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
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }
}

