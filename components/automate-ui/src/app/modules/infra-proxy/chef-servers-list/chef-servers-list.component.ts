import { Component, OnInit, OnDestroy, EventEmitter } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatOptionSelectionChange } from '@angular/material/core/option';
import { Store } from '@ngrx/store';
import { filter, takeUntil } from 'rxjs/operators';
import { Regex } from 'app/helpers/auth/regex';
import { Subject, combineLatest } from 'rxjs';
import { isNil } from 'lodash/fp';

import { HttpStatus } from 'app/types/types';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { EntityStatus, pending } from 'app/entities/entities';
import { Server } from 'app/entities/servers/server.model';
import {
  getAllStatus as getAllServersStatus,
  allServers,
  saveStatus,
  saveError
} from 'app/entities/servers/server.selectors';
import { CreateServer, GetServers, DeleteServer } from 'app/entities/servers/server.actions';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

@Component({
  selector: 'app-chef-servers-list',
  templateUrl: './chef-servers-list.component.html',
  styleUrls: ['./chef-servers-list.component.scss']
})
export class ChefServersListComponent implements OnInit, OnDestroy {
  public isLoading = true;
  public servers: Server[] = [];
  public createModalVisible = false;
  public createChefServerForm: FormGroup;
  public creatingChefServer = false;
  public chefServersLoading = true;
  public conflictErrorEvent = new EventEmitter<boolean>();
  public fqdnForm: FormGroup;
  public ipForm: FormGroup;
  private isDestroyed = new Subject<boolean>();
  public serverToDelete: Server;
  public deleteModalVisible = false;
  public messageModalVisible = false;

  constructor(
    private store: Store<NgrxStateAtom>,
    private fb: FormBuilder,
    private layoutFacade: LayoutFacadeService,
    private telemetryService: TelemetryService
  ) {
    this.chefServersLoading = true;

    this.createChefServerForm = this.fb.group({
      // Must stay in sync with error checks in create-chef-server-modal.component.html
      // and updateServerForm in chef-server-details.component.ts
      id: ['',
        [Validators.required, Validators.pattern(Regex.patterns.ID), Validators.maxLength(64)]],
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]]
    });
    this.fqdnForm = this.fb.group({
      fqdn: ['', [Validators.required,
        Validators.pattern(Regex.patterns.NON_BLANK),
        Validators.pattern(Regex.patterns.VALID_FQDN)
      ]]
    });
    this.ipForm = this.fb.group({
      ip_address: ['', [Validators.required,
        Validators.pattern(Regex.patterns.NON_BLANK),
        Validators.pattern(Regex.patterns.VALID_IP_ADDRESS)
      ]]
    });
  }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);
    this.store.dispatch(new GetServers());

    combineLatest([
      this.store.select(getAllServersStatus),
      this.store.select(allServers)
    ]).pipe(
      filter(([getServersStatus, allServersState]) =>
        getServersStatus === EntityStatus.loadingSuccess &&
        !isNil(allServersState)),
      takeUntil(this.isDestroyed)
    ).subscribe(([_getServerSt, allServersState]) => {
      this.servers = allServersState;
      this.chefServersLoading = false;
    });

    this.store.select(saveStatus)
    .pipe(
      takeUntil(this.isDestroyed),
      filter(state => this.createModalVisible && !pending(state)))
      .subscribe(state => {
        this.creatingChefServer = false;
        if (state === EntityStatus.loadingSuccess) {
          this.closeCreateModal();
        }
      });

    combineLatest([
      this.store.select(saveStatus),
      this.store.select(saveError)
    ]).pipe(
      takeUntil(this.isDestroyed),
      filter(() => this.createModalVisible),
      filter(([state, error]) => state === EntityStatus.loadingFailure && !isNil(error)))
      .subscribe(([_, error]) => {
        if (error.status === HttpStatus.CONFLICT) {
          this.conflictErrorEvent.emit(true);
        } else {
          // Close the modal on any error other than conflict and display in banner.
          this.closeCreateModal();
        }
      });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public openCreateModal(): void {
    this.createModalVisible = true;
    this.resetCreateModal();
  }

  public closeCreateModal(): void {
    this.createModalVisible = false;
    this.resetCreateModal();
  }

  public createChefServer(): void {
    this.creatingChefServer = true;
    const server = {
      id: this.createChefServerForm.controls['id'].value,
      name: this.createChefServerForm.controls['name'].value.trim(),
      fqdn: this.fqdnForm.controls['fqdn'].value?.trim() || '',
      ip_address: this.ipForm.controls['ip_address'].value?.trim() || ''
    };
    this.store.dispatch(new CreateServer(server));
    this.telemetryService.track('InfraServer_Add_Chef_InfraServer');
  }

  private resetCreateModal(): void {
    this.creatingChefServer = false;
    this.createChefServerForm.reset();
    this.fqdnForm.reset();
    this.ipForm.reset();
    this.conflictErrorEvent.emit(false);
  }

  public startServerDelete($event: MatOptionSelectionChange, server: Server): void {
    if ($event.isUserInput) {
      if (server.orgs_count > 0) {
        this.messageModalVisible = true;
      } else {
        this.serverToDelete = server;
        this.deleteModalVisible = true;
      }
    }
  }

  public deleteServer(): void {
    this.closeDeleteModal();
    this.store.dispatch(new DeleteServer(this.serverToDelete));
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }

  public closeMessageModal(): void {
    this.messageModalVisible = false;
  }
}
