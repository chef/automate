import { Component, OnInit, OnDestroy, EventEmitter } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Store, select } from '@ngrx/store';
import { filter, takeUntil, map } from 'rxjs/operators';
import { Regex } from 'app/helpers/auth/regex';
import { Observable, Subject, combineLatest } from 'rxjs';
import { isNil } from 'lodash/fp';
import { HttpStatus } from 'app/types/types';
import { ChefKeyboardEvent } from 'app/types/material-types';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { loading, EntityStatus, pending } from 'app/entities/entities';
import { Server } from 'app/entities/servers/server.model';
import {
  allServers, getStatus, saveStatus, saveError } from 'app/entities/servers/server.selectors';
import { CreateServer, GetServers, DeleteServer } from 'app/entities/servers/server.actions';
import { ChefSorters } from 'app/helpers/auth/sorter';

@Component({
  selector: 'app-chef-servers-list',
  templateUrl: './chef-servers-list.component.html'
})
export class ChefServersListComponent implements OnInit, OnDestroy {
  public loading$: Observable<boolean>;
  public sortedChefServers$: Observable<Server[]>;
  public createModalVisible = false;
  public createChefServerForm: FormGroup;
  public creatingChefServer = false;
  public conflictErrorEvent = new EventEmitter<boolean>();
  private isDestroyed = new Subject<boolean>();
  public serverToDelete: Server;
  public deleteModalVisible = false;

  constructor(
    private store: Store<NgrxStateAtom>,
    private fb: FormBuilder,
    private layoutFacade: LayoutFacadeService
  ) {
    this.loading$ = store.pipe(select(getStatus), map(loading));

    this.sortedChefServers$ = store.pipe(
      select(allServers),
      map(servers => ChefSorters.naturalSort(servers, 'name')));

    this.createChefServerForm = this.fb.group({
      // Must stay in sync with error checks in create-chef-server-modal.component.html
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      description: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      fqdn: ['', [Validators.required,
        Validators.pattern(Regex.patterns.NON_BLANK),
        Validators.pattern(Regex.patterns.VALID_FQDN)
      ]],
      ip_address: ['', [Validators.required,
        Validators.pattern(Regex.patterns.NON_BLANK),
        Validators.pattern(Regex.patterns.VALID_IP_ADDRESS)
      ]]
    });
  }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);
    this.store.dispatch(new GetServers());
    this.store.pipe(
      select(saveStatus),
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
      name: this.createChefServerForm.controls['name'].value.trim(),
      description: this.createChefServerForm.controls['description'].value.trim(),
      fqdn: this.createChefServerForm.controls['fqdn'].value.trim(),
      ip_address: this.createChefServerForm.controls['ip_address'].value.trim()
    };
    this.store.dispatch(new CreateServer(server));
  }

  private resetCreateModal(): void {
    this.creatingChefServer = false;
    this.createChefServerForm.reset();
    this.conflictErrorEvent.emit(false);
  }

  public startServerDelete($event: ChefKeyboardEvent, server: Server): void {
    if ($event.isUserInput) {
      this.serverToDelete = server;
      this.deleteModalVisible = true;
    }
  }

  public deleteServer(): void {
    this.closeDeleteModal();
    this.store.dispatch(new DeleteServer(this.serverToDelete));
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }
}
