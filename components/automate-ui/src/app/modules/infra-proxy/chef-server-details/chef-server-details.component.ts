import { Component, OnInit, OnDestroy, EventEmitter } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { Store, select } from '@ngrx/store';
import { Subject, combineLatest } from 'rxjs';
import { filter, pluck, takeUntil } from 'rxjs/operators';
import { identity, isNil } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeParams, routeURL } from 'app/route.selectors';
import { Regex } from 'app/helpers/auth/regex';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';

import { pending, EntityStatus, allLoaded } from 'app/entities/entities';
import {
  getStatus, serverFromRoute, updateStatus
} from 'app/entities/servers/server.selectors';

import { Server } from 'app/entities/servers/server.model';
import { GetServer, UpdateServer } from 'app/entities/servers/server.actions';
import { GetOrgs, CreateOrg, DeleteOrg } from 'app/entities/orgs/org.actions';
import { Org } from 'app/entities/orgs/org.model';
import {
  allOrgs,
  getAllStatus as getAllOrgsForServerStatus,
  deleteStatus as deleteOrgStatus
} from 'app/entities/orgs/org.selectors';
import { ChefKeyboardEvent } from 'app/types/material-types';

export type ChefServerTabName = 'orgs' | 'details';



@Component({
  selector: 'app-chef-server-details',
  templateUrl: './chef-server-details.component.html',
  styleUrls: ['./chef-server-details.component.scss']
})
export class ChefServerDetailsComponent implements OnInit, OnDestroy {
  public server: Server;
  public orgs: Org[] = [];
  public tabValue: ChefServerTabName = 'orgs';
  public url: string;
  public updateServerForm: FormGroup;
  public orgForm: FormGroup;
  public createModalVisible = false;
  public creatingServerOrg = false;
  public conflictErrorEvent = new EventEmitter<boolean>();
  public orgToDelete: Org;
  public deleteModalVisible = false;
  public modalType: string;
  private id: string;
  public saveSuccessful = false;

  // isLoading represents the initial load as well as subsequent updates in progress.
  public isLoading = true;
  public saving = false;
  private isDestroyed = new Subject<boolean>();

  constructor(
    private fb: FormBuilder,
    private store: Store<NgrxStateAtom>,
    private router: Router,
    private layoutFacade: LayoutFacadeService

  ) {

    this.orgForm = fb.group({
      name: ['', [Validators.required]],
      admin_user: ['', [Validators.required]],
      admin_key: ['', [Validators.required]]
    });
  }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);
    // Populate our tabValue from the fragment.
    this.store.select(routeURL).pipe(takeUntil(this.isDestroyed))
    .subscribe((url: string) => {
      this.url = url;
      const [, fragment] = url.split('#');
      this.tabValue = (fragment === 'details') ? 'details' : 'orgs';
    });

    this.updateServerForm = this.fb.group({
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      description: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      fqdn: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      ip_address: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]]
    });

    this.store.select(routeParams).pipe(
      pluck('id'),
      filter(identity),
      takeUntil(this.isDestroyed))
      .subscribe((id: string) => {
        this.id = id;
        this.store.dispatch(new GetServer({ id }));
        this.store.dispatch(new GetOrgs({ server_id: id }));
      });

    combineLatest([
      this.store.select(getStatus),
      this.store.select(getAllOrgsForServerStatus)
    ]).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe(([getServerSt, getOrgsSt]) => {
      this.isLoading =
        !allLoaded([getServerSt, getOrgsSt]);
    });

    combineLatest([
      this.store.select(getStatus),
      this.store.select(getAllOrgsForServerStatus),
      this.store.select(serverFromRoute),
      this.store.select(allOrgs)
    ]).pipe(
      filter(([getServerStatus, getOrgsStatus, serverState, allOrgsState]) =>
        getServerStatus === EntityStatus.loadingSuccess &&
        getOrgsStatus === EntityStatus.loadingSuccess &&
        !isNil(serverState) &&
        !isNil(allOrgsState)),
      takeUntil(this.isDestroyed)
    ).subscribe(([_getServerSt, _getOrgsSt, ServerState, allOrgsState]) => {
      this.server = { ...ServerState };
      this.orgs = allOrgsState;
      this.updateServerForm.controls['name'].setValue(this.server.name);
      this.updateServerForm.controls['description'].setValue(this.server.description);
      this.updateServerForm.controls['fqdn'].setValue(this.server.fqdn);
      this.updateServerForm.controls['ip_address'].setValue(this.server.ip_address);
      this.creatingServerOrg = false;
      this.closeCreateModal();
    });

    this.store.select(deleteOrgStatus).pipe(
      filter(status => this.id !== undefined && status === EntityStatus.loadingSuccess),
      takeUntil(this.isDestroyed))
      .subscribe(() => {
        this.store.dispatch(new GetServer({ id: this.id })
      );
    });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  onSelectedTab(event: { target: { value: ChefServerTabName } }) {
    this.tabValue = event.target.value;
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
  }

  public openCreateModal(type: string): void {
    this.createModalVisible = true;
    this.modalType = type;
  }

  public closeCreateModal(): void {
    this.createModalVisible = false;
    this.resetCreateModal();
  }

  public createServerOrg(): void {
    this.creatingServerOrg = true;
    const serverOrg = {
      server_id: this.id,
      name: this.orgForm.controls['name'].value.trim(),
      admin_user: this.orgForm.controls['admin_user'].value.trim(),
      admin_key: this.orgForm.controls['admin_key'].value.trim()
    };
    this.store.dispatch(new CreateOrg( serverOrg ));
  }

  private resetCreateModal(): void {
    this.creatingServerOrg = false;
    this.orgForm.reset();
    this.conflictErrorEvent.emit(false);
  }

  public startOrgDelete($event: ChefKeyboardEvent, org: Org): void {
    if ($event.isUserInput) {
      this.orgToDelete = org;
      this.deleteModalVisible = true;
    }
  }

  public deleteOrg(): void {
    this.closeDeleteModal();
    this.store.dispatch(new DeleteOrg(this.orgToDelete));
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }

  saveServer(): void {
    this.saveSuccessful = false;
    this.saving = true;
    const updatedServer = {
      id: this.server.id,
      name: this.updateServerForm.controls.name.value.trim(),
      description: this.updateServerForm.controls.description.value.trim(),
      fqdn: this.updateServerForm.controls.fqdn.value.trim(),
      ip_address: this.updateServerForm.controls.ip_address.value.trim()
    };
    this.store.dispatch(new UpdateServer({server: updatedServer}));
    this.store.pipe(select(updateStatus),
      takeUntil(this.isDestroyed),
      filter(state => this.saving && !pending(state)))
      .subscribe((state) => {
        this.saving = false;
        this.saveSuccessful = (state === EntityStatus.loadingSuccess);
        if (this.saveSuccessful) {
          this.updateServerForm.markAsPristine();
        }
      });
  }

}
