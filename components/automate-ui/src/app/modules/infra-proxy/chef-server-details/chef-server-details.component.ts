import { Component, OnInit, OnDestroy, EventEmitter } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatOptionSelectionChange } from '@angular/material/core/option';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { Subject, combineLatest } from 'rxjs';
import { filter, pluck, takeUntil } from 'rxjs/operators';
import { identity, isNil } from 'lodash/fp';
import { HttpStatus } from 'app/types/types';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeParams, routeURL } from 'app/route.selectors';
import { Regex } from 'app/helpers/auth/regex';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { pending, EntityStatus, allLoaded } from 'app/entities/entities';
import {
  getStatus,
  serverFromRoute,
  updateStatus,
  getUsers,
  getUsersStatus,
  updateWebUIKey,
  validateWebUIKeyStatus,
  getValidateWebUIKeyStatus
} from 'app/entities/servers/server.selectors';

import { Server, WebUIKey } from 'app/entities/servers/server.model';
import {
  GetServer,
  UpdateServer,
  UpdateWebUIKey,
  ValidateWebUIKey
  // , GetUsers
} from 'app/entities/servers/server.actions';
import { GetOrgs, CreateOrg, DeleteOrg } from 'app/entities/orgs/org.actions';
import { Org } from 'app/entities/orgs/org.model';
import {
  createStatus,
  createError,
  allOrgs,
  getAllStatus as getAllOrgsForServerStatus,
  deleteStatus as deleteOrgStatus
} from 'app/entities/orgs/org.selectors';
import { ProjectConstants } from 'app/entities/projects/project.model';

export type ChefServerTabName = 'orgs' | 'users' | 'details';
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
  public fqdnForm: FormGroup;
  public ipForm: FormGroup;
  public orgForm: FormGroup;
  public createModalVisible = false;
  public creatingServerOrg = false;
  public conflictErrorEvent = new EventEmitter<boolean>();
  public orgToDelete: Org;
  public deleteModalVisible = false;
  private id: string;
  public saveSuccessful = false;
  public saveInProgress = false;
  public orgsListLoading = true;
  // isLoading represents the initial load as well as subsequent updates in progress.
  public isLoading = true;
  private isDestroyed = new Subject<boolean>();
  public unassigned = ProjectConstants.UNASSIGNED_PROJECT_ID;
  public selected = 'fqdn';
  public isUserLoaded = false;
  public users;
  public usersListLoading;
  public authFailure = false;
  public isValid = false;
  public isServerLoaded = false;
  public validating = true;

  public updateWebuiKeyForm: FormGroup;
  public updatingWebuiKey = false;
  public webuiKey: WebUIKey;
  public updateWebUIKeySuccessful = false;

  public uploadSliderVisible = false;
  public uploadZipForm: FormGroup;

  constructor(
    private fb: FormBuilder,
    private store: Store<NgrxStateAtom>,
    private router: Router,
    private layoutFacade: LayoutFacadeService

  ) {

    this.orgForm = fb.group({
      id: ['',
        [Validators.required, Validators.pattern(Regex.patterns.ID), Validators.maxLength(64)]],
      name: ['', [Validators.required]],
      admin_user: ['', [Validators.required]],
      admin_key: ['', [Validators.required]],
      projects: [[]]
    });

    this.updateWebuiKeyForm = this.fb.group({
      webUiKey: ['', [Validators.required]]
    });
  }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);
    // Populate our tabValue from the fragment.
    this.store.select(routeURL).pipe(takeUntil(this.isDestroyed))
    .subscribe((url: string) => {
      this.url = url;
      const [, fragment] = url.split('#');
      switch (fragment) {
        case 'details':
          this.tabValue = 'details';
          break;
        case 'users':
          this.tabValue = 'users';
          break;
        case 'attributes':
          this.tabValue = 'orgs';
          break;
      }
    });

    this.updateServerForm = this.fb.group({
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
    this.uploadZipForm = this.fb.group({
      fileName: ['', [Validators.required]]
    });

    this.store.select(routeParams).pipe(
      pluck('id'),
      filter(identity),
      takeUntil(this.isDestroyed))
      .subscribe((id: string) => {
        this.id = id;
        this.store.dispatch(new GetServer({ id }));
        this.store.dispatch(new GetOrgs({ server_id: id }));
        // this.store.dispatch(new GetUsers({ server_id: id }));
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
      this.fqdnForm.controls['fqdn'].setValue(this.server.fqdn);
      this.ipForm.controls['ip_address'].setValue(this.server.ip_address);
      this.creatingServerOrg = false;
      this.orgsListLoading = false;
      this.closeCreateModal();
      this.isServerLoaded = true;
    });

    combineLatest([
      this.store.select(createStatus),
      this.store.select(createError)
    ]).pipe(
      takeUntil(this.isDestroyed),
      filter(() => this.createModalVisible),
      filter(([state, error]) => state === EntityStatus.loadingFailure && !isNil(error)))
      .subscribe(([_, error]) => {
        if (error.status === HttpStatus.CONFLICT) {
          this.conflictErrorEvent.emit(true);
          this.creatingServerOrg = false;
        } else {
          // Close the modal on any error other than conflict and display in banner.
          this.closeCreateModal();
        }
      });

    this.store.select(deleteOrgStatus).pipe(
      filter(status => this.id !== undefined && status === EntityStatus.loadingSuccess),
      takeUntil(this.isDestroyed))
      .subscribe(() => {
        this.store.dispatch(new GetServer({ id: this.id })
      );
    });

    this.store.select(updateStatus).pipe(
      takeUntil(this.isDestroyed),
      filter(state => this.saveInProgress && !pending(state)))
    .subscribe((state) => {
      this.saveInProgress = false;
      this.saveSuccessful = (state === EntityStatus.loadingSuccess);
      if (this.saveSuccessful) {
        this.updateServerForm.markAsPristine();
        this.fqdnForm.markAsPristine();
        this.ipForm.markAsPristine();
      }
    });

    combineLatest([
      this.store.select(getUsersStatus),
      this.store.select(getUsers)
    ]).pipe(takeUntil(this.isDestroyed))
    .subscribe(([getUsersSt, UsersState]) => {
      if (getUsersSt === EntityStatus.loadingSuccess && !isNil(UsersState)) {
        this.users = UsersState;
        this.usersListLoading = false;
        this.users.users.length > 0 ? this.isUserLoaded = true : this.isUserLoaded = false;
      } else if (getUsersSt === EntityStatus.loadingFailure) {
        this.usersListLoading = false;
        this.authFailure = true;
        this.isUserLoaded = false;
      }
    });

    this.store.select(updateWebUIKey).pipe(
      takeUntil(this.isDestroyed),
      filter(state => this.updatingWebuiKey && !pending(state)))
    .subscribe((state) => {
      this.updatingWebuiKey = false;
      this.updateWebUIKeySuccessful = (state === EntityStatus.loadingSuccess);
      if (this.updateWebUIKeySuccessful) {
        this.isValid = true;
      }
    });

    setTimeout(() => {
      if (this.isServerLoaded) {
        this.validateWebUIKey(this.server);
      }
    }, 1000);
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  onSelectedTab(event: { target: { value: ChefServerTabName } }) {
    this.tabValue = event.target.value;
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
  }

  updateFormDisplay(id: string): void {
    this.selected = id;
  }

  public openCreateModal(): void {
    this.createModalVisible = true;
  }

  public closeCreateModal(): void {
    this.createModalVisible = false;
    this.resetCreateModal();
  }

  public createServerOrg(): void {
    this.creatingServerOrg = true;
    const serverOrg = {
      id: this.orgForm.controls['id'].value,
      server_id: this.id,
      name: this.orgForm.controls['name'].value.trim(),
      admin_user: this.orgForm.controls['admin_user'].value.trim(),
      admin_key: this.orgForm.controls['admin_key'].value.trim(),
      projects: this.orgForm.controls.projects.value
    };
    this.store.dispatch(new CreateOrg( serverOrg ));
  }

  private resetCreateModal(): void {
    this.creatingServerOrg = false;
    this.orgForm.reset();
    this.conflictErrorEvent.emit(false);
  }

  public startOrgDelete($event: MatOptionSelectionChange, org: Org): void {
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

  // validate the webui ui key
  private validateWebUIKey(server: Server): void {
    this.store.dispatch(new ValidateWebUIKey(server));
    combineLatest([
      this.store.select(validateWebUIKeyStatus),
      this.store.select(getValidateWebUIKeyStatus)
    ]).pipe(takeUntil(this.isDestroyed))
      .subscribe(([validateWebUISt, getValidateWebUIkeyState]) => {
        if (validateWebUISt === EntityStatus.loadingSuccess && !isNil(getValidateWebUIkeyState)) {
          this.isValid = getValidateWebUIkeyState.valid;
          this.isServerLoaded = false;
        }
        this.validating = false;
      });
  }

  saveServer(): void {
    this.saveSuccessful = false;
    this.saveInProgress = true;
    const updatedServer = {
      id: this.server.id,
      name: this.updateServerForm.controls.name.value.trim(),
      fqdn: this.fqdnForm.controls.fqdn.value?.trim() || '',
      ip_address: this.ipForm.controls.ip_address.value?.trim() || ''
    };
    this.store.dispatch(new UpdateServer({server: updatedServer}));
  }

  public updateWebuiKey(): void {
    this.updatingWebuiKey = true;
    this.webuiKey = {
      id: this.id,
      webui_key: this.updateWebuiKeyForm.controls['webUiKey'].value
    };
    this.updatingWebuiKeyData(this.webuiKey);
    this.updateWebuiKeyForm.reset();
  }

  private updatingWebuiKeyData(webuikey: WebUIKey) {
    this.store.dispatch(new UpdateWebUIKey(webuikey));
  }


  public openUploadSlider(): void {
    this.uploadSliderVisible = true;
    this.resetCreateModal();
  }

  public closeUploadSlider(): void {
    this.uploadSliderVisible = false;
    this.resetCreateModal();
  }

  public uploadZipFile(): void {
    this.resetUploadSlider();
  }

  private resetUploadSlider(): void {
    this.uploadZipForm.reset();
    this.conflictErrorEvent.emit(false);
  }
}
