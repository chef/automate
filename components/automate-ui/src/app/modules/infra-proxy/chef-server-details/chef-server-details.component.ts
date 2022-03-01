import { Component, OnInit, OnDestroy, EventEmitter, ViewChild } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatOptionSelectionChange } from '@angular/material/core/option';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { Subject, combineLatest, interval, Subscription } from 'rxjs';
import { filter, pluck, take, takeUntil } from 'rxjs/operators';
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
  getValidateWebUIKeyStatus,
  migrationStatus,
  getMigrationStatus
} from 'app/entities/servers/server.selectors';

import { MigrationStatus, Server, WebUIKey } from 'app/entities/servers/server.model';
import {
  GetMigrationStatus,
  GetServer,
  UpdateServer,
  UpdateWebUIKey,
  ValidateWebUIKey
  // , GetUsers
} from 'app/entities/servers/server.actions';
import {
  GetOrgs,
  CreateOrg,
  DeleteOrg,
  UploadZip,
  CancelMigration,
  GetPreviewData,
  ConfirmPreview
} from 'app/entities/orgs/org.actions';
import { Org } from 'app/entities/orgs/org.model';
import {
  createStatus,
  createError,
  allOrgs,
  getAllStatus as getAllOrgsForServerStatus,
  deleteStatus as deleteOrgStatus,
  uploadStatus,
  uploadDetails,
  cancelStatus,
  previewStatus,
  previewData,
  confirmPreviewStatus
} from 'app/entities/orgs/org.selectors';
import { ProjectConstants } from 'app/entities/projects/project.model';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';
import { SyncOrgUsersSliderComponent } from '../sync-org-users-slider/sync-org-users-slider.component';

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

  // used for webuikey
  public updateWebuiKeyForm: FormGroup;
  public updatingWebuiKey = false;
  public webuiKey: WebUIKey;
  public updateWebUIKeySuccessful = false;

  // used for the migration
  public uploadZipForm: FormGroup;
  public isUploaded = false;
  public migrationStatus: MigrationStatus;
  public migrationStatusPercentage = 0;
  public stepsCompleted: string;
  public totalMigrationSteps = 13;
  public migrationStepValue: number;
  public migrationfailed = false;
  public migrationCompleted = false;
  public migrationInProgress = false;
  public migrationLoading = true;
  public migrationStarted = false;
  public migrationIsInPreview = false;
  public migrationNotRunning = true;
  public migration_id = '';
  public migration_type: string;
  public cancelMigrationInProgress = false;
  public canceMigrationSuccessful = false;
  public isCancelled = false;
  public previewDataLoaded =  false;
  public previewData;
  public isPreview = false;
  public confirmPreviewSuccessful = false;
  public confirmPreviewsubmit = false;

  public migrationSteps: Record<string, string> = {
    1: 'Migration started',
    2: 'Upload of zip file',
    3: 'Unzip of zip file',
    4: 'Parsing of orgs file',
    5: 'Parsing of users file',
    6: 'Parsing of user association file',
    7: 'Parsing of user permissions file',
    8: 'Creating Preview',
    9: 'Migration of organization',
    10: 'Migration of users',
    11: 'Association of users to orgs',
    12: 'Migrating user permissions',
    13: 'Migration Completed'
  };

  @ViewChild('upload', { static: false }) upload: SyncOrgUsersSliderComponent;
  mySubscription: Subscription;

  constructor(
    private fb: FormBuilder,
    private store: Store<NgrxStateAtom>,
    private router: Router,
    private layoutFacade: LayoutFacadeService,
    private telemetryService: TelemetryService
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
      file: ['', [Validators.required]]
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

    // Get server and orgs 
    this.getServerAndOrgs();

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

    combineLatest([
      this.store.select(uploadStatus),
      this.store.select(uploadDetails)
    ]).pipe(takeUntil(this.isDestroyed))
    .subscribe(([uploadStatusSt, uploadDetailsState]) => {
      if (uploadStatusSt === EntityStatus.loadingSuccess && !isNil(uploadDetailsState)) {
        // show migration slider
        this.isUploaded = true;
        this.migration_id = uploadDetailsState?.migration_id;
        this.migrationLoading = false;
        this.getMigrationStatus(this.migration_id);
      } else if (uploadStatusSt === EntityStatus.loadingFailure) {
        // close upload slider with error notification
        this.isUploaded = false;
        this.migrationIsFailed()
        this.upload.closeUploadSlider();
      }
    });

    this.store.select(cancelStatus).pipe(
      takeUntil(this.isDestroyed),
      filter(state => !pending(state)))
    .subscribe((state) => {
      this.cancelMigrationInProgress = true;
      this.canceMigrationSuccessful = (state === EntityStatus.loadingSuccess);
      if (this.canceMigrationSuccessful) {
        this.migrationIsCancelled();
      } else {
        this.migrationIsInProcess();
      }
    });

    combineLatest([
      this.store.select(previewStatus),
      this.store.select(previewData)
    ]).pipe(takeUntil(this.isDestroyed))
    .subscribe(([previewStatusSt, previewState]) => {
      if (previewStatusSt === EntityStatus.loadingSuccess && !isNil(previewState)) {
        this.previewData = previewState.staged_data;
        this.isPreview = true;
      } else if (previewStatusSt === EntityStatus.loadingFailure) {
        this.previewDataLoaded = false;
      }
    });

    this.store.select(confirmPreviewStatus).pipe(
    takeUntil(this.isDestroyed),
    filter(state => this.migrationIsInPreview && !pending(state)))
    .subscribe((state) => {
      this.confirmPreviewSuccessful = (state === EntityStatus.loadingSuccess);
      if (this.confirmPreviewSuccessful) {
        this.confirmPreviewsubmit = true;
        this.migrationLoading = false;
        this.migrationIsCompleted();
        this.orgsListLoading = true;
        setTimeout( () => {
          this.getServerAndOrgs();
        }, 5000 );
      } else {
        this.confirmPreviewsubmit = false;
      }
    });

    setTimeout(() => {
      if (this.isServerLoaded) {
        this.validateWebUIKey(this.server);
      }
    }, 1000);

    this.mySubscription = interval(3000).subscribe(() => {
      if (this.migrationStarted && this.migration_type !== 'Migration Completed') {
        this.getMigrationStatus(this.migration_id);
      }
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

  public getServerAndOrgs(): void {
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
    ).pipe(take(1))
    .subscribe(([_getServerSt, _getOrgsSt, ServerState, allOrgsState]) => {
      this.server = { ...ServerState };
      this.orgs = allOrgsState;
      this.updateServerForm.controls['name'].setValue(this.server.name);
      this.fqdnForm.controls['fqdn'].setValue(this.server.fqdn);
      this.ipForm.controls['ip_address'].setValue(this.server.ip_address);
      this.creatingServerOrg = false;
      this.orgsListLoading = false;
      this.closeCreateModal();
      this.isServerLoaded = true;
      this.migrationLoading = false;
      this.migration_id = '';
      this.migration_id = this.server.migration_id;
      this.migration_type = this.server.migration_type;
      if (this.orgs.length > 0 ) {
        this.migrationCompleted = true;
        this.migrationNotRunning = false;
      }
      if (this.migration_id !== '') {
        this.migrationProcessStarted();
        this.getMigrationStatus(this.migration_id);
      }
    });
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
    this.telemetryService.track('InfraServer_Add_Chef_Organization');
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

  // get migration status
  private getMigrationStatus(migration_id: string): void {
    this.store.dispatch(new GetMigrationStatus(migration_id));
    combineLatest([
      this.store.select(migrationStatus),
      this.store.select(getMigrationStatus)
    ]).pipe(takeUntil(this.isDestroyed))
      .subscribe(([migrationSt, getMigrationState]) => {
        if (migrationSt === EntityStatus.loadingSuccess && !isNil(getMigrationState)) {
          this.migrationStatus = getMigrationState;
          this.migration_type = this.migrationStatus.migration_type;
          const migration_status = this.migrationStatus.migration_status;
          if (migration_status === 'Completed' || migration_status === 'In Progress') {
            this.migrationStepValue = this.getKeyByValue(this.migrationSteps, this.migration_type);
            this.migrationStatusPercentage =
              Number((this.migrationStepValue / this.totalMigrationSteps) * 100);
            this.migrationIsInProcess();
            this.stepsCompleted =  this.migrationStepValue.toFixed(0) + '/' + '13';
            if (this.migration_type === 'Creating Preview'
              && this.confirmPreviewsubmit === false
              && this.isCancelled === false) {
              this.migrationInPreview();
            }

            if (this.migration_type === 'Migration Completed') {
              this.mySubscription.unsubscribe();
              this.migrationIsCompleted();
            }

            if (this.migration_type === 'Migration Cancelled' && this.canceMigrationSuccessful) {
              this.mySubscription.unsubscribe();
              this.migrationIsCancelled();
            }
          } 
          else {
            this.migrationfailed = true;
            this.migrationCompleted = false;
            this.mySubscription.unsubscribe();
          }
        }
      });
  }


  public migrationProcessStarted() {
    this.migrationNotRunning = false;
    this.migrationStarted = true;
    this.migrationInProgress = true;
    this.migrationIsInPreview = false;
    this.migrationCompleted = false;
    this.migrationfailed = false;
    this.isCancelled = false;
  }

  public migrationIsInProcess(): void {
    this.migrationStarted = true;
    this.migrationInProgress = true;
    this.migrationIsInPreview = false;
    this.migrationCompleted = false;
    this.migrationfailed = false;
    this.isCancelled = false;
  }

  public migrationInPreview(): void {
    this.migrationStarted = true;
    this.migrationInProgress = true;
    this.migrationIsInPreview = true;
    this.migrationCompleted = false;
    this.migrationfailed = false;
    this.isCancelled = false;
  }

  public migrationIsCompleted(): void {
    this.migrationStarted = true;
    this.migrationInProgress = false;
    this.migrationIsInPreview = false;
    this.migrationCompleted = true;
    this.migrationfailed = false;
    this.isCancelled = false;
  }

  public migrationIsFailed(): void {
    this.migrationStarted = true;
    this.migrationInProgress = false;
    this.migrationIsInPreview = false;
    this.migrationCompleted = false;
    this.migrationfailed = true;
    this.isCancelled = false;
  }

  public migrationIsCancelled(): void {
    this.migrationStarted = true;
    this.migrationInProgress = false;
    this.migrationIsInPreview = false;
    this.migrationCompleted = false;
    this.migrationfailed = false;
    this.isCancelled = true;
  }

  public getKeyByValue(object: Record<string, string>, value: string) {
    return Number(Object.keys(object).find(key =>
      object[key] === value));
  }

  public currentMigrationProcess() {
    return `${this.migrationStatusPercentage.toFixed(0)}, 100`;
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

  // upload zip slider function
  public uploadZipFile(file: File): void {
    const formData: FormData = new FormData();
    if (file) {
      formData.append('server_id', this.server.id);
      formData.append('file', file);
    }
    const uploadZipPayload = {
      formData: formData
    };
    this.store.dispatch(new UploadZip( uploadZipPayload ));
    this.migrationIsInProcess();
  }

  // cancel migration function
  public cancelMigration(migration_id: string): void {
    const payload = {
      server_id : this.server.id,
      migration_id : migration_id
    };
    this.store.dispatch(new CancelMigration(payload));
  }

  // get migraion preview function
  public getPreviewData() {
    const payload = {
      migration_id: this.migration_id
    };
    this.store.dispatch(new GetPreviewData(payload));
  }

  public confirmPreview(migrationID: string) {
    this.confirmPreviewsubmit = true;
    const payload = {
      server_id: this.server.id,
      migration_id: migrationID
    };
    this.store.dispatch(new ConfirmPreview(payload));
  }
}
