import { Component, OnInit, OnDestroy, EventEmitter, ViewChild } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatOptionSelectionChange } from '@angular/material/core/option';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { Subject, combineLatest, Subscription } from 'rxjs';
import { filter, pluck, takeUntil } from 'rxjs/operators';
import { identity, isNil } from 'lodash/fp';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeParams, routeURL } from 'app/route.selectors';
import { Regex } from 'app/helpers/auth/regex';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { pending, EntityStatus } from 'app/entities/entities';
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
  ValidateWebUIKey,
  GetUsers
} from 'app/entities/servers/server.actions';
import {
  GetOrgs,
  DeleteOrg,
  UploadZip,
  CancelMigration,
  GetPreviewData,
  ConfirmPreview
} from 'app/entities/orgs/org.actions';
import { Org, User } from 'app/entities/orgs/org.model';
import {
  allOrgs,
  getAllStatus as getOrgsStatus,
  deleteStatus as deleteOrgStatus,
  uploadStatus,
  uploadDetails,
  cancelStatus,
  previewStatus,
  previewData,
  confirmPreviewStatus
} from 'app/entities/orgs/org.selectors';
import { ProjectConstants } from 'app/entities/projects/project.model';
import { SyncOrgUsersSliderComponent } from '../sync-org-users-slider/sync-org-users-slider.component';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

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
  public conflictErrorEvent = new EventEmitter<boolean>();
  public orgToDelete: Org;
  public deleteModalVisible = false;
  public saveSuccessful = false;
  public saveInProgress = false;
  public orgsListLoading = true;

  private server_id: string;
  private isDestroyed = new Subject<boolean>();

  // isLoading represents the initial load as well as subsequent updates in progress.
  public isLoading = true;
  public unassigned = ProjectConstants.UNASSIGNED_PROJECT_ID;
  public selected = 'fqdn';
  public isUserListLoaded = false;
  public users;
  public usersListLoading: boolean;
  public authFailure = false;
  public isValid = false;
  public isServerLoaded = false;
  public validating = true;

  // used for webuikey
  public updateWebuiKeyForm: FormGroup;
  public updatingWebuiKey = false;
  public webuiKey: WebUIKey;
  public updateWebUIKeySuccessful = false;

  // used for the preview migration step
  public uploadZipForm: FormGroup;
  public isUploaded = false;
  public migrationStatus: MigrationStatus;
  public migrationStatusPercentage = 0;
  public stepsCompleted: string;
  public totalMigrationSteps = 13;
  public migrationStepValue: number;

  // migration steps
  public migrationLoading = true;
  public migrationInProgress = false;
  public migrationIsInPreview = false;
  public migrationCompleted = false;
  public migrationFailed = false;
  public migrationCancelled = false;
  public migrationNotRunning = true;

  // migration values
  public migration_id = '';
  public migration_type = '';
  public migration_status = '';

  // migration preview data
  public previewData;
  public isPreview = false;
  public confirmPreviewSuccessful = false;
  public cancelMigrationSuccessful = false;
  public confirmPreviewsubmit = false;
  public checkMigrationStatus: Subscription;
  public myInterval: ReturnType<typeof setInterval>;
  public orgDataLoaded = false;
  public orgData: Org[] = [];
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

    this.store.select(routeParams).pipe(
      pluck('id'),
      filter(identity),
      takeUntil(this.isDestroyed))
      .subscribe((server_id: string) => {
        this.server_id = server_id;
      });


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
          // call to get the list of users
          this.getListOfUsers(this.server_id);
          break;
        case 'orgs':
          this.tabValue = 'orgs';
          break;
      }
    });

    // validations
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

    // Call to get server and orgs
    if (this.tabValue !== 'users') {
      setTimeout( () => {
        this.getServerAndOrgs();
      }, 2000 );
    }

    // Get deleted org status
    this.store.select(deleteOrgStatus).pipe(
      filter(status => this.server_id !== undefined && status === EntityStatus.loadingSuccess),
      takeUntil(this.isDestroyed))
      .subscribe(() => {
        this.store.dispatch(new GetOrgs({ server_id: this.server_id })
      );
    });

    // Get updated server status
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

    // Get update webuikey status
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

    // Get upload migration status
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
        setTimeout(() => { this.getMigrationStatus(this.migration_id); }, 1000);
      } else if (uploadStatusSt === EntityStatus.loadingFailure) {
        // close upload slider with error notification
        this.isUploaded = false;
        setTimeout(() => { this.migrationIsFailed(); }, 1000);
        this.upload.closeUploadSlider();
      }
    });

    // Get cancel migration status
    this.store.select(cancelStatus).pipe(
      takeUntil(this.isDestroyed),
      filter(state => !pending(state)))
    .subscribe((state) => {
      this.cancelMigrationSuccessful = (state === EntityStatus.loadingSuccess);
      if (this.cancelMigrationSuccessful) {
        setTimeout(() => { this.migrationIsCancelled(); }, 1000);
      } else {
        setTimeout(() => { this.migrationIsInProcess(); }, 1000);
      }
    });

    //  Get preview migration status
    combineLatest([
      this.store.select(previewStatus),
      this.store.select(previewData)
    ]).pipe(takeUntil(this.isDestroyed))
    .subscribe(([previewStatusSt, previewState]) => {
      if (previewStatusSt === EntityStatus.loadingSuccess && !isNil(previewState)) {
        this.previewData = previewState;
        this.isPreview = true;
      }
    });

    // Get confirm preview status
    this.store.select(confirmPreviewStatus).pipe(
    takeUntil(this.isDestroyed),
    filter(state => this.migrationIsInPreview && !pending(state)))
    .subscribe((state) => {
      this.confirmPreviewSuccessful = (state === EntityStatus.loadingSuccess);
      if (this.confirmPreviewSuccessful) {
        this.confirmPreviewsubmit = true;
        this.migrationLoading = false;
        this.orgsListLoading = true;
        setTimeout( () => {
          // call to get the list of orgs
          this.getListofOrgs(this.server_id);
        }, 2000 );
      } else {
        this.confirmPreviewsubmit = false;
      }
    });

    // Get the running migration status
    this.callToGetMigrationStatus();
  }

  ngOnDestroy(): void {
    this.isDestroyed.next();
    this.isDestroyed.complete();
    this.checkMigrationStatus?.unsubscribe();
    clearInterval(this.myInterval);
  }

  onSelectedTab(event: { target: { value: ChefServerTabName } }) {
    this.tabValue = event.target.value;
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
  }

  updateFormDisplay(id: string): void {
    this.selected = id;
  }

  // get server and orgs
  public getServerAndOrgs(): void {
    // call to get the server details
    this.getServerDetails(this.server_id);
    // call to get the list of orgs
    this.getListofOrgs(this.server_id);

    if (this.migration_id !== '') {
      setTimeout(() => { this.migrationProcessStarted(); }, 1000);
      this.getMigrationStatus(this.migration_id);
    }

    // Call to check the webuikey valid or not
    setTimeout(() => {
      if (this.isServerLoaded) {
        this.validateWebUIKey(this.server);
      }
    }, 1000);
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

  // get server details
  private getServerDetails(server_id: string): void {
    this.store.dispatch(new GetServer({ server_id }));
    combineLatest([
      this.store.select(getStatus),
      this.store.select(serverFromRoute)
    ]).pipe(takeUntil(this.isDestroyed))
      .subscribe(([getServerStatus, serverState]) => {
        if (getServerStatus === EntityStatus.loadingSuccess && !isNil(serverState)) {
          this.server = { ...serverState };
          this.updateServerForm.controls['name'].setValue(this.server.name);
          this.fqdnForm.controls['fqdn'].setValue(this.server.fqdn);
          this.ipForm.controls['ip_address'].setValue(this.server.ip_address);
          this.isServerLoaded = true;
          this.migration_id = this.server?.migration_id;
          console.log(this.migration_id);
          this.migration_status = this.server?.migration_status
          this.migration_type = this.server?.migration_type;
          
          // call migration status single time to load the last status
          if (this.migration_id !== '') {
            this.migrationLoading = true;
            console.log('called')
            setTimeout(() => { this.getMigrationStatus(this.migration_id); }, 2000);
          }
        }
    });
  }

  // get list of orgs
  private getListofOrgs(server_id: string): void {
    this.store.dispatch(new GetOrgs({ server_id: server_id }));
    combineLatest([
      this.store.select(getOrgsStatus),
      this.store.select(allOrgs)
    ]).pipe(takeUntil(this.isDestroyed))
      .subscribe(([_getOrgsStatus, allOrgsState]) => {
        if (_getOrgsStatus === EntityStatus.loadingSuccess && !isNil(allOrgsState)) {
          this.orgs = allOrgsState;
          this.orgsListLoading = false;
        } else {
          this.orgs = [];
        }
    });
  }

  // get list of users
  private getListOfUsers(server_id: string): void {
    this.store.dispatch(new GetUsers({ server_id: server_id }));
    // Get users status
    combineLatest([
      this.store.select(getUsersStatus),
      this.store.select(getUsers)
    ]).pipe(takeUntil(this.isDestroyed))
    .subscribe(([getUsersSt, UsersState]) => {
      if (getUsersSt === EntityStatus.loadingSuccess && !isNil(UsersState)) {
        this.users = UsersState;
        this.usersListLoading = false;
        this.users.users.length > 0 ? this.isUserListLoaded = true : this.isUserListLoaded = false;
      } else if (getUsersSt === EntityStatus.loadingFailure) {
        this.usersListLoading = false;
        this.authFailure = true;
        this.isUserListLoaded = false;
      }
      this.orgsListLoading = false;
    });
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
  public getMigrationStatus(migration_id: string): void {
    this.store.dispatch(new GetMigrationStatus(migration_id));
    this.checkMigrationStatus = combineLatest([
      this.store.select(migrationStatus),
      this.store.select(getMigrationStatus)
    ]).pipe(
        takeUntil(this.isDestroyed)
      ).subscribe(([getMigrationSt, migrationState]) => {
        if (getMigrationSt === EntityStatus.loadingSuccess && !isNil(migrationState)) {
          this.migrationStatus = migrationState;
          this.migration_type = this.migrationStatus.migration_type;
          this.migration_status = this.migrationStatus.migration_status;

          console.log('migration status')
          console.log(this.migration_type)
          console.log(this.migration_status)
          // case for the migration is in progress/creating_peview
          if (this.migration_status === 'Completed' || this.migration_status === 'In Progress') {
            this.migrationStepValue = this.getKeyByValue(this.migrationSteps, this.migration_type);
            this.migrationStatusPercentage =
              Number((this.migrationStepValue / this.totalMigrationSteps) * 100);
            this.migrationIsInProcess();
            this.stepsCompleted =  this.migrationStepValue.toFixed(0) + '/' + '13';
            if (this.migration_type === 'Creating Preview'
              && this.migrationCancelled === false
              && this.migrationFailed === false) {
              console.log("migration in progress");
              this.migrationInPreview();
            }
          }

          // case for the migration completed
          if (this.migration_type === 'Migration Completed' && this.migration_status ===  'Completed') {
            console.log("migration completed");
            this.migrationIsCompleted();
          }

          // case for the migration cancelled
          if (this.migration_type === 'Migration Cancelled' && this.cancelMigrationSuccessful) {
            console.log("migration cancelled");
            this.migrationIsCancelled();
          }

          // case for the migration failed
          if (this.migration_status ===  'Failed' &&
                this.migration_type === 'Migration Completed') {
            console.log("migration failed");
            this.migrationIsFailed();
          }
        }
      });

      // if (this.migration_type !== ''  && this.migration_status !== '') {
      //   this.checkMigrationStatus.unsubscribe();
      // }
      this.migrationLoading = false;
  }

  public migrationProcessStarted() {
    this.migrationInProgress = true;
    this.migrationIsInPreview = false;
    this.migrationCompleted = false;
    this.migrationFailed = false;
    this.migrationCancelled = false;
    this.migrationNotRunning = false;
  }

  public migrationIsInProcess(): void {
    this.migrationInProgress = true;
    this.migrationIsInPreview = false;
    this.migrationCompleted = false;
    this.migrationFailed = false;
    this.migrationCancelled = false;
    this.migrationNotRunning = false;
  }

  public migrationInPreview(): void {
    this.migrationInProgress = true;
    this.migrationIsInPreview = true;
    this.migrationCompleted = false;
    this.migrationFailed = false;
    this.migrationCancelled = false;
    this.migrationNotRunning = false;
  }

  public migrationIsCompleted(): void {
    if (!this.migrationCancelled) {
      this.migrationInProgress = false;
      this.migrationIsInPreview = false;
      this.migrationCompleted = true;
      this.migrationFailed = false;
      this.migrationCancelled = false;
      this.migrationNotRunning = true;
      this.checkMigrationStatus.unsubscribe();
      clearInterval(this.myInterval);
    }
  }

  public migrationIsFailed(): void {
    this.migrationInProgress = false;
    this.migrationIsInPreview = false;
    this.migrationCompleted = false;
    this.migrationFailed = true;
    this.migrationCancelled = false;
    this.migrationNotRunning = true;
    this.checkMigrationStatus.unsubscribe();
    clearInterval(this.myInterval);
  }

  public migrationIsCancelled(): void {
    this.migrationInProgress = false;
    this.migrationIsInPreview = false;
    this.migrationCompleted = false;
    this.migrationFailed = false;
    this.migrationCancelled = true;
    this.migrationNotRunning = true;
    this.checkMigrationStatus.unsubscribe();
    clearInterval(this.myInterval);
  }

  public callToGetMigrationStatus() {
    this.myInterval =  setInterval(() => {
      if (this.migration_id !== '' && this.migration_type !== 'Migration Completed') {
        this.getMigrationStatus(this.migration_id);
      }
    }, 5000);
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
      id: this.server_id,
      webui_key: this.updateWebuiKeyForm.controls['webUiKey'].value
    };
    this.updatingWebuiKeyData(this.webuiKey);
    this.updateWebuiKeyForm.reset();
    this.telemetryService.track('InfraServer_ChefInfraServer_Update_UpdateWebUIKey');
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
  }

  // cancel migration function
  public cancelMigration(migration_id: string): void {
    const payload = {
      server_id : this.server.id,
      migration_id : migration_id
    };
    this.store.dispatch(new CancelMigration(payload));
    this.telemetryService.track('InfraServer_ChefInfraServer_ClickToPreview_CancelMigration');
  }

  // get migraion preview function
  public getPreviewData() {
    const payload = {
      migration_id: this.migration_id
    };
    this.store.dispatch(new GetPreviewData(payload));
  }

  public confirmPreview(usersData: User[]) {
    this.confirmPreviewsubmit = true;
    this.previewData.staged_data.users = usersData;
    this.previewData.migration_id = this.migration_id;
    const payload = {
      server_id: this.server.id,
      previewData: this.previewData
    };
    this.store.dispatch(new ConfirmPreview(payload));
    this.telemetryService.track('InfraServer_ChefInfraServer_ClickToPreview_ConfirmMigration');
  }

  // public displayMigrationStatus(type: string) {

  //   switch (type) {
  //     case 'migration_not_started':
  //       console.log("not started");
  //       this.migration_id == '' ? true : false;
  //       break;
  //     case 'running':
  //       console.log("running");
  //       this.migrationCancelled ? true : false;
  //       break;
  //     case 'completed':
  //       console.log("completed");
  //       if (this.migration_status == "Completed"
  //             && this.migration_type ==  "Migration Completed") {
  //         true;
  //       } else {
  //         false;
  //       }
  //       break;
  //     case 'cancelled':
  //       console.log("cancelled");
  //       this.migrationCancelled ? true : false
  //       break;
  //     case 'failed':
  //       console.log("failed");
  //       this.migrationFailed ? true : false;
  //       break;
  //     default:
  //       false
  //       break;
  // }

  //   // if (type == 'migration_not_started' && this.migration_id == '') {
  //   //   true
  //   // }

  //   // if (type == 'running' && this.migrationCancelled) {
  //   //   true
  //   // }

  //   // if (type == 'completed' && this.migration_status == "Completed"
  //   //       && this.migration_type ==  "Migration Completed") {
  //   //   true
  //   // }

  //   // if (type == 'cancelled' && this.migrationCancelled) {
  //   //   true
  //   // }

  //   // if (type == 'failed' && this.migrationFailed) {
  //   //   true
  //   // }

  // }
}
