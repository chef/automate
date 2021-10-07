import { Component, OnInit, OnDestroy } from '@angular/core';
import { FormBuilder, FormGroup, Validators, FormControl } from '@angular/forms';
import { Store, select } from '@ngrx/store';
import { identity, isNil } from 'lodash/fp';
import { combineLatest, Subject } from 'rxjs';
import { filter, pluck, takeUntil } from 'rxjs/operators';

import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeParams } from 'app/route.selectors';
import { Regex } from 'app/helpers/auth/regex';
import { pending, EntityStatus } from 'app/entities/entities';
import {
  GetDestination,
  UpdateDestination,
  TestDestination ,
  EnableDisableDestination,
  DeleteDestination
} from 'app/entities/destinations/destination.actions';

import {
  destinationFromRoute,
  getStatus,
  updateStatus,
  destinationEnableStatus,
  deleteStatus,
  testConnectionStatus
} from 'app/entities/destinations/destination.selectors';
import { Destination, regions } from 'app/entities/destinations/destination.model';
import { Router } from '@angular/router';
import { trigger, state, animate, transition, style, keyframes } from '@angular/animations';
import { KVData } from 'app/entities/node-credentials/node-credential.model';

const fadeEnable = trigger('fadeEnable', [
   state('inactive', style({})),
   state('active', style({})),
   transition('inactive <=> active', [
    animate('0.3s', keyframes([
      style({transform: 'translateX(0%)'}),
      style({transform: 'translateX(100%)'}),
      style({transform: 'translateX(0%)'})
    ]))
   ])
]);

const fadeDisable = trigger('fadeDisable', [
  state('inactive', style({})),
  state('active', style({})),
  transition('inactive <=> active', [
   animate('0.3s', keyframes([
     style({transform: 'translateX(0%)'}),
     style({transform: 'translateX(-100%)'}),
     style({transform: 'translateX(0%)'})
   ]))
  ])
]);

type DestinationTabName = 'details';

enum UrlTestState {
  Inactive,
  Loading,
  Success,
  Failure
}
export enum WebhookIntegrationTypes {
  SERVICENOW = 'ServiceNow',
  SPLUNK = 'Splunk',
  ELK_KIBANA = 'ELK',
  CUSTOM = 'Custom'
}

export enum StorageIntegrationTypes {
  MINIO = 'Minio',
  AMAZON_S3 = 'S3'
}

export enum IntegrationTypes {
  WEBHOOK = 'Webhook',
  STORAGE = 'Storage'
}
@Component({
  selector: 'app-data-feed-details',
  templateUrl: './data-feed-details.component.html',
  styleUrls: ['./data-feed-details.component.scss'],
  animations: [fadeEnable, fadeDisable]
})

export class DataFeedDetailsComponent implements OnInit, OnDestroy {
  public tabValue: DestinationTabName = 'details';
  public destination: Destination;
  public updateForm: FormGroup;
  public saveInProgress = false;
  public testInProgress = false;
  public saveSuccessful = false;
  public hookStatus = UrlTestState.Inactive;
  private isDestroyed = new Subject<boolean>();
  public deleteModalVisible = false;
  public state = 'inactive';
  public regionSelected: string;
  public regionList = regions;
  public regionName: string;
  public serviceIntegrations = {
    webhook: {
      WebhookIntegrationTypes
    },
    storage: {
      StorageIntegrationTypes
    }
  };

  public integrations = {IntegrationTypes};
  constructor(
    private fb: FormBuilder,
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private router: Router
  ) { }

  ngOnInit(): void {
    this.layoutFacade.showSidebar(Sidebar.Settings);

    this.store.select(routeParams).pipe(
      pluck('id'),
      filter(identity),
      takeUntil(this.isDestroyed))
      .subscribe((id: string) => {
        this.store.dispatch(new GetDestination({ id }));
      });

    combineLatest([
      this.store.select(getStatus),
      this.store.select(destinationFromRoute)
    ]).pipe(
      filter(([status, destination]) =>
      status === EntityStatus.loadingSuccess && !isNil(destination)),
      takeUntil(this.isDestroyed))
      .subscribe(([_, destination]) => {
        this.destination = destination;
        this.updateForm.controls.name.setValue(this.destination.name);
        this.updateForm.controls.url.setValue(this.destination.url);
        if (this.destination.integration_types === IntegrationTypes.STORAGE) {
          this.destination.meta_data.forEach((v) => {
            if (v.key === 'bucket') {
              this.updateForm.controls.bucket.setValue(v.value);
            }
            if (v.key === 'region') {
              this.regionName = v.value;
              this.regionSelected = v.value;
            }
          });
        }
      });

    this.updateForm = this.fb.group({
      // Must stay in sync with error checks in data-feed-details.component.html
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      // Note that URL here may be FQDN -or- IP!
      url: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      bucket: this.destination?.services === StorageIntegrationTypes.AMAZON_S3 ?
        ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]] : null
    });

    this.store.pipe(
      select(updateStatus),
      takeUntil(this.isDestroyed),
      filter(status => this.saveInProgress && !pending(status)))
      .subscribe((res) => {
        this.saveInProgress = false;
        this.saveSuccessful = (res === EntityStatus.loadingSuccess);
        if (this.saveSuccessful) {
          this.updateForm.markAsPristine();
        }
      });
  }
  selectChangeHandlers(region: string): void {
    this.regionSelected = region;
  }
  metaDataValue(): Array<KVData> {
    if (this.destination.integration_types === IntegrationTypes.STORAGE ) {
      if (this.destination.services === StorageIntegrationTypes.AMAZON_S3 ) {
        return Array<KVData>(
          {
            key: 'bucket', value: this.updateForm.controls['bucket'].value.trim()
          },
          {
            key: 'region', value: this.regionSelected
          }
        );
      }
      if (this.destination.services === StorageIntegrationTypes.MINIO ) {
        return Array<KVData>(
          {
            key: 'bucket', value: this.updateForm.controls['bucket'].value.trim()
          }
        );
      }
    }
  }

  public saveDataFeed(): void {
    this.saveSuccessful = false;
    this.saveInProgress = true;
    const destinationObj = {
      id: this.destination.id,
      name: this.updateForm.controls['name'].value.trim(),
      url: this.updateForm.controls['url'].value.trim(),
      secret: this.destination.secret,
      enable: this.destination.enable,
      integration_types: this.destination.integration_types,
      meta_data:
        this.destination.integration_types === IntegrationTypes.STORAGE ?
          this.metaDataValue() : this.destination.meta_data,
      services: this.destination.services
    };
    this.store.dispatch(new UpdateDestination({ destination: destinationObj }));
    this.destination = destinationObj;
  }

  public sendTestForDataFeedUrl(): void {
    this.testInProgress = true;
    const destinationObj = {
      ...this.destination,
      name: this.updateForm.controls['name'].value.trim(),
      url: this.updateForm.controls['url'].value.trim(),
      secret: this.destination.secret,
      enable: this.destination.enable,
      integration_types: this.destination.integration_types,
      meta_data:
        this.destination.integration_types === IntegrationTypes.STORAGE ?
          this.metaDataValue() : this.destination.meta_data,
      services: this.destination.services
    };
    this.store.dispatch(new TestDestination({destination: destinationObj}));
    this.store.pipe(
      select(testConnectionStatus),
      takeUntil(this.isDestroyed),
      filter(status => !pending(status)))
      .subscribe(res => {
        if (res === EntityStatus.loadingSuccess || EntityStatus.loadingFailure) {
          this.testInProgress = false;
        }
      });
  }

  public get nameCtrl(): FormControl {
    return <FormControl>this.updateForm.controls.name;
  }
  public get bucketCtrl(): FormControl {
    return <FormControl>this.updateForm.controls.bucket;
  }

  public get urlCtrl(): FormControl {
    return <FormControl>this.updateForm.controls.url;
  }
  public enableDestination(val: boolean) {
    const destinationEnableObj = {
      id: this.destination.id,
      enable: val
    };
    this.store.dispatch(new EnableDisableDestination({enableDisable: destinationEnableObj}));
    this.store.pipe(
      select(destinationEnableStatus),
      takeUntil(this.isDestroyed),
      filter(status => !pending(status)))
      .subscribe(res => {
        if (res === EntityStatus.loadingSuccess) {
          this.state = this.state === 'active' ? 'inactive' : 'active';
          this.destination.enable = val;
        }
      });
  }
  public deleteDataFeed() {
    this.store.dispatch(new DeleteDestination(this.destination));
    this.store.pipe(
      select(deleteStatus),
      takeUntil(this.isDestroyed),
      filter(status => !pending(status)))
      .subscribe(res => {
        if (res === EntityStatus.loadingSuccess) {
          this.router.navigate(['/settings/data-feeds']);
        }
      });
  }
  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }
  public openDeleteModal(): void {
    this.deleteModalVisible = true;
  }
  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
  public cancel(): void {
    this.router.navigate(['/settings/data-feeds']);
  }
  public disableOnsave(service: string) {
    const isDisable = this.saveInProgress
    || !this.updateForm.valid
    || !this.updateForm.dirty
    || !this.destination?.enable;
    if (service === StorageIntegrationTypes.AMAZON_S3 ) {
      return isDisable && this.regionSelected === this.regionName;
    } else {
      return isDisable ;
    }

  }

  public showUrl() {
    return !(this.destination?.integration_types === IntegrationTypes.STORAGE)
    || this.destination?.services === StorageIntegrationTypes.MINIO;
  }
  public enableBtn() {
    return !this.destination?.enable ? 'is-enable enable-btn' : 'is-disable enable-btn';
  }
  public disableBtn() {
    return this.destination?.enable ? 'is-enable disable-btn' : 'is-disable disable-btn';
  }
}

