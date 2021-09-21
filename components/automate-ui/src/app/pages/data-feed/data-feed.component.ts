import { Component, OnInit, OnDestroy, EventEmitter, ViewChild } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatOptionSelectionChange } from '@angular/material/core/option';
import { Store, select } from '@ngrx/store';
import { Observable, combineLatest, Subject } from 'rxjs';
import { filter, takeUntil, map } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Regex } from 'app/helpers/auth/regex';
import { HttpStatus, SortDirection } from 'app/types/types';
import { loading, EntityStatus, pending } from 'app/entities/entities';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { Destination } from 'app/entities/destinations/destination.model';
import {
  allDestinations,
  getStatus,
  saveStatus,
  saveError
} from 'app/entities/destinations/destination.selectors';
import {
  CreateDestination,
  GetDestinations,
  DeleteDestination,
  TestDestination,
  CreateDestinationPayload
} from 'app/entities/destinations/destination.actions';

import { DestinationRequests } from 'app/entities/destinations/destination.requests';
import {
  AuthTypes,
  DataFeedCreateComponent,
  IntegrationTypes,
  StorageIntegrationTypes,
  WebhookIntegrationTypes
} from '../data-feed-create/data-feed-create.component';

export enum UrlTestState {
  Inactive,
  Loading,
  Success,
  Failure
}

@Component({
  selector: 'app-data-feed',
  templateUrl: './data-feed.component.html',
  styleUrls: ['./data-feed.component.scss']
})

export class DataFeedComponent implements OnInit, OnDestroy {
  public loading$: Observable<boolean>;
  public sortedDestinations$: Observable<Destination[]>;
  public createModalVisible = false;
  public createDataFeedForm: FormGroup;
  public creatingDataFeed = false;
  public conflictErrorEvent = new EventEmitter<boolean>();
  public dataFeedToDelete: Destination;
  public deleteModalVisible = false;
  public sendingDataFeed = false;
  private isDestroyed = new Subject<boolean>();
  public checkedHeaders = false;

  @ViewChild(DataFeedCreateComponent) createChild: DataFeedCreateComponent;

  // The field or column used to sort nodes
  sortField$: Observable<string>;

  // The direction nodes are sorted
  fieldDirection$: Observable<SortDirection>;

  constructor(
    private store: Store<NgrxStateAtom>,
    private fb: FormBuilder,
    private layoutFacade: LayoutFacadeService,
    private datafeedRequests: DestinationRequests
  ) {

    this.loading$ = store.pipe(select(getStatus), map(loading));
    this.sortedDestinations$ = store.pipe(
      select(allDestinations));
    this.createDataFeedForm = this.fb.group({
      // Must stay in sync with error checks in create-data-feed-modal.component.html
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      endpoint: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      // Note that URL here may be FQDN -or- IP!
      url: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      tokenType: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      token: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      username: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      password: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      headers: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      bucketName: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      accessKey: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      secretKey: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]]
    });
  }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Settings);
    this.store.dispatch(new GetDestinations());
    this.store.pipe(
      select(saveStatus),
      takeUntil(this.isDestroyed),
      filter(state => this.createModalVisible && !pending(state)))
      .subscribe(state => {
        this.creatingDataFeed = false;
        if (state === EntityStatus.loadingSuccess) {
          this.closeSlider();
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
          this.createChild.conflictErrorSetter = `Could not create data feed: ${error?.error?.error || error}.`;
          this.conflictErrorEvent.emit(false);
        }
      });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public closeSlider() {
    this.createChild.closeCreateSlider();
    this.createChild.saveDone = false;
    this.createModalVisible = false;
    this.conflictErrorEvent.emit(false);
  }

  public startDataFeedDelete($event: MatOptionSelectionChange, destination: Destination): void {
    if ($event.isUserInput) {
      this.dataFeedToDelete = destination;
      this.deleteModalVisible = true;
    }
  }

  public startDataFeedSendTest($event: MatOptionSelectionChange, destination: Destination) {
    if ($event.isUserInput) {
      this.store.dispatch(new TestDestination({destination}));
    }
  }

  public deleteDataFeed(): void {
    this.closeDeleteModal();
    this.store.dispatch(new DeleteDestination(this.dataFeedToDelete));
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }

  public setCheck(event) {
    this.checkedHeaders = event;
  }

  public addHeadersforCustomDataFeed(customHeaders: string): {} {
        const headersJson = {};
        const headersVal = customHeaders.split('\n');
        for (const values in headersVal) {
          if (headersVal[values]) {
            const word = headersVal[values].split(':');
            headersJson[word[0]] = word[1];
          }
        }
        return headersJson;
  }


  public sendTestForDataFeed(event: {name: string, auth: string, region: string}): void {
    let testConnectionObservable: Observable<Object> = null;

    switch (event.name) {
      case WebhookIntegrationTypes.SERVICENOW:
      case WebhookIntegrationTypes.SPLUNK:
      case WebhookIntegrationTypes.ELK_KIBANA:
      case WebhookIntegrationTypes.CUSTOM: {
        switch (event.auth) {
          case AuthTypes.ACCESSTOKEN: {
            const targetUrl: string = this.createDataFeedForm.controls['url'].value;
            const tokenType: string = this.createDataFeedForm.controls['tokenType'].value;
            const token: string = this.createDataFeedForm.controls['token'].value;
            const headerVal: string = this.createDataFeedForm.controls['headers'].value;
            const userToken = JSON.stringify({
              Authorization: tokenType + ' ' + token
            });
            let value;
            if (headerVal && this.checkedHeaders) {
              const headersJson = this.addHeadersforCustomDataFeed(headerVal);
              const headers = {...headersJson, ...JSON.parse(userToken)};
              value = JSON.stringify(headers);
            } else {
              value = userToken;
            }
            testConnectionObservable = this.datafeedRequests.
                testDestinationWithHeaders(targetUrl, value);
            break;
          }
          case AuthTypes.USERNAMEANDPASSWORD: {
            const targetUrl: string =  this.createDataFeedForm.controls['url'].value;
            const targetUsername: string = this.createDataFeedForm.controls['username'].value;
            const targetPassword: string = this.createDataFeedForm.controls['password'].value;
            const headerVal: string = this.createDataFeedForm.controls['headers'].value;
            const userToken = JSON.stringify({
              Authorization: 'Basic ' + btoa(targetUsername + ':' + targetPassword)
            });
            let value;
            if (headerVal && this.checkedHeaders) {
              const headersJson = this.addHeadersforCustomDataFeed(headerVal);
              const headers = {...headersJson, ...JSON.parse(userToken)};
              value = JSON.stringify(headers);
            } else {
              value = userToken;
            }
            testConnectionObservable = this.datafeedRequests.
            testDestinationWithHeaders(targetUrl, value);
            break;
          }
        }
        break;
      }
      case StorageIntegrationTypes.MINIO: {
        // handling minio
        const targetUrl: string =
          this.createDataFeedForm.controls['endpoint'].value;
        const data = {
          url: targetUrl,
          aws: {
            access_key: this.createDataFeedForm.controls['accessKey'].value.trim(),
            secret_access_key: this.createDataFeedForm.controls['secretKey'].value.trim(),
            bucket: this.createDataFeedForm.controls['bucketName'].value.trim()
          }
        };
        testConnectionObservable = this.datafeedRequests.testDestinationForStorage(data);
          break;
      }
      case StorageIntegrationTypes.AMAZON_S3: {
        const data = {
          url: 'null',
          aws: {
            access_key: this.createDataFeedForm.controls['accessKey'].value.trim(),
            secret_access_key: this.createDataFeedForm.controls['secretKey'].value.trim(),
            bucket: this.createDataFeedForm.controls['bucketName'].value.trim(),
            region: event.region
          }
        };
        testConnectionObservable = this.datafeedRequests.testDestinationForStorage(data);
      }
    }
    if (testConnectionObservable != null) {
      testConnectionObservable.subscribe(
        () => this.revealUrlStatus(UrlTestState.Success),
        () => this.revealUrlStatus(UrlTestState.Failure)
      );
    }
  }

  revealUrlStatus(status: UrlTestState) {
    this.createChild.testDoneSetter = false;
    if (status === UrlTestState.Success) {
      this.createChild.testSuccessSetter = true;
    } else {
      this.createChild.testErrorSetter = true;
    }
  }

  public slidePanel(): void {
    this.createModalVisible = true;
    this.createChild.slidePanel();
  }

  public saveDestination(event: {name: string, auth: string, region: string}) {

    let destinationObj: CreateDestinationPayload,
        headers: string,
        storage: any;

    this.creatingDataFeed = true;

    switch (event.name) {
      case WebhookIntegrationTypes.SERVICENOW:
      case WebhookIntegrationTypes.SPLUNK:
      case WebhookIntegrationTypes.ELK_KIBANA:
      case WebhookIntegrationTypes.CUSTOM: {
        destinationObj = {
          name: this.createDataFeedForm.controls['name'].value.trim(),
          url: this.createDataFeedForm.controls['url'].value.trim(),
          integration_types: IntegrationTypes.WEBHOOK,
          services: event.name
        };
        switch (event.auth) {
          case AuthTypes.ACCESSTOKEN: {
            const tokenType: string = this.createDataFeedForm.controls['tokenType'].value.trim();
            const token: string = this.createDataFeedForm.controls['token'].value.trim();
            const headerVal: string = this.createDataFeedForm.controls['headers'].value;
            const userToken = JSON.stringify({
              Authorization: tokenType + ' ' + token
            });
            if (headerVal && this.checkedHeaders) {
              const headersJson = this.addHeadersforCustomDataFeed(headerVal);
              const value = {...headersJson, ...JSON.parse(userToken)};
              headers = JSON.stringify(value);
            } else {
              headers = userToken;
            }
            storage = null;
            break;
          }
          case AuthTypes.USERNAMEANDPASSWORD: {
            const username: string = this.createDataFeedForm.controls['username'].value.trim();
            const password: string = this.createDataFeedForm.controls['password'].value.trim();
            const headerVal: string = this.createDataFeedForm.controls['headers'].value;
            const userToken = JSON.stringify({
              Authorization: 'Basic ' + btoa(username + ':' + password)
            });
            if (headerVal && this.checkedHeaders) {
              const headersJson = this.addHeadersforCustomDataFeed(headerVal);
              const value = {...headersJson, ...JSON.parse(userToken)};
              headers = JSON.stringify(value);
            } else {
              headers = userToken;
            }
            storage = null;
            break;
          }
        }
        break;
      }
      case StorageIntegrationTypes.MINIO: {
        // handling minio
        destinationObj = {
          name: this.createDataFeedForm.controls['name'].value.trim(),
          url: this.createDataFeedForm.controls['endpoint'].value.trim(),
          integration_types: IntegrationTypes.STORAGE,
          services: event.name,
          meta_data: [
            {
              key: 'bucket',
              value: this.createDataFeedForm.controls['bucketName'].value.trim()
            }
          ]
        };
        const accessKey: string = this.createDataFeedForm.controls['accessKey'].value.trim();
        const secretKey: string = this.createDataFeedForm.controls['secretKey'].value.trim();
        storage = { accessKey, secretKey };
        headers = null;
        break;
      }
      case StorageIntegrationTypes.AMAZON_S3: {
        destinationObj = {
          name: this.createDataFeedForm.controls['name'].value.trim(),
          url: 'null',
          integration_types: IntegrationTypes.STORAGE,
          services: event.name,
          meta_data: [
            {
              key: 'bucket',
              value: this.createDataFeedForm.controls['bucketName'].value.trim()
            },
            {
              key: 'region',
              value: event.region
            }
          ]
        };
        const accessKey: string = this.createDataFeedForm.controls['accessKey'].value.trim();
        const secretKey: string = this.createDataFeedForm.controls['secretKey'].value.trim();
        storage = { accessKey, secretKey };
        headers = null;
        break;
      }
    }
    if (destinationObj && (headers || storage)) {
      this.store.dispatch(new CreateDestination(destinationObj, headers, storage));
    }
  }
}
