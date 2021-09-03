import { Component, OnInit, OnDestroy, EventEmitter, ViewChild } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatOptionSelectionChange } from '@angular/material/core/option';
import { Store, select } from '@ngrx/store';
import { Observable, combineLatest, Subject } from 'rxjs';
import { filter, takeUntil, map } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Regex } from 'app/helpers/auth/regex';
import { HttpStatus } from 'app/types/types';
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
import { AuthTypes, DataFeedCreateComponent, StorageIntegrationTypes, WebhookIntegrationTypes } from '../data-feed-create/data-feed-create.component';

enum UrlTestState {
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

  @ViewChild(DataFeedCreateComponent) createChild: DataFeedCreateComponent;

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
      endpoint: ['', [Validators.required, Validators.pattern(Regex.patterns.VALID_FQDN)]],
      // Note that URL here may be FQDN -or- IP!
      url: ['', [Validators.required, Validators.pattern(Regex.patterns.VALID_FQDN)]],
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
          // Close the slider on any error other than conflict and display in banner.
          // this.closeSlider();
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

  public sendTestForDataFeed(event: any): void {
    this.sendingDataFeed = true;
    if (event.name === WebhookIntegrationTypes.SERVICENOW ||
      event.name === WebhookIntegrationTypes.SPLUNK ||
      event.name === WebhookIntegrationTypes.ELK_KIBANA) {
      if (event.auth === AuthTypes.ACCESSTOKEN) {
        const targetUrl: string = this.createDataFeedForm.controls['url'].value;
        const tokenType: string = this.createDataFeedForm.controls['tokenType'].value;
        const token: string = this.createDataFeedForm.controls['token'].value;
        const value = JSON.stringify({
          Authorization: tokenType + ' ' + token
        });
        this.datafeedRequests.testDestinationWithHeaders(targetUrl,
          value).subscribe(
            () => this.revealUrlStatus(UrlTestState.Success),
            () => this.revealUrlStatus(UrlTestState.Failure)
          );
      } else if (event.auth === AuthTypes.USERNAMEANDPASSWORD) {
        const targetUrl: string =  this.createDataFeedForm.controls['url'].value;
        const targetUsername: string = this.createDataFeedForm.controls['username'].value;
        const targetPassword: string = this.createDataFeedForm.controls['password'].value;
        if (targetUrl && targetUsername && targetPassword) {
          this.datafeedRequests.testDestinationWithUsernamePassword(targetUrl,
            targetUsername, targetPassword).subscribe(
              () => this.revealUrlStatus(UrlTestState.Success),
              () => this.revealUrlStatus(UrlTestState.Failure)
            );
        } else {
          this.datafeedRequests.testDestinationWithNoCreds(targetUrl)
            .subscribe(
              () => this.revealUrlStatus(UrlTestState.Success),
              () => this.revealUrlStatus(UrlTestState.Failure)
            );
        }
      }
    } else if (event.name === WebhookIntegrationTypes.CUSTOM) {

    } else if (event.name === StorageIntegrationTypes.MINIO) {
      const targetUrl: string = this.createDataFeedForm.controls['endpoint'].value;
      // const tokenType: string = this.createDataFeedForm.controls['tokenType'].value;
      // const token: string = this.createDataFeedForm.controls['token'].value;
      // const value = JSON.stringify({
      //   Authorization: tokenType + ' ' + token
      // });
      const data = {
        url: targetUrl,
        aws: {
          access_key: this.createDataFeedForm.controls['accessKey'].value.trim(),
          secret_access_key: this.createDataFeedForm.controls['secretKey'].value.trim(),
          bucket: this.createDataFeedForm.controls['bucketName'].value.trim()
        }
      };

      this.datafeedRequests.testDestinationForMinio(data)
        .subscribe(
          () => this.revealUrlStatus(UrlTestState.Success),
          () => this.revealUrlStatus(UrlTestState.Failure)
        );
  }
    this.sendingDataFeed = false;
  }

  private revealUrlStatus(status: UrlTestState) {
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

  public saveDestination(event: any) {

    let destinationObj: CreateDestinationPayload,
        headers: string,
        storage: any;

    this.creatingDataFeed = true;
    if (event.name === WebhookIntegrationTypes.SERVICENOW ||
      event.name === WebhookIntegrationTypes.SPLUNK ||
      event.name === WebhookIntegrationTypes.ELK_KIBANA) {
      if (event.auth === AuthTypes.ACCESSTOKEN) {
        destinationObj = {
          name: this.createDataFeedForm.controls['name'].value.trim(),
          url: this.createDataFeedForm.controls['url'].value.trim(),
          integration_types: 'Webhook',
          services: event.name
        };
        const tokenType: string = this.createDataFeedForm.controls['tokenType'].value.trim();
        const token: string = this.createDataFeedForm.controls['token'].value.trim();
        headers = JSON.stringify({
          Authorization: tokenType + ' ' + token
        });
        storage = null;

      } else if (event.auth === 'Username and Password') {
        destinationObj = {
          name: this.createDataFeedForm.controls['name'].value.trim(),
          url: this.createDataFeedForm.controls['url'].value.trim(),
          integration_types: 'Webhook',
          services: event.name
        };
        const username: string = this.createDataFeedForm.controls['username'].value.trim();
        const password: string = this.createDataFeedForm.controls['password'].value.trim();
        headers = JSON.stringify({
          Authorization: 'Basic ' + btoa(username + ':' + password)
        });
        storage = null;
      }
    } else if (event.name === WebhookIntegrationTypes.CUSTOM) {

    } else if (event.name === StorageIntegrationTypes.MINIO) {
        // if (this.createForm.get('name').valid && this.createForm.get('endpoint').valid &&
        //   this.createForm.get('bucketName').valid && this.createForm.get('accessKey').valid &&
        //   this.createForm.get('secretKey').valid) {
        //   return true;
        // }
        destinationObj = {
          name: this.createDataFeedForm.controls['name'].value.trim(),
          url: this.createDataFeedForm.controls['endpoint'].value.trim(),
          integration_types: 'Storage',
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
        storage = {accessKey, secretKey};
        headers = null;
    }

    this.store.dispatch(new CreateDestination(destinationObj, headers, storage));
  }
}
