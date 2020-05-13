import { Component, OnInit, OnDestroy, EventEmitter } from '@angular/core';
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
  TestDestination
} from 'app/entities/destinations/destination.actions';

import { DestinationRequests } from 'app/entities/destinations/destination.requests';

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
  public hookStatus = UrlTestState.Inactive;
  private isDestroyed = new Subject<boolean>();

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
      url: ['', [Validators.required,
        Validators.pattern(Regex.patterns.NON_BLANK),
        Validators.pattern(Regex.patterns.VALID_FQDN)
      ]],
      username: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      password: ['', [Validators.required,
        Validators.pattern(Regex.patterns.NON_BLANK)
      ]]
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
          this.closeCreateModal();
          this.hookStatus = UrlTestState.Inactive;
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
          this.hookStatus = UrlTestState.Inactive;
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

  public createDataFeed(): void {
    this.creatingDataFeed = true;
    const destinationObj = {
      name: this.createDataFeedForm.controls['name'].value.trim(),
      url: this.createDataFeedForm.controls['url'].value.trim()
    };
    const username: string = this.createDataFeedForm.controls['username'].value.trim();
    const password: string = this.createDataFeedForm.controls['password'].value.trim();
    this.store.dispatch(new CreateDestination(destinationObj, username, password));
  }

  private resetCreateModal(): void {
    this.hookStatus = UrlTestState.Inactive;
    this.creatingDataFeed = false;
    this.createDataFeedForm.reset();
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

  public sendTestForDataFeed(): void {
    this.sendingDataFeed = true;
    this.hookStatus = UrlTestState.Loading;
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
    this.sendingDataFeed = false;
  }

  private revealUrlStatus(status: UrlTestState) {
    this.hookStatus = status;
  }
}
