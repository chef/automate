import { Component, OnInit, OnDestroy, EventEmitter } from '@angular/core';
import { Store, select } from '@ngrx/store';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { filter, takeUntil, map } from 'rxjs/operators';
import { Regex } from 'app/helpers/auth/regex';
import { Observable, combineLatest, Subject } from 'rxjs';
import { isNil } from 'lodash/fp';
import { HttpStatus } from 'app/types/types';
import { loading, EntityStatus, pending } from 'app/entities/entities';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { ChefKeyboardEvent } from 'app/types/material-types';
import { Destination } from '../../entities/destinations/destination.model';
import {
  allDestinations, getStatus, saveStatus, saveError } from 'app/entities/destinations/destination.selectors';
import { CreateDestination, GetDestinations, DeleteDestination } from 'app/entities/destinations/destination.actions';
import { ChefSorters } from 'app/helpers/auth/sorter';
import { DatafeedService } from 'app/services/data-feed/data-feed.service';

enum UrlTestState {
  Inactive,
  Loading,
  Success,
  Failure
}

type Modal = 'url';

@Component({
  selector: 'app-data-feed',
  templateUrl: './data-feed.component.html',
  styleUrls: ['./data-feed.component.scss']
})
export class DatafeedComponent implements OnInit, OnDestroy {
  public loading$: Observable<boolean>;
  public sortedDestinations$: Observable<Destination[]>;
  public createModalVisible = false;
  public createDataFeedForm: FormGroup;
  public creatingDataFeed = false;
  public conflictErrorEvent = new EventEmitter<boolean>();
  private isDestroyed = new Subject<boolean>();
  public dataFeedToDelete: Destination;
  public deleteModalVisible = false;
  public sendingDataFeed = false;
  public urlState = UrlTestState;
  public hookStatus = UrlTestState.Inactive;
  public urlStatusModalVisible = false;
  constructor(
    private store: Store<NgrxStateAtom>,
    private fb: FormBuilder,
    private layoutFacade: LayoutFacadeService,
    private datafeedService: DatafeedService
  ) {

    this.loading$ = store.pipe(select(getStatus), map(loading));
    this.sortedDestinations$ = store.pipe(
      select(allDestinations),
      map(destinations => ChefSorters.naturalSort(destinations, 'name')));
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
    const destinationObj = new Destination(undefined, '', '', '');
    destinationObj.name = this.createDataFeedForm.controls['username'].value.trim();
    destinationObj.url = this.createDataFeedForm.controls['url'].value.trim();
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

  public startDataFeedDelete($event: ChefKeyboardEvent, destination: Destination): void {
    if ($event.isUserInput) {
      this.dataFeedToDelete = destination;
      this.deleteModalVisible = true;
    }
  }

  public startDataFeedSendTest($event: ChefKeyboardEvent, destination: any) {
    if ($event.isUserInput) {
      this.hookStatus = UrlTestState.Loading;
      if (destination) {
        this.datafeedService.testDestinationWithSecretId(destination.url,
          destination.secret)
          .subscribe(
            () => this.revealUrlStatusUsingSecretId(UrlTestState.Success),
            () => this.revealUrlStatusUsingSecretId(UrlTestState.Failure)
          );
      }
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
      this.datafeedService.testDestinationWithUsernamePassword(targetUrl,
        targetUsername, targetPassword).subscribe(
          () => this.revealUrlStatus(UrlTestState.Success),
          () => this.revealUrlStatus(UrlTestState.Failure)
        );
    } else {
      this.datafeedService.testDestinationWithNoCreds(targetUrl)
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

  private revealUrlStatusUsingSecretId(status: UrlTestState) {
    this.hookStatus = status;
    this.openModal('url');
  }

  public openModal(type: Modal): void {
    switch (type) {
      case 'url':
        this.urlStatusModalVisible = true;
        return;
      default:
        return;
    }
  }

  public closeModal(type: Modal): void {
    switch (type) {
      case 'url':
        this.urlStatusModalVisible = false;
        return;
      default:
        return;
    }
  }
}
