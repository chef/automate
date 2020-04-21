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
import { GetDestination, UpdateDestination } from 'app/entities/destinations/destination.actions';
import { destinationFromRoute, getStatus, updateStatus } from 'app/entities/destinations/destination.selectors';
import { Destination } from 'app/entities/destinations/destination.model';
import { DatafeedService } from 'app/services/data-feed/data-feed.service';

type DestinationTabName = 'details';

enum UrlTestState {
  Inactive,
  Loading,
  Success,
  Failure
}

type Modal = 'url';

@Component({
  selector: 'app-data-feed-details',
  templateUrl: './data-feed-details.component.html',
  styleUrls: ['./data-feed-details.component.scss']
})
export class DataFeedDetailsComponent implements OnInit, OnDestroy {
  public tabValue: DestinationTabName = 'details';
  public destination: Destination;
  private isDestroyed = new Subject<boolean>();
  public updateForm: FormGroup;
  public saveInProgress = false;
  public testInProgress = false;
  public saveSuccessful = false;
  public urlState = UrlTestState;
  public hookStatus = UrlTestState.Inactive;
  public urlStatusModalVisible = false;

  constructor(
    private fb: FormBuilder,
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private datafeedService: DatafeedService
  ) { }

  public ngOnInit(): void {
    this.layoutFacade.showSidebar(Sidebar.Settings);

    this.updateForm = this.fb.group({
      // Must stay in sync with error checks in data-feed-details.component.html
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      url: ['', [Validators.required,
        Validators.pattern(Regex.patterns.NON_BLANK),
        Validators.pattern(Regex.patterns.VALID_FQDN)
      ]]
    });

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
      });

    this.store.pipe(
      select(updateStatus),
      takeUntil(this.isDestroyed),
      filter(state => this.saveInProgress && !pending(state)))
      .subscribe((state) => {
        this.saveInProgress = false;
        this.saveSuccessful = (state === EntityStatus.loadingSuccess);
        if (this.saveSuccessful) {
          this.updateForm.markAsPristine();
        }
      });
  }

  public saveDataFeed(): void {
    this.saveSuccessful = false;
    this.saveInProgress = true;
    const destinationObj = {
      id: this.destination.id,
      name: this.updateForm.controls['name'].value.trim(),
      url: this.updateForm.controls['url'].value.trim(),
      secret: this.destination.secret
    };

    this.store.dispatch(new UpdateDestination({ destination: destinationObj }));
    this.destination = destinationObj;
  }

  public sendTestForDataFeedUrl(): void {
    this.testInProgress = true;
    this.hookStatus = UrlTestState.Loading;
    const targetUrl: string =  this.updateForm.controls['url'].value;
    if (targetUrl) {
      this.datafeedService.testDestinationWithNoCreds(targetUrl)
        .subscribe(
          () => this.revealUrlStatus(UrlTestState.Success),
          () => this.revealUrlStatus(UrlTestState.Failure)
        );
    }
    this.testInProgress = false;
  }

  private revealUrlStatus(status: UrlTestState) {
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

  public get nameCtrl(): FormControl {
    return <FormControl>this.updateForm.controls.name;
  }

  public get urlCtrl(): FormControl {
    return <FormControl>this.updateForm.controls.url;
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
