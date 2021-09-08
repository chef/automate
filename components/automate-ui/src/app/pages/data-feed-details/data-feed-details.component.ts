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
import { GetDestination, UpdateDestination, TestDestination } from 'app/entities/destinations/destination.actions';
import { destinationFromRoute, getStatus, updateStatus } from 'app/entities/destinations/destination.selectors';
import { Destination } from 'app/entities/destinations/destination.model';

type DestinationTabName = 'details';

enum UrlTestState {
  Inactive,
  Loading,
  Success,
  Failure
}

@Component({
  selector: 'app-data-feed-details',
  templateUrl: './data-feed-details.component.html',
  styleUrls: ['./data-feed-details.component.scss']
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

  constructor(
    private fb: FormBuilder,
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
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
      });

    this.updateForm = this.fb.group({
      // Must stay in sync with error checks in data-feed-details.component.html
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      // Note that URL here may be FQDN -or- IP!
      url: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]]
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
    const destinationObj = {
      ...this.destination,
      name: this.updateForm.controls['name'].value.trim(),
      url: this.updateForm.controls['url'].value.trim()
    };
    this.store.dispatch(new TestDestination({destination: destinationObj}));
    this.testInProgress = false;
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
