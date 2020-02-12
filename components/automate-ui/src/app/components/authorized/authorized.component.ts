import { Component, Input, EventEmitter, OnInit, OnDestroy, ChangeDetectorRef, Output } from '@angular/core';
import { Store } from '@ngrx/store';
import { isEmpty, isArray, pipe } from 'lodash/fp';
import { takeUntil, filter, debounceTime } from 'rxjs/operators';
import { Subject, Subscription } from 'rxjs';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { AuthorizedChecker, CheckObj } from 'app/helpers/auth/authorized';
import { allPerms } from 'app/entities/userperms/userperms.selectors';

// Data arrives in this form for user convenience,
// i.e. just [allOf]="['/auth/users', 'get']
// rather than having to build a more verbose object like:
// [allOf]="{ endpoint: '/auth/users', verb: 'get'}
// Each input datum is converted to a CheckObj upon arrival.
// Internally, this allows for more robust handling of the data.
// (So properties must be in sync with `CheckObj`!)
export type Check = [string, string, string | string[]];

@Component({
  selector: 'app-authorized',
  template: '<ng-content *ngIf="visible || overrideVisible"></ng-content>'
})
export class AuthorizedComponent implements OnInit, OnDestroy {

  private authorizedChecker: AuthorizedChecker;
  private subscription: Subscription;
  private _allOf: CheckObj[] = [];
  private _anyOf: CheckObj[] = [];

  @Input()
  set allOf(val: Check[] | Check) {
    this._allOf = val ? this.normalizeInput(val) : undefined;
  }

  @Input()
  set anyOf(val: Check[] | Check) {
    this._anyOf = val ? this.normalizeInput(val) : undefined;
  }

  // Include the bare `not` attribute in your HTML element to negate the check.
  @Input() not?: boolean;

  // If you wish to override the permissions check and set content to visible anyway.
  // Defaults to false. Useful when trying to generalize components that contain
  // this component.
  @Input() overrideVisible = false;

  // if you wish to output visible
  @Output() isAuthorized: EventEmitter<boolean> = new EventEmitter();

  public visible = false;
  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  constructor(private cdr: ChangeDetectorRef, private store: Store<NgrxStateAtom>) {
    this.authorizedChecker = new AuthorizedChecker(this.store);
  }

  // This component leverages as much of AuthorizedChecker as it can
  // without incurring performance degradation.
  // That is, the content of ngOnInit() could have all been done by
  // AuthorizedChecker but that causes an unacceptable UI rendering artifact:
  // You first see the whole list of items in the left nav, then after a
  // fraction of a second it shrinks to just the allowable ones.
  ngOnInit() {
    this.subscription = this.store.select(allPerms).pipe(
      takeUntil(this.isDestroyed),
      debounceTime(AuthorizedChecker.DebounceTime),
      filter(perms => !isEmpty(perms)))
      .subscribe(perms => {
        const oldVisible = this.visible;
        const newVisible = this.authorizedChecker.evalPerms(perms);
        this.visible = this.not === undefined ? newVisible : !newVisible;
        this.isAuthorized.emit(this.visible);

        // Allow this component to receive updates on pages with "OnPush" strategy.
        if (oldVisible !== this.visible) {
          this.cdr.detectChanges();
        }
      });
    this.authorizedChecker.setPermissions(this._allOf, this._anyOf);
  }

  ngOnDestroy() {
    this.cdr.detach();
    this.subscription.unsubscribe();
    this.authorizedChecker.destroy();
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public normalizeInput(input: Check[] | Check): CheckObj[] {
    return pipe(
      this.ensureArrayInput.bind(this),
      this.convertToObject.bind(this),
      this.downcaseVerb.bind(this)
    ).call(this, input);
  }

  private ensureArrayInput(input: Check[] | Check): Check[] {
    return (isEmpty(input) || isArray(input[0]) ? input : [input]) as Check[];
  }

  private convertToObject(input: Check[]): CheckObj[] {
    return input.map(
      ([endpoint, verb, paramList]: Check) =>
        <CheckObj>{ endpoint, verb, paramList });
  }

  private downcaseVerb(input: CheckObj[]): CheckObj[] {
    return input.map(
      ({ endpoint, verb, paramList }: CheckObj) =>
        <CheckObj>{ endpoint, verb: verb.toLowerCase(), paramList }
    );
  }

}
