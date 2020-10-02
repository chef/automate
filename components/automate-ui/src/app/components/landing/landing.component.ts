import { Component, OnInit, Input, ChangeDetectorRef, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { takeUntil } from 'rxjs/operators';
import { Subject } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Check, AuthorizedComponent } from 'app/components/authorized/authorized.component';
import { AuthorizedChecker, CheckObj } from 'app/helpers/auth/authorized';

  /**
   * Each RoutePerm specifies the same `anyOf` and `allOf` values used
   * in the `<app-authorized>` elements in your sidebar component.
   * The `route` property specifies where to re-route should those
   * permissions allow access to the user.
   */
export interface RoutePerms {
  anyOfCheck?: Check[];
  allOfCheck?: Check[];
  route: string;
}

/**
 * To use this component you need to "subclass" it.
 * That is, create a new page-specific component, e.g. MyLandingComponent.
 * The template for that component should be just this:
 *     <app-landing [routePerms]="routeList"></app-landing>
 * ... where MyLandingComponent.routeList specifies the list of your sub-pages,
 * in order, and matching your sidebar component.
 * Each of the items in routeList will be permission-checked;
 * the user will be sent to the first allowable one found.
 */

@Component({
  selector: 'app-landing',
  template: '', // no content
  styleUrls: []
})
export class LandingComponent implements OnInit, OnDestroy {

  public authorizedChecker: AuthorizedChecker;
  private authComponent: AuthorizedComponent;
  private isDestroyed = new Subject<boolean>();
  private index = 0;

  /**
   * The single input to this class is this list that corresponds
   * to the items in your sidebar component.
   * Each such sidebar component should be guarded with an `<app-authorized>`
   * so the sidebar entry will only show if the user has access to it.
   * The same permissions are used here to actually re-route to it when permissible.
   */
  @Input() routePerms: RoutePerms[] = [];

  /**
   * A callback function to perform some actions in the event that no landing page was found.
   * If no callback is provided, the caller guarantees one will always be found.
   * (That should be the case for sidebars because we will never attempt to
   * materialize a sidebar unless at least one item is permissible.)
   * Thus, this defaults to a no-op function.
   */
  @Input() onNotFound: () => void = () => { };

  constructor(
    cdr: ChangeDetectorRef,
    private store: Store<NgrxStateAtom>,
    private router: Router
  ) {
    this.authorizedChecker = new AuthorizedChecker(this.store);
    this.authComponent = new AuthorizedComponent(cdr, this.store);
  }

  ngOnInit() {
    this.authorizedChecker.isAuthorized$
      .pipe(takeUntil(this.isDestroyed))
      .subscribe(isAuthorized => this.handlePerm(isAuthorized));
    this.setPerms();
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  private handlePerm(isAuthorized: boolean): void {
    if (isAuthorized) {
      this.router.navigate(this.routeToSegmentList(this.route), { replaceUrl: true });
    } else if (++this.index < this.routePerms.length) {
      this.setPerms();
    } else {
        this.onNotFound();
    }
  }

  private setPerms(): void {
    this.authorizedChecker.setPermissions(
      this.getAllOfChecks(this.index), this.getAnyOfChecks(this.index));
  }

  private get route(): string {
    return this.routePerms[this.index].route;
  }

  public getAnyOfChecks(index: number): CheckObj[] {
    return this.normalizeChecks(
      this.routePerms[index].anyOfCheck);
  }

  public getAllOfChecks(index: number): CheckObj[] {
    return this.normalizeChecks(
      this.routePerms[index].allOfCheck);
  }

  private normalizeChecks(check: Check[]): CheckObj[] {
    return check ? this.authComponent.normalizeInput(check) : [];
  }

  public routeToSegmentList(route: string): string[] {
    // swallow initial virgule then split on the rest
    return route.substr(1).split('/');
  }

}
