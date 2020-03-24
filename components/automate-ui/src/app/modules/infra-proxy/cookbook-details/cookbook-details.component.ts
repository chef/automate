import { Component, OnInit } from '@angular/core';
import { Store } from '@ngrx/store';
// import { Router } from '@angular/router';
import { Observable, Subject, combineLatest } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { routeURL, routeParams } from 'app/route.selectors';
import { filter, takeUntil, pluck } from 'rxjs/operators';
import { identity, isNil } from 'lodash/fp';
// import { EntityStatus } from 'app/entities/entities';
import { GetCookbookDetails } from 'app/entities/cookbooks/cookbook.actions';
import { Cookbook } from 'app/entities/cookbooks/cookbook.model';
import {
  cookbookFromRoute
} from 'app/entities/cookbooks/cookbook.selectors';
export type CookbookDetailsTab = 'details' | 'content';

@Component({
  selector: 'app-cookbook-details',
  templateUrl: './cookbook-details.component.html',
  styleUrls: ['./cookbook-details.component.scss']
})
export class CookbookDetailsComponent implements OnInit {
  public loading$: Observable<boolean>;
  private isDestroyed = new Subject<boolean>();
  public cookbooks: Cookbook;
  public saveSuccessful = false;
  public saving = false;
  public isLoading = true;
  public url: string;
  public serverId: string;
  public orgId: string;
  public cookbookDetails: Cookbook[];
  public tabValue: CookbookDetailsTab = 'details';
  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) {}

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);
    this.store.select(routeURL).pipe()
    .subscribe((url: string) => {
      this.url = url;
      const [, fragment] = url.split('#');
      this.tabValue = (fragment === 'details') ? 'details' : 'content';
    });

    // this.store.select(allCookbooks).pipe(
    //   filter(identity),
    //   takeUntil(this.isDestroyed))
    //   .subscribe((state) => {
    //     this.cookbookDetails = { ...state };
    //     console.log(this.cookbookDetails);
    //   });

    // this.store.select(routeParams).pipe(
    //   takeUntil(this.isDestroyed),
    //   pluck('id'),
    //   filter(identity),
    //   distinctUntilChanged()
    // ).subscribe((id: string) => this.store.dispatch(new GetPolicy({ id })));

    // combineLatest([
    //   this.store.select(getAllCookbooksForOrgStatus)
    // ]).pipe(
    //   takeUntil(this.isDestroyed)
    // ).subscribe(([getCookbooksSt]) => {
    //     this.isLoading = !allLoaded([getCookbooksSt]);
    //   });

    combineLatest([
      this.store.select(cookbookFromRoute),
      this.store.select(routeParams).pipe(pluck('id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('orgid'), filter(identity))
    ]).pipe(
        filter(([allCookbooksState]) =>
          !isNil(allCookbooksState)),
        takeUntil(this.isDestroyed)
        ).subscribe(([allCookbooksState, server_id, org_id ]) => {
          this.serverId = server_id;
          this.orgId = org_id;
          this.cookbooks = allCookbooksState;
          this.store.dispatch(new GetCookbookDetails({
            server_id: server_id,
            org_id: org_id,
            cookbook_name: this.cookbooks.name,
            cookbook_version: this.cookbooks.current_version
          }));
          // console.log('asdfasdf', this.cookbooks);
          // this.onCookbookVersionChange();
      });
  }

  // public onCookbookVersionChange(): void {
  //   combineLatest([
  //     this.store.select(cookbookFromRoute)
  //   ]).pipe(
  //       filter(([ allCookbooksState]) => !isNil(allCookbooksState)),
  //       takeUntil(this.isDestroyed)
  //       ).subscribe(([ allCookbooksState ]) => {
  //         this.cookbookDetails = <Cookbook>Object.assign({}, allCookbooksState);
  //         console.log(this.cookbookDetails);
  //     });
  // }

  // onSelectedTab(event: { target: { value: CookbookDetailsTab } }) {
  //   this.tabValue = event.target.value;
  //   this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
  // }
}
