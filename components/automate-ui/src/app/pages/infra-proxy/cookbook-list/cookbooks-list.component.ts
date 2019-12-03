import { Component, OnInit } from '@angular/core';
import { Store, select } from '@ngrx/store';
import { Observable, Subject, combineLatest } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { loading } from 'app/entities/entities';
import { Cookbook } from 'app/entities/cookbooks/cookbook.model';
import { routeParams } from 'app/route.selectors';
import { filter, pluck, takeUntil, map } from 'rxjs/operators';
import { identity } from 'lodash/fp';
import { GetCookbooksForOrg } from 'app/entities/cookbooks/cookbook.actions';
import { ChefSorters } from 'app/helpers/auth/sorter';
import {
  allCookbooks,
  getStatus
} from 'app/entities/cookbooks/cookbook.selectors';

@Component({
  selector: 'app-cookbooks-list',
  templateUrl: './cookbooks-list.component.html',
  styleUrls: ['./cookbooks-list.component.scss']
})
export class CookbooksListComponent implements OnInit {
  public loading$: Observable<boolean>;
  public sortedCookbooks$: Observable<Cookbook[]>;
  private isDestroyed = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) {
    this.loading$ = store.pipe(select(getStatus), map(loading));

    this.sortedCookbooks$ = store.pipe(
      select(allCookbooks),
      map(cookbooks => ChefSorters.naturalSort(cookbooks, 'name'))
    );
    console.log(this.sortedCookbooks$);
  }

  ngOnInit() {
    this.layoutFacade.showInfastructureSidebar();
    combineLatest([
      this.store.select(routeParams).pipe(pluck('id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('orgid'), filter(identity))
    ]).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe(([server_id, org_id]: string[]) => {
      this.store.dispatch(new GetCookbooksForOrg({
        server_id, org_id
      }));
    });
  }

}
