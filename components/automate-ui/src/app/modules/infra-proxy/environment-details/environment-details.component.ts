import { Component, OnInit, OnDestroy, EventEmitter, ViewChild } from '@angular/core';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { routeParams, routeURL } from 'app/route.selectors';
import { filter, pluck, takeUntil } from 'rxjs/operators';
import { identity } from 'lodash/fp';
import { environmentFromRoute } from 'app/entities/environments/environment-details.selectors';
import { GetEnvironment } from 'app/entities/environments/environment.action';
import {
  Environment,
  CookbookVersion,
  EnvironmentAttributes,
  CookbookVersionDisplay
} from 'app/entities/environments/environment.model';
import { JsonTreeTableComponent as JsonTreeTable } from './../json-tree-table/json-tree-table.component';

export type EnvironmentTabName = 'cookbookConstraints' | 'attributes';

@Component({
  selector: 'app-environment-details',
  templateUrl: './environment-details.component.html',
  styleUrls: ['./environment-details.component.scss']
})

export class EnvironmentDetailsComponent implements OnInit, OnDestroy {
  public environment: Environment;
  public cookbookVersions: CookbookVersionDisplay[];
  public tabValue: EnvironmentTabName = 'cookbookConstraints';
  public url: string;
  public conflictErrorEvent = new EventEmitter<boolean>();
  public modalType: string;
  public serverId: string;
  public orgId: string;
  public name: string;
  public show = false;
  public hasCookbookConstraints = false;
  private isDestroyed = new Subject<boolean>();
  environmentDetailsLoading = true;

  public attributes = new EnvironmentAttributes({
    default_attributes: '',
    override_attributes: ''
  });

  public selectedAttrs: any;
  public selected_level = 'all';

  // precedence levels
  public default_attributes = 'default_attributes';
  public override_attributes = 'override_attributes';
  public all = 'all';

  @ViewChild(JsonTreeTable, { static: true })
  tree: JsonTreeTable;

  constructor(
    private store: Store<NgrxStateAtom>,
    private router: Router,
    private layoutFacade: LayoutFacadeService
  ) { }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);
    this.store.select(routeURL).pipe(takeUntil(this.isDestroyed))
      .subscribe((url: string) => {
        this.url = url;
        const [, fragment] = url.split('#');
        this.tabValue = (fragment === 'attributes') ? 'attributes' : 'cookbookConstraints';
      });

    combineLatest([
      this.store.select(routeParams).pipe(pluck('id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('org-id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('name'), filter(identity))
    ]).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe(([server_id, org_id, name]: string[]) => {
      this.serverId = server_id;
      this.orgId = org_id;
      this.name = name;
      this.store.dispatch(new GetEnvironment({
        server_id: server_id, org_id: org_id, name: name
      }));
    });

    this.store.select(environmentFromRoute).pipe(
      filter(identity),
      takeUntil(this.isDestroyed)
    ).subscribe(environment => {
      this.show = true;
      this.environment = environment;
      this.cookbookVersions = this.toDisplay(environment.cookbook_versions);
      if (Object.keys(environment.cookbook_versions).length > 0) {
        this.hasCookbookConstraints = true;
      }
      //this.attributes = new EnvironmentAttributes(this.environment);

      setTimeout(() => this.filter(this.selected_level), 10);
      this.environmentDetailsLoading = false;
    });
  }

  // set selected item to selected_level
  filter(precedence_level: string): void {
    this.selected_level = precedence_level;
    this.selectedAttrs = this.retrieve(precedence_level);
  }

  // retrieve attributes based on their level of precedence
  retrieve(level: string): Object {
    switch (level) {
      case this.all: {
        return this.attributes.all;
      }
      case this.default_attributes: {
        return this.attributes.default_attributes;
      }
      case this.override_attributes: {
        return this.attributes.override_attributes;
      }
      default: {
        return {};
      }
    }
  }

  toDisplay(cookbookVersions: CookbookVersion[]): CookbookVersionDisplay[] {
    return Object.keys(cookbookVersions).map(function (key) {
      const value = cookbookVersions[key].split(' ');
      return {name: key, operator: value[0], versionNumber: value[1]};
    });
  }

  onSelectedTab(event: { target: { value: EnvironmentTabName } }) {
    this.tabValue = event.target.value;
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
    this.filter(this.selected_level);
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
