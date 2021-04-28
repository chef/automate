import { Component, OnInit, OnDestroy, EventEmitter, ViewChild } from '@angular/core';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { routeParams, routeURL } from 'app/route.selectors';
import { filter, pluck, takeUntil } from 'rxjs/operators';
import { identity } from 'lodash/fp';
import { Cookbook } from 'app/entities/cookbooks/cookbook.model';
import { environmentFromRoute } from 'app/entities/environments/environment-details.selectors';
import { GetEnvironment } from 'app/entities/environments/environment.action';
import {
  Environment,
  EnvironmentAttributes,
  CookbookVersionDisplay
} from 'app/entities/environments/environment.model';
import { GetCookbooks } from 'app/entities/cookbooks/cookbook.actions';
import {
  allCookbooks,
  getAllStatus as getAllCookbooksForOrgStatus
} from 'app/entities/cookbooks/cookbook.selectors';
import { EntityStatus } from 'app/entities/entities';
import { isNil } from 'ngx-cookie';
import { JsonTreeTableComponent as JsonTreeTable } from './../json-tree-table/json-tree-table.component';
import { FormGroup, FormBuilder, Validators } from '@angular/forms';


import { CookbookConstraintGrid } from '../edit-environment-attribute-modal/edit-environment-attribute-modal.component';

export type EnvironmentTabName = 'cookbookConstraints' | 'attributes';

@Component({
  selector: 'app-environment-details',
  templateUrl: './environment-details.component.html',
  styleUrls: ['./environment-details.component.scss']
})

export class EnvironmentDetailsComponent implements OnInit, OnDestroy {
  public environment: Environment;
  public cookbookVersions: CookbookVersionDisplay[];
  public cookbooks: Cookbook[] = [];
  public constraintKeys: string[] = [];
  public nameKeys: string[] = [];
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
  public environmentDetailsLoading = true;
  public editAttrModalVisible = false;
  public editAttributeForm: FormGroup;
  public label: string;
  public attributes = new EnvironmentAttributes({
    default_attributes: '',
    override_attributes: ''
  });
  public cookbookConstraints: Array<CookbookConstraintGrid> = [];
  public name_id = '';
  public openEdit = false;
  public selectedAttrs: any;
  public selected_level = 'all';
  public jsonText: any;
  public hasOverrideJson = true;
  public hasDefaultJson = true;
  public openEnvironmentModal = new EventEmitter<boolean>();

  // precedence levels
  public default_attributes = 'default_attributes';
  public override_attributes = 'override_attributes';
  public all = 'all';

  @ViewChild(JsonTreeTable, { static: true })
  tree: JsonTreeTable;

  constructor(
    private store: Store<NgrxStateAtom>,
    private router: Router,
    private layoutFacade: LayoutFacadeService,
    private fb: FormBuilder
  ) {
    this.editAttributeForm = this.fb.group({
      defaultAttributes: ['', [Validators.required]]
    });
   }

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

    this.loadCookbooks();

    this.store.select(environmentFromRoute).pipe(
      filter(identity),
      takeUntil(this.isDestroyed)
    ).subscribe(environment => {
      this.cookbookConstraints = [];
      this.show = true;
      this.environment = environment;
      this.cookbookVersions = this.toDisplay(environment.cookbook_versions);
      this.cookbookVersions.forEach((obj, index) => {
        this.cookbookConstraints.push({
          id: index + 1,
          name: obj.name,
          operator: obj.operator,
          version: obj.versionNumber
        });
      });
      this.hasCookbookConstraints = Object.keys(
        environment.cookbook_versions).length > 0 ? true : false;
      this.attributes = new EnvironmentAttributes(this.environment);

      this.hasDefaultJson = Object.keys(
        JSON.parse(this.environment.default_attributes)).length > 0 ? true : false;
      this.hasOverrideJson = Object.keys(
        JSON.parse(this.environment.override_attributes)).length > 0 ? true : false;

      setTimeout(() => this.filter(this.selected_level), 10);
      this.environmentDetailsLoading = false;
    });

  }


  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
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

  toDisplay(cookbookVersions: Object): CookbookVersionDisplay[] {
    return Object.keys(cookbookVersions).map(function (key) {
      const value = cookbookVersions[key].split(' ');
      return {name: key, operator: value[0], versionNumber: value[1]};
    });
  }

  onSelectedTab(event: { target: { value: EnvironmentTabName } }) {
    this.tabValue = event.target.value;
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
    this.filter(this.selected_level);
    this.label = '';
  }

  public openEditAttributeModal(value, label: string): void {
    this.openEdit = true;
    const obj = JSON.parse(value);
    this.jsonText = JSON.stringify(obj, null, 4);
    this.label = label;
    this.openEnvironmentModal.emit(true);
  }

  public closeEditAttributeModal(): void {
    this.editAttrModalVisible = false;
  }

  private loadCookbooks() {
    this.name_id = '';
    this.store.dispatch(new GetCookbooks({
      server_id: this.serverId, org_id: this.orgId
    }));

    combineLatest([
      this.store.select(getAllCookbooksForOrgStatus),
      this.store.select(allCookbooks)
    ]).pipe(takeUntil(this.isDestroyed))
    .subscribe(([ getCookbooksSt, allCookbooksState]) => {
      if (getCookbooksSt === EntityStatus.loadingSuccess && !isNil(allCookbooksState)) {
        this.constraintKeys = [];
        this.nameKeys = [];

        this.cookbooks = allCookbooksState;
        this.cookbooks.forEach((cookbook) => {
          this.constraintKeys.push(cookbook.name);
          this.nameKeys.push(cookbook.name);
        });
      }
    });

  }
}
