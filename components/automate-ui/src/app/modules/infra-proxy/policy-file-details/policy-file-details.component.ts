import { Component, OnInit, OnDestroy, ViewChild } from '@angular/core';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { routeParams, routeURL } from 'app/route.selectors';
import { filter, pluck, takeUntil } from 'rxjs/operators';
import { identity } from 'lodash/fp';
import { PolicyFile, CookbookLocks, IncludedPolicyLocks, SolutionDependencies } from 'app/entities/policy-files/policy-file.model';
import { GetPolicyFile } from 'app/entities/policy-files/policy-file.action';
import { policyFileFromRoute } from 'app/entities/policy-files/policy-file-details.selectors';
import { JsonTreeTableComponent as JsonTreeTable } from './../json-tree-table/json-tree-table.component';

export type PolicyFileTabName = 'details' | 'runList' | 'attributes';
export class CookbookList {
  name: string;
  version: string;
  source: string;
}

export class CookbookRuleList {
  name: string;
  operator: string;
  version: string;
}

export class CookbookDependencyList {
  name: string;
  operator: string;
  version: string;
  dependName: string;
  dependOperator: string;
  dependVersion: string;
}

@Component({
  selector: 'app-policy-file-details',
  templateUrl: './policy-file-details.component.html',
  styleUrls: ['./policy-file-details.component.scss']
})

export class PolicyFileDetailsComponent implements OnInit, OnDestroy {
  public conflictError = false;
  public PolicyFile: PolicyFile;
  public tabValue: PolicyFileTabName = 'details';
  public serverId: string;
  public orgId: string;
  public name: string;
  public revision: string;
  public policyFileDetailsLoading = true;
  public url: string;

  private isDestroyed = new Subject<boolean>();
  public activeIncludedPolicies: string;
  public activeRunlist: string;
  public showRunList = false;
  public showIncludedPolicies = false;
  public cookbook_locks: CookbookLocks[] = [];
  public included_policy_locks: IncludedPolicyLocks[] = [];
  public cookbookList: CookbookList[] = [];
  public cookbookRuleList: CookbookRuleList[] = [];
  public cookbookDependencyList: CookbookDependencyList[] = [];
  public defaultAttributes = [];
  public overrideAttributes = [];
  public hasDefaultattributes = false;
  public hasOverrideattributes = false;

  @ViewChild(JsonTreeTable, { static: true })
  tree: JsonTreeTable;

  constructor(
    private router: Router,
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) { }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);
    this.store.select(routeURL).pipe(takeUntil(this.isDestroyed))
    .subscribe((url: string) => {
      this.url = url;
      const [, fragment] = url.split('#');
      switch (fragment) {
        case 'details':
          this.tabValue = 'details';
          break;
        case 'runList':
          this.tabValue = 'runList';
          break;
        case 'attributes':
          this.tabValue = 'attributes';
          break;
      }
    });
    // load policy file details
    combineLatest([
      this.store.select(routeParams).pipe(pluck('id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('org-id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('name'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('revision'), filter(identity))
    ]).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe(([server_id, org_id, name, revision_id]: string[]) => {
      this.serverId = server_id;
      this.orgId = org_id;
      this.name = name;
      if (revision_id !== this.revision) {
        this.revision = revision_id;
        this.store.dispatch(new GetPolicyFile({
          server_id: server_id, org_id: org_id, name: name, revision: revision_id
        }));
      }
    });
    this.store.select(policyFileFromRoute).pipe(
      filter(identity),
      takeUntil(this.isDestroyed)
    ).subscribe(policyFile => {
      this.PolicyFile = policyFile;
      this.cookbook_locks = policyFile.cookbook_locks;
      this.included_policy_locks = policyFile.included_policy_locks;
      this.policyFileDetailsLoading = false;
      if (policyFile.solution_dependecies) {
        this.loadDependenciesRules(policyFile.solution_dependecies);
        this.loadCookbooks(policyFile.solution_dependecies);
      }
      // load attributes
      this.defaultAttributes = (policyFile.default_attributes
        && JSON.parse(policyFile.default_attributes)) || {};
      this.hasDefaultattributes = Object.keys(
        JSON.parse(policyFile.default_attributes)).length > 0 ? true : false;
      this.overrideAttributes = (policyFile.override_attributes
       && JSON.parse(policyFile.override_attributes)) || {};
      this.hasOverrideattributes = Object.keys(
        JSON.parse(policyFile.override_attributes)).length > 0 ? true : false;
    });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  onSelectedTab(event: { target: { value: PolicyFileTabName } }) {
    this.tabValue = event.target.value;
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
  }

  handlePolicyFileSelected() {
    if (!this.showIncludedPolicies) {
      this.showIncludedPolicies = true;
      this.activeIncludedPolicies = 'autoHeight';
    } else {
      this.showIncludedPolicies = false;
      this.activeIncludedPolicies = '';
    }
  }

  handleRunListSelected() {
    if (!this.showRunList) {
      this.showRunList = true;
      this.activeRunlist = 'autoHeight';

    } else {
      this.showRunList = false;
      this.activeRunlist = '';
    }
  }

  loadDependenciesRules(solution_dependecies: SolutionDependencies[]) {
    const cookbook_dependent = [];
    for (let i = 0; i < solution_dependecies.length; i++) {
      const version = solution_dependecies[i].version.split(' ');
      this.cookbookRuleList.push({
        name: solution_dependecies[i].name,
        operator: version[0],
        version: version[1]
      });

      if (solution_dependecies[i].dependencies.length > 0) {
        cookbook_dependent.push(solution_dependecies[i]);
      }
    }
    this.loadCookbookDependent(cookbook_dependent);
  }

  loadCookbookDependent(cookbook_dependent: SolutionDependencies[]) {
    for (let i = 0; i < cookbook_dependent.length; i++) {
      const name = cookbook_dependent[i].name;
      const cookbook_version = cookbook_dependent[i].version.split(' ');
      const operator = cookbook_version[0];
      const version = cookbook_version[1];
      const cookbook_dependencies = cookbook_dependent[i].dependencies;
      for (let j = 0; j < cookbook_dependencies.length; j++) {
        const depend_version = cookbook_dependencies[j].version.split(' ');
        this.cookbookDependencyList.push({
          name: name,
          operator: operator,
          version: version,
          dependName: cookbook_dependencies[j].name,
          dependOperator: depend_version[0],
          dependVersion: depend_version[1]
        });
      }
    }
  }

  loadCookbooks(solution_dependecies: SolutionDependencies[]) {
    solution_dependecies.forEach((avail) => {
      this.cookbook_locks.forEach(elm => {
        if (avail.name === elm.name) {
          const version = avail.version.split(' ');
          this.cookbookList.push({
            name: avail.name,
            version: version[1],
            source: elm.source
          });
        }
      });
    });
  }
}
