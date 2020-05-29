import { Component, OnInit, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { MatOptionSelectionChange } from '@angular/material/core/option';
import { Store } from '@ngrx/store';
import { filter, map, takeUntil, distinctUntilChanged } from 'rxjs/operators';
import { isNil } from 'lodash/fp';
import { Subject, combineLatest } from 'rxjs';

import { EntityStatus } from 'app/entities/entities';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeURL, routeState } from 'app/route.selectors';
import { GetPolicy } from 'app/entities/policies/policy.actions';
import { policyFromRoute, getStatus } from 'app/entities/policies/policy.selectors';
import {
  Policy, Member, Type, stringToMember
} from 'app/entities/policies/policy.model';
import { RemovePolicyMembers } from 'app/entities/policies/policy.actions';

export type PolicyTabName = 'definition' | 'members';

const POLICY_DETAILS_ROUTE = /^\/settings\/policies/;

@Component({
  selector: 'app-policy-details',
  templateUrl: './policy-details.component.html',
  styleUrls: ['./policy-details.component.scss']
})
export class PolicyDetailsComponent implements OnInit, OnDestroy {
  public policy: Omit<Policy, 'members'>; // policy without 'members' key
  public policyJSON: string;
  public members: Member[] = [];
  public tabValue: PolicyTabName = 'definition';
  private url: string;
  // Map of local user and team member IDs to URLs.
  // Will not contain LDAP, SAML, or * members.
  public memberURLs: { [id: string]: string[] } = {};

  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private router: Router,
    private layoutFacade: LayoutFacadeService
  ) {  }

  ngOnInit(): void {
    this.layoutFacade.showSidebar(Sidebar.Settings);
    // Populate our tabValue from the fragment.
    this.store.select(routeURL).pipe(takeUntil(this.isDestroyed))
      .subscribe((url: string) => {
        this.url = url;
        const [, fragment] = url.split('#');
        // goes to #definition if (1) explicit #definition, (2) no fragment, or (3) invalid fragment
        this.tabValue = (fragment === 'members') ? 'members' : 'definition';
     });

    combineLatest([
      this.store.select(getStatus),
      this.store.select(policyFromRoute)
    ]).pipe(
      filter(([status, _]) => status === EntityStatus.loadingSuccess),
      filter(([_, state]) => !isNil(state)),
      takeUntil(this.isDestroyed)
    ).subscribe(([_, { id, type, name, members, projects, statements: allStatements }]) => {
        // massage statements: remove '*' resources
        const statements = allStatements.map(
          ({resources: allResources, effect, actions, projects: stmtProjects, role }) => {
          const resources = allResources.filter(res => res !== '*');
          // include remaining resources if there are some
          if (resources.length > 0) {
            return { effect, role, actions, resources, projects: stmtProjects };
          } else {
            return { effect, role, actions, projects: stmtProjects }; // omit resources
          }
        });
        // we do this to sort the keys in the json output
        this.policy =  { name, id, type, projects, statements }; // omit members
        this.policyJSON = this.policyToString({ name, id, type, projects, members, statements });
        this.members = members.map(e => stringToMember(e));
        this.members.forEach(member => {
          if (member.type === Type.LocalUser) {
            this.memberURLs[member.name] = ['/settings', 'users', member.displayName];
          } else if (member.type === Type.LocalTeam) {
            this.memberURLs[member.name] = ['/settings', 'teams', member.displayName];
          } else if (member.type === Type.Token) {
            this.memberURLs[member.name] = ['/settings', 'tokens', member.displayName];
          }
        });
      });

    this.store.select(routeState).pipe(
      takeUntil(this.isDestroyed),
      map(state => [state.params.id as string, state.url]),
      // Only fetch if we are on the policy details route, otherwise
      // we'll trigger GetPolicy with the wrong input on any route
      // away to a page that also uses the :id param.
      filter(([id, url]) => POLICY_DETAILS_ROUTE.test(url) && id !== undefined),
      // Remove the url because we only need to check if the id has changed
      map(([id, _url]) => id),
      distinctUntilChanged()
    ).subscribe(id => this.store.dispatch(new GetPolicy({ id })));
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  private policyToString(policy: Policy): string {
    return JSON.stringify(policy, null, '  ');
  }

  removeMember($event: MatOptionSelectionChange, member: Member): void {
    if ($event.isUserInput) {
      this.store.dispatch(new RemovePolicyMembers({
        id: this.policy.id,
        members: [member]
      }));
    }
  }

  onSelectedTab(event: { target: { value: PolicyTabName } } ): void {
    this.tabValue = event.target.value;
    // Drop the previous fragment and add the incoming fragment.
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
  }
}
