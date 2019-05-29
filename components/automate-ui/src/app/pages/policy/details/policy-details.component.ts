import { Component, OnInit, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { identity } from 'lodash/fp';
import { Observable, Subject } from 'rxjs';
import { filter, map, pluck, takeUntil } from 'rxjs/operators';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeParams } from 'app/route.selectors';
import { routeURL } from 'app/route.selectors';
import { GetPolicy } from 'app/entities/policies/policy.actions';
import { policyFromRoute } from 'app/entities/policies/policy.selectors';
import {
  Policy, Member, Type, stringToMember
} from 'app/entities/policies/policy.model';
import {
  RemovePolicyMembers, PolicyMembersMgmtPayload
} from 'app/entities/policies/policy.actions';

const POLICY_DETAILS_ROUTE = /^\/settings\/policies/;

@Component({
  selector: 'app-policy-details',
  templateUrl: './policy-details.component.html',
  styleUrls: ['./policy-details.component.scss']
})
export class PolicyDetailsComponent implements OnInit, OnDestroy {
  public policy: Policy;
  public policyJSON: string;
  public members$: Observable<Member[]>;
  public tabValue = 'definition';
  public url: string;
  // Map of local user and team member IDs to URLs.
  // Will not contain LDAP, SAML, or * members.
  private memberURLs: { [id: string]: string[] } = {};

  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  constructor(private store: Store<NgrxStateAtom>,
              private router: Router) {  }

  ngOnInit(): void {
    // Populate our tabValue from the fragment.
    this.store.select(routeURL).pipe(takeUntil(this.isDestroyed))
      .subscribe((url: string) => {
        this.url = url;
        const splitFragment = url.split('#');
        if (splitFragment.length === 2) {
          switch (splitFragment[1]) {
            case 'members': {
              this.tabValue = 'members';
              break;
            }
            default: { // If the user passed an invalid fragment or #definition
              this.tabValue = 'definition';
            }
          }
        } else {
          // Default to definition in the case of no fragment.
          this.tabValue = 'definition';
        }
      });

    this.members$ = this.store.select(policyFromRoute).pipe(
      filter(identity),
      takeUntil(this.isDestroyed),
      map((state) => {
        this.policy = <Policy>Object.assign({}, state);
        this.policyJSON = this.policyToString(this.policy);
        const members = <Member[]>[];
        this.policy.members.forEach(element => {
          const member = stringToMember(element);
          members.push(member);
          if (member.type === Type.LocalUser) {
            this.memberURLs[member.name] = ['/settings', 'users', member.displayName];
          } else if (member.type === Type.LocalTeam) {
            this.memberURLs[member.name] = ['/settings', 'teams', member.displayName];
          } else if (member.type === Type.Token) {
            this.memberURLs[member.name] = ['/settings', 'tokens', member.displayName];
          }
        });
        delete this.policy.members;
        return members;
      }));
    this.members$.subscribe();

    this.store.select(routeParams).pipe(
      pluck('id'),
      filter(identity),
      takeUntil(this.isDestroyed))
      .subscribe((id: string) => {
        this.store.select(routeURL).pipe(
          filter(identity),
          takeUntil(this.isDestroyed))
          .subscribe((url: string) => {
            // Only fetch if we are on the policy details route, otherwise
            // we'll trigger GetPolicy with the wrong input on any route
            // away to a page that also uses the :id param.
            if (POLICY_DETAILS_ROUTE.test(url)) {
              this.store.dispatch(new GetPolicy({ id }));
            }
        });
      });
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  private policyToString(policy: Policy): string {
    return JSON.stringify(policy);
  }

  removeMember(member: Member): void {
    this.store.dispatch(new RemovePolicyMembers(<PolicyMembersMgmtPayload>{
      id: this.policy.id,
      members: [member]
    }));
  }

  onSelectedTab(event): void {
    this.tabValue = event.target.value;
    // Current URL sans any now outdated fragment.
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
  }
}
