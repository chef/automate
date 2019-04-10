import { Component, OnInit, OnDestroy } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { HttpErrorResponse } from '@angular/common/http';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { identity } from 'lodash/fp';
import { filter, pluck, takeUntil } from 'rxjs/operators';
import { combineLatest, Subject } from 'rxjs';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeParams } from 'app/route.selectors';
import { EntityStatus } from 'app/entities/entities';

import {
  GetPolicy, AddPolicyMembers, PolicyMembersMgmtPayload
} from 'app/entities/policies/policy.actions';
import {
  policyFromRoute,
  getStatus as getPolicyStatus,
  addPolicyMembersStatus,
  addPolicyMembersHTTPError
} from 'app/entities/policies/policy.selectors';
import { Policy, Member, Type, stringToMember } from 'app/entities/policies/policy.model';
import { allTeams, getAllStatus as getAllTeamsStatus } from 'app/entities/teams/team.selectors';
import { Team } from 'app/entities/teams/team.model';
import { GetTeams } from 'app/entities/teams/team.actions';
import { allUsers, userStatus } from 'app/entities/users/user.selectors';
import {
  GetUsers
} from 'app/entities/users/user.actions';
import { User } from 'app/entities/users/user.model';

@Component({
  selector: 'app-policy-add-members',
  templateUrl: './policy-add-members.component.html',
  styleUrls: ['./policy-add-members.component.scss']
})
export class PolicyAddMembersComponent implements OnInit, OnDestroy {
  // Data structures and state

  public policy: Policy;
  // Members that are in the list of possible members to add.
  private membersAvailableMap: { [id: string]: Member } = {};
  // Sorted version of the above in array form for chef-table.
  // Use addAvailableMember and removeAvailableMember to keep in sync.
  public sortedMembersAvailable: Member[];
  // Members that user has selected to add on page submit.
  private membersToAdd: { [id: string]: Member } = {};
  // Map of local user and team member IDs to URLs.
  // Will not contain LDAP and SAML stuff.
  private memberURLs: { [id: string]: string[] } = {};
  // If all required data has loaded or not.
  public loading = true;
  // If the request to add members is in progress.
  public addingMembers = false;
  private isDestroyed: Subject<boolean> = new Subject<boolean>();
  public addMembersFailed = '';

  // Add expression modal and modal error cases

  public modalVisible = false;
  public unparsableMember = false;
  public duplicateMember = false;
  public alreadyPolicyMember = false;

  // Form info
  public expressionForm: FormGroup;

  constructor(
    private store: Store<NgrxStateAtom>,
    private router: Router,
    fb: FormBuilder) {

    this.expressionForm = fb.group({
      expression: ['', Validators.required]
    });
  }

  ngOnInit(): void {
    this.store.select(routeParams).pipe(
      pluck('id'),
      filter(identity),
      takeUntil(this.isDestroyed))
      .subscribe((id: string) => {
        this.store.dispatch(new GetPolicy({ id }));
        combineLatest(
          this.store.select(getAllTeamsStatus),
          this.store.select(userStatus),
          this.store.select(getPolicyStatus),
          this.store.select(allTeams),
          this.store.select(allUsers)
        ).pipe(takeUntil(this.isDestroyed))
          .subscribe(([tStatus, uStatus, pStatus, teams, users]) => {
            if (tStatus === EntityStatus.loadingSuccess &&
                uStatus === EntityStatus.loadingSuccess &&
                pStatus === EntityStatus.loadingSuccess) {

              this.store.select(policyFromRoute).pipe(
                filter(identity),
                takeUntil(this.isDestroyed)).subscribe((policy: Policy) => {
                  this.policy = <Policy>Object.assign({}, policy);

                  teams.forEach((team: Team) => {
                    const member = stringToMember(`team:local:${team.name}`);
                    this.memberURLs[member.name] = ['/settings', 'teams', team.id];
                    // We'll refresh the sorted map for the chef-table below.
                    this.addAvailableMember(member, false);
                  });

                  users.forEach((user: User) => {
                    const member = stringToMember(`user:local:${user.username}`);
                    this.memberURLs[member.name] = ['/settings', 'users', user.username];
                    // We'll refresh the sorted map for the chef-table below.
                    this.addAvailableMember(member, false);
                  });

                  this.policy.members.forEach((memberName: string) => {
                    if (memberName in this.membersAvailableMap) {
                      this.removeAvailableMember(memberName, false);
                    }
                  });

                  // Now that the membersAvailableMap is correct, refresh.
                  this.refreshSortedMembersAvailable();
                  this.loading = false;
              });
            } else {
              this.loading = true;
            }
          });
        });

    this.store.dispatch(new GetTeams());
    this.store.dispatch(new GetUsers());
  }

  addAvailableMember(member: Member, refresh: boolean): void {
    this.membersAvailableMap[member.name] = member;
    if (refresh) {
      this.refreshSortedMembersAvailable();
    }
  }

  removeAvailableMember(memberName: string, refresh: boolean): void {
    delete this.membersAvailableMap[memberName];
    if (refresh) {
      this.refreshSortedMembersAvailable();
    }
  }

  refreshSortedMembersAvailable(): void {
    this.sortedMembersAvailable = this.membersMapAvailableToSortedToArray();
  }

  addMembers(): void {
    this.addingMembers = true;
    this.addMembersFailed = '';
    this.store.dispatch(new AddPolicyMembers(<PolicyMembersMgmtPayload>{
      id: this.policy.id,
      members: this.membersToAddValues()
    }));

    const pendingAdd = new Subject<boolean>();
    this.store.select(addPolicyMembersStatus).pipe(
      filter(identity),
      takeUntil(pendingAdd))
      .subscribe((state) => {
        if (state === EntityStatus.loadingSuccess) {
          pendingAdd.next(true);
          pendingAdd.complete();
          this.addingMembers = false;
          this.router.navigate(this.backRoute(), { fragment: 'members' });
        }
        if (state === EntityStatus.loadingFailure) {
          this.store.select(addPolicyMembersHTTPError).pipe(
            filter(identity),
            takeUntil(pendingAdd)).subscribe((error: HttpErrorResponse) => {
              if (error.message === undefined) {
                this.addMembersFailed = 'An error occurred while attempting ' +
                'to add members. Please try again.';
              } else {
                this.addMembersFailed = `Failed to add members: ${error.message}`;
              }
              this.addingMembers = false;
          });
        }
      });
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  backRoute(): string[] {
    return ['/settings', 'policies', this.policy.id];
  }

  public closeModal(): void {
    this.resetModal();
    this.modalVisible = false;
  }

  public openModal(): void {
    this.resetModal();
    this.modalVisible = true;
  }

  resetModal(): void {
    this.expressionForm.reset();
    this.unparsableMember = false;
    this.duplicateMember = false;
  }

  memberHasURL(member: Member): boolean {
    return member.name in this.memberURLs;
  }

  memberURL(member: Member): string[] {
    return this.memberURLs[member.name];
  }

  membersMapAvailableToSortedToArray(): Member[] {
    return Object.values(this.membersAvailableMap)
      .sort(
        (a, b) => {
          // See https://stackoverflow.com/a/38641281 for these options
          const opts = { numeric: true, sensitivity: 'base' };
          // sort by displayName then by name
          return a.displayName.localeCompare(b.displayName, undefined, opts) ||
            a.displayName.localeCompare(b.displayName, undefined, { numeric: true }) ||
            a.name.localeCompare(b.name, undefined, opts);
        });
  }

  membersToAddValues(): Member[] {
    return Object.values(this.membersToAdd);
  }

  addOrRemoveQueuedMember(checked: boolean, member: Member): void {
    if (checked) {
      this.membersToAdd[member.name] = member;
    } else {
      delete this.membersToAdd[member.name];
    }
  }

  isMemberChecked(member: Member): boolean {
    return member.name in this.membersToAdd;
  }

  public validateAndAddExpression(): void {
    this.unparsableMember = false;
    this.duplicateMember = false;
    this.alreadyPolicyMember = false;

    const member = stringToMember(this.expressionForm.value.expression);

    if (member.type === Type.Unknown) {
      this.unparsableMember = true;
      return;
    }

    this.duplicateMember = member.name in this.membersAvailableMap;
    if (this.duplicateMember) {
      return;
    }

    this.policy.members.forEach((policyMember: string) => {
      if (policyMember === member.name) {
        this.alreadyPolicyMember = true;
        return;
      }
    });
    if (this.alreadyPolicyMember) {
      return;
    }

    this.addAvailableMember(member, true);
    this.addOrRemoveQueuedMember(true, member);
    this.closeModal();
  }
}
