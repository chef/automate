import { Component, OnInit, OnDestroy } from '@angular/core';
import { FormBuilder, FormGroup, Validators, FormControl } from '@angular/forms';
import { trigger, transition, style, animate, state, keyframes } from '@angular/animations';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { identity } from 'lodash/fp';
import { filter, pluck, takeUntil, distinctUntilChanged, map } from 'rxjs/operators';
import { combineLatest, Subject, Observable } from 'rxjs';

import { ChefSorters } from 'app/helpers/auth/sorter';
import { Regex } from 'app/helpers/auth/regex';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeParams } from 'app/route.selectors';
import { EntityStatus, allLoadedSuccessfully, pending } from 'app/entities/entities';
import {
  GetPolicy, AddPolicyMembers, PolicyMembersMgmtPayload
} from 'app/entities/policies/policy.actions';
import {
  policyFromRoute,
  getStatus as getPolicyStatus,
  addPolicyMembersStatus
} from 'app/entities/policies/policy.selectors';
import { Policy, Member, Type, stringToMember } from 'app/entities/policies/policy.model';
import { allTeams, getAllStatus as getAllTeamsStatus } from 'app/entities/teams/team.selectors';
import { Team } from 'app/entities/teams/team.model';
import { GetTeams } from 'app/entities/teams/team.actions';
import { allUsers, getStatus as getAllUsersStatus } from 'app/entities/users/user.selectors';
import {
  GetUsers
} from 'app/entities/users/user.actions';
import { User } from 'app/entities/users/user.model';

export type FieldName = 'type' | 'identityProvider' | 'name';

@Component({
  selector: 'app-policy-add-members',
  templateUrl: './policy-add-members.component.html',
  styleUrls: ['./policy-add-members.component.scss'],
  animations: [
    trigger('dropInAnimation', [
      state('void', style({ 'opacity': '0', 'height' : '0' })),
      transition('void => *', animate(400, keyframes([
        style({opacity: 0, offset: 0}),
        style({opacity: 0, height: '{{height}}px', offset: 0.3}),
        style({opacity: 1, height: '{{height}}px', offset: 1})
      ]))),
      transition('* => void', animate(150, keyframes([
        style({ opacity: 1, height: '{{height}}px', offset: 0 }),
        style({ opacity: 0, height: '{{height}}px', offset: 0.1 }),
        style({ opacity: 0, height: '0px', offset: 1 })
      ])))
    ])
  ]
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
  public loading$: Observable<boolean>;
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
  public expressionOutput: string;
  public allIdentity: string;
  public nameOrId: string;
  public ldapOrSaml = false;

  // Animation
  public triggerValue: 'void' | '*';


  constructor(
    private store: Store<NgrxStateAtom>,
    private router: Router,
    fb: FormBuilder) {

    this.expressionForm = fb.group({
      // Must stay in sync with error checks in policy-add-members.component.html
      type: ['', Validators.required]
    });
  }

  ngOnInit(): void {
    this.store.select(routeParams).pipe(
      takeUntil(this.isDestroyed),
      pluck('id'),
      filter(identity),
      distinctUntilChanged()
    ).subscribe((id: string) => this.store.dispatch(new GetPolicy({ id })));

    this.store.dispatch(new GetTeams());
    this.store.dispatch(new GetUsers());

    this.loading$ = combineLatest([
        this.store.select(getAllTeamsStatus),
        this.store.select(getAllUsersStatus),
        this.store.select(getPolicyStatus)
      ]).pipe(
        map((statuses: EntityStatus[]) => !allLoadedSuccessfully(statuses)));

    combineLatest([
        this.store.select(allTeams),
        this.store.select(allUsers),
        this.store.select(policyFromRoute),
        this.loading$
      ]).pipe(
        takeUntil(this.isDestroyed),
        filter(([_teams, _users, _policy, loading]) => !loading)
      ).subscribe(([teams, users, policy, _loading]) => {
          this.policy = <Policy>Object.assign({}, policy);
          teams.forEach((team: Team) => {
            const member = stringToMember(`team:local:${team.id}`);
            this.memberURLs[member.name] = ['/settings', 'teams', team.id];
            // We'll refresh the sorted map for the chef-table below.
            this.addAvailableMember(member, false);
          });

          users.forEach((user: User) => {
            const member = stringToMember(`user:local:${user.id}`);
            this.memberURLs[member.name] = ['/settings', 'users', user.id];
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
      });

    this.store.select(addPolicyMembersStatus).pipe(
      takeUntil(this.isDestroyed),
      filter(addState => this.addingMembers && !pending(addState)))
      .subscribe(() => {
        this.addingMembers = false;
        this.closePage();
      });
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
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  closePage() {
    this.router.navigate(this.backRoute(), { fragment: 'members' });
  }

  getHeading() {
    return this.policy
      ? `Add Members to ${this.policy.name}`
      : '';
  }

  getMemberConfirmBtnText() {
    if (this.addingMembers) {
      return (this.membersToAddValues().length < 2)
        ? 'Adding Member...'
        : `Adding ${this.membersToAddValues().length} Members...`;
    } else {
      return (this.membersToAddValues().length < 2)
        ? 'Add Member'
        : `Add ${this.membersToAddValues().length} Members`;
    }
  }

  getErrorMessage() {
    return this.addMembersFailed.length > 0 ? this.addMembersFailed : undefined;
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
    this.resetForm();
    this.resetErrors();
  }

  memberHasURL(member: Member): boolean {
    return member.name in this.memberURLs;
  }

  memberURL(member: Member): string[] {
    return this.memberURLs[member.name];
  }

  membersMapAvailableToSortedToArray(): Member[] {
    const membersAvailable = Object.values(this.membersAvailableMap);
    // sort by displayName then by name
    return ChefSorters.naturalSort(membersAvailable, ['displayName', 'name']);
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

  private resetErrors(): void {
    this.unparsableMember = false;
    this.duplicateMember = false;
    this.alreadyPolicyMember = false;
  }

  public validateAndAddExpression(): void {
    this.resetErrors();

    const member = stringToMember(this.expressionOutput.trim());

    if (member.type === Type.Unknown) {
      this.unparsableMember = true;
      return;
    }

    if (member.name in this.membersAvailableMap) {
      this.duplicateMember = true;
      return;
    }

    this.policy.members.forEach((policyMember: string) => {
      if (policyMember === member.name) {
        this.alreadyPolicyMember = true;
        return;
      }
    });

    this.addAvailableMember(member, true);
    this.addOrRemoveQueuedMember(true, member);
    this.closeModal();
  }

  private showInputs(fieldName: FieldName): void {

    const formValues = this.expressionForm.value;
    const matchAllWildCard = '*';

    this.setFormLabels(formValues.type);

    switch (fieldName) {
      case 'type':
        this.resetFormControls();
        this.resetErrors();
        if (formValues.type === 'user' || formValues.type === 'team') {
          this.addIdentityControl();
        } else if (formValues.type === 'token') {
          this.addNameControl(null);
        }
        break;
      case 'identityProvider':
        this.resetErrors();
        this.expressionForm.removeControl('name');
        if (formValues.identityProvider !== matchAllWildCard) {
          this.addNameControl(formValues.identityProvider);
        }
        break;
      case 'name': // fallthrough
      default:
        break;
    }

  }

  private setFormLabels(typeValue): void {
    if (typeValue === 'token') {
      this.nameOrId = 'ID';
    } else {
      this.allIdentity = typeValue;
      this.nameOrId = 'Name';
    }
  }

  private resetFormControls(): void {
    this.expressionForm.removeControl('identityProvider');
    this.expressionForm.removeControl('name');
  }

  private resetForm(): void {
    this.resetFormControls();
    this.expressionForm.reset();
    this.expressionOutput = '';
  }

  private addIdentityControl(): void {
    this.expressionForm.addControl('identityProvider', new FormControl('', Validators.required));
  }

  private addNameControl(identityProvider): void {
    if (identityProvider === 'ldap' || identityProvider === 'saml' ) {
        this.ldapOrSaml = true;
        this.expressionForm.addControl('name', new FormControl('',
          [
            Validators.required,
            Validators.pattern(Regex.patterns.NO_MIXED_WILDCARD_ALLOW_SPECIAL)
          ]
        )
      );
    } else {
        this.ldapOrSaml = false;
        this.expressionForm.addControl('name', new FormControl('',
          [
            Validators.required,
            Validators.pattern(Regex.patterns.NO_MIXED_WILDCARD_ALLOW_HYPHEN)
          ]
        )
      );
    }
  }


  private setExpressionOutput(): void {
    this.expressionOutput =
      (Object.values(this.expressionForm.value) as string[])
             .filter(value => value != null && value.length > 0)
             .join(':');
  }

  public updateFormDisplay(fieldName: FieldName): void {
    this.showInputs(fieldName);
    this.setExpressionOutput();
  }
}
