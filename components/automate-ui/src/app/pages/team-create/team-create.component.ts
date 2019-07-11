import { Component, OnInit } from '@angular/core';
import { FormBuilder, Validators, FormGroup } from '@angular/forms';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Observable } from 'rxjs';

import { Regex } from 'app/helpers/auth/regex';
import {
  CreateTeam,
  CreateTeamPayload
} from 'app/entities/teams/team.actions';
import { iamMajorVersion } from 'app/entities/policies/policy.selectors';


// TODO: We should only allow users to nav to this page if they have
// permissions on user create.
@Component({
  selector: 'app-team-create',
  templateUrl: './team-create.component.html',
  styleUrls: ['./team-create.component.scss']
})
export class TeamCreateComponent implements OnInit {

  public teamCreateForm: FormGroup;
  public iamMajorVersion$: Observable<string>;
  private isV1 = true;

  constructor(
    private store: Store<NgrxStateAtom>,
    fb: FormBuilder
  ) {
    this.teamCreateForm = fb.group({
      // Must stay in sync with error checks in team-create.component.html
      teamId: ['',
        [Validators.required, Validators.pattern(Regex.patterns.ID), Validators.maxLength(64)]],
      teamName: ['',
        [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]]
    });
    this.iamMajorVersion$ = store.select(iamMajorVersion);
  }

  public get versionedNameOrId(): string {
    return this.isV1 ? 'name' : 'ID';
  }

  public get versionedDescOrName(): string {
    return this.isV1 ? 'description' : 'name';
  }

  ngOnInit(): void {
    this.iamMajorVersion$.subscribe((version) => {
      if (version === null) { return; }
      this.isV1 = version === 'v1';
    });
  }

  saveTeam(): void {
    const formValues = this.teamCreateForm.value;

    const teamData = <CreateTeamPayload>{
      id: formValues.teamId,
      name: formValues.teamName,
      projects: [] // TODO: fix once projects implemented here
    };

    this.store.dispatch(new CreateTeam(teamData));
  }
}
