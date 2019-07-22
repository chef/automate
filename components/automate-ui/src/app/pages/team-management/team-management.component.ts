import { Component, OnInit, EventEmitter } from '@angular/core';
import { Store, select } from '@ngrx/store';
import { Observable, Subject } from 'rxjs';
import { map, filter, takeUntil } from 'rxjs/operators';
import { identity } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { loading, EntityStatus } from 'app/entities/entities';
import { iamMajorVersion } from 'app/entities/policies/policy.selectors';
import {
  createError,
  createStatus,
  allTeams,
  getAllStatus } from 'app/entities/teams/team.selectors';
import { Team } from 'app/entities/teams/team.model';
import { CreateTeam, DeleteTeam, GetTeams } from 'app/entities/teams/team.actions';
import { Validators, FormGroup, FormBuilder } from '@angular/forms';
import { Regex } from 'app/helpers/auth/regex';
import { Router } from '@angular/router';

@Component({
  selector: 'app-team-management',
  templateUrl: './team-management.component.html',
  styleUrls: ['./team-management.component.scss']
})
export class TeamManagementComponent implements OnInit {
  public sortedTeams$: Observable<Team[]>;
  public loading$: Observable<boolean>;
  public teamToDelete: Team;
  public deleteModalVisible = false;
  public createModalVisible = false;
  public createTeamForm: FormGroup;
  public creatingTeam = false;
  public conflictErrorEvent = new EventEmitter<boolean>();
  public iamMajorVersion$: Observable<string>;

 constructor(
    private store: Store<NgrxStateAtom>,
    private router: Router,
    fb: FormBuilder
    ) {
    this.loading$ = store.select(getAllStatus).pipe(map(loading));
    this.sortedTeams$ = store.select(allTeams).pipe(
      map((teams: Team[]) => teams.sort(
        (a, b) => {
          // See https://stackoverflow.com/a/38641281 for these options
          const opts = { numeric: true, sensitivity: 'base' };
          // TODO: still need to observe case for v1; not needed for v2; simplify when v1 removed.
          // Sort by name then by cased-name, since no other field is useful as a secondary sort;
          // this ensures stable sort with respect to case, so 'a' always comes before 'A'.
          return a.id.localeCompare(b.id, undefined, opts)
          || a.id.localeCompare(b.id, undefined, {numeric: true});
        }
      )));
    this.iamMajorVersion$ = store.select(iamMajorVersion);
    this.createTeamForm = fb.group({
      // Must stay in sync with error checks in create-object-modal.component.html
      name: ['', Validators.required],
      id: ['',
        [Validators.required, Validators.pattern(Regex.patterns.ID), Validators.maxLength(64)]]
    });
  }

  ngOnInit() {
    this.store.dispatch(new GetTeams());
  }

  public closeModal(): void {
    this.deleteModalVisible = false;
  }

  public startTeamDelete(team: Team): void {
    this.teamToDelete = team;
    this.deleteModalVisible = true;
  }

  public deleteTeam(): void {
    this.closeModal();
    this.store.dispatch(new DeleteTeam(this.teamToDelete));
  }

  public createTeam(): void {
    this.creatingTeam = true;
    const team = {
      id: this.createTeamForm.controls['id'].value,
      name: this.createTeamForm.controls['name'].value.trim(),
      projects: []
    };
    this.store.dispatch(new CreateTeam(team));

    const pendingCreate = new Subject<boolean>();
    this.store.pipe(
      select(createStatus),
      filter(identity),
      takeUntil(pendingCreate))
      .subscribe((state) => {
        if (!loading(state)) {
          pendingCreate.next(true);
          pendingCreate.complete();
          this.creatingTeam = false;
          if (state === EntityStatus.loadingSuccess) {
            this.closeCreateModal();
            this.router.navigate(['/settings', 'teams', team.id]);
          }
          if (state === EntityStatus.loadingFailure) {
            const pendingCreateError = new Subject<boolean>();
            this.store.pipe(
              select(createError),
              filter(identity),
              takeUntil(pendingCreateError))
              .subscribe((error) => {
                pendingCreateError.next(true);
                pendingCreateError.complete();
                if (error.status === 409) {
                  this.conflictErrorEvent.emit(true);
                // Close the modal on any error other than conflict and display in banner.
                } else {
                  this.closeCreateModal();
                }
            });
          }
        }
      });
  }

  public openCreateModal(): void {
    this.createModalVisible = true;
    this.resetCreateModal();
  }

  public closeCreateModal(): void {
    this.createModalVisible = false;
    this.resetCreateModal();
  }

  resetCreateModal(): void {
    this.creatingTeam = false;
    this.createTeamForm.reset();
  }
}
