import { Component, OnInit } from '@angular/core';
import { Store } from '@ngrx/store';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { loading } from 'app/entities/entities';
import { iamMajorVersion } from 'app/entities/policies/policy.selectors';
import { allTeams, getAllStatus } from 'app/entities/teams/team.selectors';
import { Team } from 'app/entities/teams/team.model';
import { DeleteTeam, GetTeams } from 'app/entities/teams/team.actions';

@Component({
  selector: 'app-team-management',
  templateUrl: './team-management.component.html',
  styleUrls: ['./team-management.component.scss']
})
export class TeamManagementComponent implements OnInit {
  public sortedTeams$: Observable<Team[]>;
  public loading$: Observable<boolean>;
  public teamToDelete: Team;
  public modalVisible = false;
  public iamMajorVersion$: Observable<string>;

 constructor(
    private store: Store<NgrxStateAtom>
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
  }

  ngOnInit() {
    this.store.dispatch(new GetTeams());
  }

  public closeModal(): void {
    this.modalVisible = false;
  }

  public startTeamDelete(team: Team): void {
    this.teamToDelete = team;
    this.modalVisible = true;
  }

  public deleteTeam(): void {
    this.closeModal();
    this.store.dispatch(new DeleteTeam(this.teamToDelete));
  }
}
