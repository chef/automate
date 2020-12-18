import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { MatOptionSelectionChange } from '@angular/material/core/option';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { StoreModule, Store } from '@ngrx/store';

import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { Team } from 'app/entities/teams/team.model';
import {
  GetTeamsSuccess,
  CreateTeamSuccess,
  DeleteTeamSuccess
} from 'app/entities/teams/team.actions';
import { TeamManagementComponent } from './team-management.component';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';

describe('TeamManagementComponent', () => {
  let component: TeamManagementComponent;
  let fixture: ComponentFixture<TeamManagementComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'app-authorized',
                        inputs: ['allOf'] }),
        MockComponent({ selector: 'app-delete-object-modal',
                        inputs: ['default', 'visible', 'objectNoun', 'objectName'],
                        outputs: ['close', 'deleteClicked'] }),
        MockComponent({
          selector: 'app-create-object-modal',
          inputs: ['visible', 'creating', 'conflictErrorEvent', 'objectNoun',
            'createForm', 'assignableProjects'],
          outputs: ['close', 'createClicked']
        }),
        MockComponent({ selector: 'chef-button',
                        inputs: ['disabled', 'routerLink'] }),
        MockComponent({ selector: 'mat-select' }),
        MockComponent({ selector: 'mat-option' }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-icon' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-toolbar' }),
        MockComponent({ selector: 'chef-table' }),
        MockComponent({ selector: 'chef-thead' }),
        MockComponent({ selector: 'chef-tbody' }),
        MockComponent({ selector: 'chef-tr' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        TeamManagementComponent
      ],
      providers: [
        FeatureFlagsService
      ],
      imports: [
        FormsModule,
        ReactiveFormsModule,
        RouterTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TeamManagementComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  describe('create team', () => {
    let store: Store<NgrxStateAtom>;
    const team: Team = {
      guid: 'uuid-1',
      id: 'new-team',
      name: 'new team',
      projects: []
    };

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('openCreateModal opens modal', () => {
      expect(component.createModalVisible).toBe(false);
      component.openCreateModal();
      expect(component.createModalVisible).toBe(true);
    });

    it('on success, closes modal and adds new team', () => {
      component.openCreateModal();
      component.createTeamForm.controls['name'].setValue(team.name);
      component.createTeamForm.controls['id'].setValue(team.id);
      component.createTeam();

      store.dispatch(new CreateTeamSuccess(team));

      component.sortedTeams$.subscribe(teams => {
        expect(teams).toContain(team);
      });
    });
  });

  describe('delete team', () => {
    let store: Store<NgrxStateAtom>;
    const mockEvent = { isUserInput: true } as MatOptionSelectionChange;

    const deleteTeam: Team = {
      guid: 'uuid-1',
      id: 'new-team',
      name: 'new team',
      projects: []
    };


    beforeEach(() => {
      store = TestBed.inject(Store);
      store.dispatch(new GetTeamsSuccess({
        teams: []
      }));
      fixture.detectChanges();
    });

    it('opens the delete modal', () => {
      component.startTeamDelete(mockEvent, deleteTeam);
      fixture.detectChanges();

      expect(component.deleteModalVisible).toBe(true);
    });

    it('confirming delete closes modal and removes the team', () => {
      component.deleteTeam();

      store.dispatch(new DeleteTeamSuccess(deleteTeam));
      fixture.detectChanges();

      expect(component.deleteModalVisible).toBe(false);
      component.sortedTeams$.subscribe(teams => {
        expect(teams).not.toContain(deleteTeam);
      });
    });
  });
});
