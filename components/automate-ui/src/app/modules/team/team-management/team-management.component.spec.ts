import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
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
import { ChefKeyboardEvent } from 'app/types/material-types';

describe('TeamManagementComponent', () => {
  let component: TeamManagementComponent;
  let fixture: ComponentFixture<TeamManagementComponent>;

  beforeEach(async(() => {
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
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-icon' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-toolbar' }),
        MockComponent({ selector: 'chef-table-new' }),
        MockComponent({ selector: 'chef-table-header' }),
        MockComponent({ selector: 'chef-table-body' }),
        MockComponent({ selector: 'chef-table-row' }),
        MockComponent({ selector: 'chef-table-header-cell' }),
        MockComponent({ selector: 'chef-table-cell' }),
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

  describe('create v1 team', () => {
    let store: Store<NgrxStateAtom>;
    const team = <Team> {
        guid: 'uuid-1',
        id: 'new', // corresponds to v1 team Name
        name: 'The Brand New Team', // corresponds to v1 team Description
        projects: []
      };

    beforeEach(() => {
      store = TestBed.inject(Store);
      component.isIAMv2 = false;
    });

    it('openCreateModal on v1 opens v1 modal', () => {
      expect(component.createV1TeamModalVisible).toBe(false);
      expect(component.createModalVisible).toBe(false);
      component.openCreateModal();
      expect(component.createV1TeamModalVisible).toBe(true);
      expect(component.createModalVisible).toBe(false);
    });

    it('opening create modal resets name and description to empty string', () => {
      component.createV1TeamForm.controls['name'].setValue('any');
      component.createV1TeamForm.controls['description'].setValue('any');
      component.openCreateModal();
      expect(component.createV1TeamForm.controls['name'].value).toBe(null);
      expect(component.createV1TeamForm.controls['description'].value).toBe(null);
    });

    it('on success, closes modal and adds new team', () => {
      component.createV1TeamForm.controls['name'].setValue(team.id);
      component.createV1TeamForm.controls['description'].setValue(team.name);
      component.createV1Team();

      store.dispatch(new CreateTeamSuccess(team));

      component.sortedTeams$.subscribe(teams => {
        expect(teams).toContain(team);
      });
    });

    it('on conflict error, modal is open with conflict error', () => {
      spyOn(component.conflictErrorEvent, 'emit');
      component.openCreateModal();
      component.createV1TeamForm.controls['name'].setValue(team.id);
      component.createV1TeamForm.controls['description'].setValue(team.name);
      component.createV1Team();

      const conflict = <HttpErrorResponse>{
        status: HttpStatus.CONFLICT,
        ok: false
      };
      store.dispatch(new CreateTeamFailure(conflict));

      expect(component.createV1TeamModalVisible).toBe(true);
      expect(component.conflictErrorEvent.emit).toHaveBeenCalled();
    });

    it('on create error, modal is closed with failure banner', () => {
      spyOn(component.conflictErrorEvent, 'emit');
      component.openCreateModal();
      component.createV1TeamForm.controls['name'].setValue(team.id);
      component.createV1TeamForm.controls['description'].setValue(team.name);
      component.createV1Team();

      const error = <HttpErrorResponse>{
        status: HttpStatus.INTERNAL_SERVER_ERROR,
        ok: false
      };
      store.dispatch(new CreateTeamFailure(error));

      expect(component.createV1TeamModalVisible).toBe(false);
      expect(component.conflictErrorEvent.emit).toHaveBeenCalledWith(false);
    });
  });

  describe('create v2 team', () => {
    let store: Store<NgrxStateAtom>;
    const team: Team = {
      guid: 'uuid-1',
      id: 'new-team',
      name: 'new team',
      projects: []
    };

    beforeEach(() => {
      store = TestBed.inject(Store);
      component.isIAMv2 = true;
    });

    it('openCreateModal on v2 opens v2 modal', () => {
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
    const mockChefKeyEvent = new KeyboardEvent('keypress') as ChefKeyboardEvent;
    mockChefKeyEvent.isUserInput = true;

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
      component.startTeamDelete(mockChefKeyEvent, deleteTeam);
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
