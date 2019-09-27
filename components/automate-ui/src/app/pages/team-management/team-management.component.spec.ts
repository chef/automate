import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpErrorResponse } from '@angular/common/http';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { StoreModule, Store } from '@ngrx/store';

import { NgrxStateAtom, runtimeChecks } from 'app/ngrx.reducers';
import { HttpStatus } from 'app/types/types';
import { policyEntityReducer } from 'app/entities/policies/policy.reducer';
import { projectsFilterReducer } from 'app/services/projects-filter/projects-filter.reducer';
import { teamEntityReducer } from 'app/entities/teams/team.reducer';
import { Team } from 'app/entities/teams/team.model';
import {
  GetTeamsSuccess,
  CreateTeamSuccess,
  CreateTeamFailure,
  DeleteTeamSuccess
} from 'app/entities/teams/team.actions';
import { TeamManagementComponent } from './team-management.component';

describe('TeamManagementComponent', () => {
  let component: TeamManagementComponent;
  let fixture: ComponentFixture<TeamManagementComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'app-settings-sidebar' }),
        MockComponent({ selector: 'app-authorized',
                        inputs: ['allOf'] }),
        MockComponent({ selector: 'app-delete-object-modal',
                        inputs: ['default', 'visible', 'objectNoun', 'objectName'],
                        outputs: ['close', 'deleteClicked'] }),
        MockComponent({
          selector: 'app-create-v1-team-modal',
          inputs: ['visible', 'creating', 'conflictErrorEvent', 'createForm'],
          outputs: ['close', 'createClicked']
        }),
        MockComponent({
          selector: 'app-create-object-modal',
          inputs: ['visible', 'creating', 'conflictErrorEvent', 'objectNoun',
            'createForm', 'showProjectsDropdown', 'assignableProjects'],
          outputs: ['close', 'createClicked']
        }),
        MockComponent({ selector: 'chef-button',
                        inputs: ['disabled', 'routerLink'] }),
        MockComponent({ selector: 'chef-control-menu' }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-icon' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-option' }),
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
      imports: [
        FormsModule,
        ReactiveFormsModule,
        RouterTestingModule,
        StoreModule.forRoot({
          teams: teamEntityReducer,
          policies: policyEntityReducer,
          projectsFilter: projectsFilterReducer
        }, { runtimeChecks })
      ]
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
      store = TestBed.get(Store);
      component.isMajorV1 = true;
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
      expect(component.conflictErrorEvent.emit).not.toHaveBeenCalled();
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
      store = TestBed.get(Store);
      component.isMajorV1 = false;
    });

    it('openCreateModal on v2 opens v2 modal', () => {
      expect(component.createV1TeamModalVisible).toBe(false);
      expect(component.createModalVisible).toBe(false);
      component.openCreateModal();
      expect(component.createV1TeamModalVisible).toBe(false);
      expect(component.createModalVisible).toBe(true);
    });

    it('on success, closes modal and adds new team', () => {
      component.openCreateModal();
      component.createTeamForm.controls['name'].setValue(team.name);
      component.createTeamForm.controls['id'].setValue(team.id);
      component.createV2Team();

      store.dispatch(new CreateTeamSuccess(team));

      component.sortedTeams$.subscribe(teams => {
        expect(teams).toContain(team);
      });
    });
  });

  describe('delete team', () => {
    let store: Store<NgrxStateAtom>;
    const deleteTeam: Team = {
      guid: 'uuid-1',
      id: 'new-team',
      name: 'new team',
      projects: []
    };

    beforeEach(() => {
      store = TestBed.get(Store);
      store.dispatch(new GetTeamsSuccess({
        teams: []
      }));
      fixture.detectChanges();
    });

    it('opens the delete modal', () => {
      component.startTeamDelete(deleteTeam);
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
