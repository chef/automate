import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { StoreModule, Store } from '@ngrx/store';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { policyEntityReducer } from 'app/entities/policies/policy.reducer';
import { teamEntityReducer } from 'app/entities/teams/team.reducer';
import { GetTeamsSuccess } from 'app/entities/teams/team.actions';
import { TeamManagementComponent } from './team-management.component';

describe('TeamManagementComponent', () => {
  let component: TeamManagementComponent;
  let fixture: ComponentFixture<TeamManagementComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'app-admin-sidebar' }),
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
          inputs: ['visible', 'creating', 'conflictErrorEvent', 'objectNoun', 'createForm'],
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
          policies: policyEntityReducer
        })
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

  describe('sortedTeams$', () => {
    let store: Store<NgrxStateAtom>;
    beforeEach(() => {
      store = TestBed.get(Store);
    });

    it('uses natural ordering', () => {
      store.dispatch(new GetTeamsSuccess({
        teams: [
          { guid: 'uuid-1', id: 'Viewer01', name: 'Viewer01 name', projects: [] },
          { guid: 'uuid-2', id: 'Viewer300', name: 'Viewer300 name', projects: [] },
          { guid: 'uuid-3', id: 'Viewer3', name: 'hello Viewer3', projects: [] },
          { guid: 'uuid-4', id: 'Viewer-2', name: 'goodbye Viewer-2', projects: [] },
          { guid: 'uuid-6', id: 'viewer', name: 'a viewer', projects: [] }
        ]
      }));
      component.sortedTeams$.subscribe(teams => {
        expect(teams.length).toBe(5);
        expect(teams[0]).toEqual(jasmine.objectContaining({ id: 'viewer' }));
        expect(teams[1]).toEqual(jasmine.objectContaining({ id: 'Viewer-2' }));
        expect(teams[2]).toEqual(jasmine.objectContaining({ id: 'Viewer01' }));
        expect(teams[3]).toEqual(jasmine.objectContaining({ id: 'Viewer3' }));
        expect(teams[4]).toEqual(jasmine.objectContaining({ id: 'Viewer300' }));
      });
    });

  });
});
