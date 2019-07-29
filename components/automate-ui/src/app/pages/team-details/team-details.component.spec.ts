import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { Router } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { routerReducer } from '@ngrx/router-store';
import { MockComponent } from 'ng2-mock-component';
import { StoreModule, Store } from '@ngrx/store';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { policyEntityReducer } from 'app/entities/policies/policy.reducer';
import {
  userEntityReducer,
  UserEntityInitialState
} from 'app/entities/users/user.reducer';
import { User, HashMapOfUsers } from 'app/entities/users/user.model';
import { GetUsersSuccess } from 'app/entities/users/user.actions';
import {
  teamEntityReducer,
  TeamEntityInitialState
} from 'app/entities/teams/team.reducer';
import {
  GetTeamSuccess,
  GetTeamUsersSuccess
} from 'app/entities/teams/team.actions';
import { Team } from 'app/entities/teams/team.model';
import { TeamDetailsComponent, TeamTabName } from './team-details.component';

describe('TeamDetailsComponent', () => {
  let component: TeamDetailsComponent;
  let fixture: ComponentFixture<TeamDetailsComponent>;
  let router: Router;

  const initialState = {
    router: {
      state: {
        url: '/settings/teams/a-team-uuid-01',
        params: { id: 'a-team-uuid-01' },
        queryParams: {},
        fragment: ''
      },
      navigationId: 0 // what's that zero?
    },
    users: UserEntityInitialState,
    teams: TeamEntityInitialState
  };

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'app-admin-sidebar' }),
        MockComponent({ selector: 'app-user-table',
          inputs: [
            'baseUrl',
            'users$',
            'removeText',
            'addButtonText',
            'addButtonEnabled',
            'overridePermissionsCheck']
        }),
        MockComponent({ selector: 'app-user-team-membership-table',
          inputs: ['usersToFilter', 'users$', 'removeText', 'addButtonText'] }),
        MockComponent({ selector: 'chef-breadcrumb', inputs: ['link'] }),
        MockComponent({ selector: 'chef-breadcrumbs' }),
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-input' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-option' }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-tab-selector',
          inputs: ['value', 'routerLink', 'fragment']
        }),
        TeamDetailsComponent
      ],
      imports: [
        ReactiveFormsModule,
        RouterTestingModule,
        StoreModule.forRoot({
          router: routerReducer,
          teams: teamEntityReducer,
          users: userEntityReducer,
          policies: policyEntityReducer
        }, { initialState })
      ]
    }).compileComponents();
  }));

  const someTeam = <Team>{
    id: 'some-team',
    name: 'some team',
    guid: 'a-team-uuid-01',
    projects: []
  };

  beforeEach(() => {
    router = TestBed.get(Router);
    spyOn(router, 'navigate').and.stub();

    fixture = TestBed.createComponent(TeamDetailsComponent);
    component = fixture.componentInstance;
    component.team = someTeam;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  it('defaults to showing users section', () => {
    expect(component.tabValue).toBe('users');
  });

  it('show users section when users tab is selected', () => {
    const tabName: TeamTabName = 'users';
    component.onSelectedTab({ target: { value: tabName } });
    expect(component.tabValue).toBe(tabName);
  });

  it('show details section when details tab is selected', () => {
    const tabName: TeamTabName = 'details';
    component.onSelectedTab({ target: { value: tabName } });
    expect(component.tabValue).toBe(tabName);
  });

  describe('empty state', () => {
    let store: Store<NgrxStateAtom>;
    beforeEach(() => {
      store = TestBed.get(Store);
      store.dispatch(new GetTeamSuccess(someTeam));
      store.dispatch(new GetTeamUsersSuccess({
        user_ids: []
      }));
      fixture.detectChanges();
    });

    it('users array should be empty', () => {
      component.sortedUsers$.subscribe((users) => {
        expect(users.length).toBe(0);
      });
    });
  });

  describe('add users', () => {
    let store: Store<NgrxStateAtom>;

    const user1 = <User>{
      id: 'user1',
      name: 'user1',
      membership_id: 'uuid-1'
    };
    const user2 = <User>{
      id: 'user2',
      name: 'user2',
      membership_id: 'uuid-2'
    };
    const usersToAdd = <HashMapOfUsers>{
      'user1': user1,
      'user2': user2
    };

    beforeEach(() => {
      store = TestBed.get(Store);
      store.dispatch(new GetTeamSuccess(someTeam));
      store.dispatch(new GetUsersSuccess({
        users: [user1, user2]
      }));
    });

    it('successfully adds users', () => {
      component.teamMembershipView = false;
      component.toggleUserMembershipView();
      component.addUsers(usersToAdd);

      expect(component.teamMembershipView).toBe(false);
      store.dispatch(new GetTeamUsersSuccess({
        user_ids: [user1.membership_id, user2.membership_id]
      }));

      for (const user of [user1, user2]) {
        component.sortedUsers$.subscribe(users => {
          expect(users).toContain(user);
        });
      }
    });
  });

  describe('sortedUsers$', () => {
    let store: Store<NgrxStateAtom>;
    beforeEach(() => {
      store = TestBed.get(Store);
    });

    it('intermixes capitals and lowercase with lowercase first', () => {
      store.dispatch(new GetTeamUsersSuccess({ user_ids: ['user-id-1', 'user-id-2'] }));
      store.dispatch(new GetUsersSuccess({ users: [
        { membership_id: 'user-id-1', name: 'Alice', id: 'alice1' },
        { membership_id: 'user-id-2', name: 'alice', id: 'alice2' }
      ]}));
      component.sortedUsers$.subscribe(users => {
        expect(users.length).toBe(2);
        expect(users[0]).toEqual(jasmine.objectContaining({ name: 'alice' }));
        expect(users[1]).toEqual(jasmine.objectContaining({ name: 'Alice' }));
      });
    });

    it('sorts by whole string before case', () => {
      store.dispatch(new GetTeamUsersSuccess({
        user_ids: ['user-id-1', 'user-id-20', 'user-id-2']
      }));
      store.dispatch(new GetUsersSuccess({ users: [
        { membership_id: 'user-id-1', name: 'alice in wonderland', id: 'alice1' },
        { membership_id: 'user-id-20', name: 'alice', id: 'alice2' },
        { membership_id: 'user-id-2', name: 'Alice', id: 'alice3' }
      ]}));
      component.sortedUsers$.subscribe(users => {
        expect(users.length).toBe(3);
        expect(users[0]).toEqual(jasmine.objectContaining({ name: 'alice' }));
        expect(users[1]).toEqual(jasmine.objectContaining({ name: 'Alice' }));
        expect(users[2]).toEqual(jasmine.objectContaining({ name: 'alice in wonderland' }));
      });
    });

    it('sorts by name then by username', () => {
      store.dispatch(new GetTeamUsersSuccess({
        user_ids: ['user-id-22', 'user-id-1', 'user-id-20', 'user-id-2'] }));
      store.dispatch(new GetUsersSuccess({ users: [
       { membership_id: 'user-id-22', name: 'Bob', id: 'builder2001' },
       { membership_id: 'user-id-2', name: 'Bob', id: 'builder2000' },
       { membership_id: 'user-id-1', name: 'Alice in Wonderland', id: 'alice' },
       { membership_id: 'user-id-20', name: 'alice', id: 'the-other-alice' }
     ]}));
     component.sortedUsers$.subscribe(users => {
       expect(users.length).toBe(4);
       expect(users[0]).toEqual(jasmine.objectContaining({ name: 'alice' }));
       expect(users[1]).toEqual(jasmine.objectContaining({ name: 'Alice in Wonderland' }));
       expect(users[2]).toEqual(
         jasmine.objectContaining({ name: 'Bob', id: 'builder2000' }));
       expect(users[3]).toEqual(
         jasmine.objectContaining({ name: 'Bob', id: 'builder2001' }));
     });
    });

    it('uses natural ordering in name', () => {
      store.dispatch(new GetTeamUsersSuccess({
        user_ids: ['user-id-1', 'user-id-2', 'user-id-3', 'user-id-4', 'user-id-5'] }));
      store.dispatch(new GetUsersSuccess({ users: [
        { membership_id: 'user-id-1', name: 'Alice01', id: 'alice1' },
        { membership_id: 'user-id-2', name: 'Alice300', id: 'alice2' },
        { membership_id: 'user-id-3', name: 'Alice3', id: 'alice3' },
        { membership_id: 'user-id-4', name: 'Alice-2', id: 'alice4' },
        { membership_id: 'user-id-5', name: 'alice', id: 'alice5' }
      ]}));
      component.sortedUsers$.subscribe(users => {
        expect(users.length).toBe(5);
        expect(users[0]).toEqual(jasmine.objectContaining({ name: 'alice' }));
        expect(users[1]).toEqual(jasmine.objectContaining({ name: 'Alice-2' }));
        expect(users[2]).toEqual(jasmine.objectContaining({ name: 'Alice01' }));
        expect(users[3]).toEqual(jasmine.objectContaining({ name: 'Alice3' }));
        expect(users[4]).toEqual(jasmine.objectContaining({ name: 'Alice300' }));
      });
    });

    it('uses natural ordering in username', () => {
      store.dispatch(new GetTeamUsersSuccess({
        user_ids: ['user-id-1', 'user-id-2', 'user-id-3', 'user-id-4', 'user-id-5'] }));
      store.dispatch(new GetUsersSuccess({ users: [
        { membership_id: 'user-id-1', name: 'Alice', id: 'Alice01' },
        { membership_id: 'user-id-2', name: 'Alice', id: 'Alice300' },
        { membership_id: 'user-id-3', name: 'Alice', id: 'Alice3' },
        { membership_id: 'user-id-4', name: 'Alice', id: 'Alice-2' },
        { membership_id: 'user-id-5', name: 'Alice', id: 'alice' }
      ]}));
      component.sortedUsers$.subscribe(users => {
        expect(users.length).toBe(5);
        expect(users[0]).toEqual(jasmine.objectContaining({ id: 'alice' }));
        expect(users[1]).toEqual(jasmine.objectContaining({ id: 'Alice-2' }));
        expect(users[2]).toEqual(jasmine.objectContaining({ id: 'Alice01' }));
        expect(users[3]).toEqual(jasmine.objectContaining({ id: 'Alice3' }));
        expect(users[4]).toEqual(jasmine.objectContaining({ id: 'Alice300' }));
      });
    });
  });
});

