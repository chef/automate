import { ComponentFixture, TestBed } from '@angular/core/testing';
import { UserTeamMembershipTableComponent } from './user-team-membership-table.component';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { RouterTestingModule } from '@angular/router/testing';
import { User, userHashToArray } from 'app/entities/users/user.model';
import { StoreModule } from '@ngrx/store';
import { userEntityReducer } from 'app/entities/users/user.reducer';
import * as faker from 'faker';

describe('UserTeamMembershipTableComponent', () => {
  let component: UserTeamMembershipTableComponent;
  let fixture: ComponentFixture<UserTeamMembershipTableComponent>;
  const userAlreadyInList = <User>{
    id: faker.random.uuid(),
    name: 'Hank Venture',
    username: 'enrico_matasa'
  };

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        StoreModule.forRoot({
          users: userEntityReducer
        })
      ],
      declarations: [
        UserTeamMembershipTableComponent
      ],
      schemas: [
        CUSTOM_ELEMENTS_SCHEMA
      ]
    })
    .compileComponents();
    fixture = TestBed.createComponent(UserTeamMembershipTableComponent);
    component = fixture.componentInstance;
    spyOn(component, 'subscribeToUsersToFilter');
    fixture.detectChanges();
  });

  it('should be created', () => {
    fixture.detectChanges();
    expect(component).toBeTruthy();
  });

  describe('addOrRemoveUser', () => {
    const testUserToAddOrRemove = <User>{
      id: 'c39d5157-4198-478b-b75c-c2a77ac0ffa8',
      name: 'Dean Venture',
      username: 'deanie'
    };

    describe('when usersToAdd is empty', () => {
      describe('when the check has been checked', () => {
        it('it add the user to the list', () => {
          fixture.detectChanges();
          expect(userHashToArray(component.usersToAdd).length).toBe(0);
          component.addOrRemoveUser(true, testUserToAddOrRemove);
          expect(userHashToArray(component.usersToAdd).length).toBe(1);
        });
      });
    });

    describe('when usersToAdd has multipleUsers already', () => {
      beforeEach(() => {
        component.usersToAdd = {
          'enrico_matasa': userAlreadyInList,
          'kuzko': {
            id: 'c39d5157-4198-478b-b75c-c2a77ac0ffa8',
            name: 'Brock Samson',
            username: 'kuzko'
          }
        };
      });

      describe('when the check has been checked', () => {
        it('it add the user to the list', () => {
          fixture.detectChanges();
          expect(userHashToArray(component.usersToAdd).length).toBe(2);
          component.addOrRemoveUser(true, testUserToAddOrRemove);
          expect(userHashToArray(component.usersToAdd).length).toBe(3);
        });
      });

      describe('when the check has been unchecked', () => {
        it('it removes the correct user from the list', () => {
          fixture.detectChanges();
          expect(userHashToArray(component.usersToAdd).length).toBe(2);
          component.addOrRemoveUser(false, userAlreadyInList);
          expect(userHashToArray(component.usersToAdd).length).toBe(1);
          expect(Object.keys(component.usersToAdd)).not.toContain(testUserToAddOrRemove.username);
        });
      });
    });
  });

  describe('userNotFiltered', () => {
    const testUserToFilter = <User>{
      id: faker.random.uuid(),
      name: 'Dean Venture',
      username: 'deanie'
    };

    describe('when usersToFilter is empty', () => {
      beforeEach(() => {
        component.mapOfUsersToFilter = {};
      });

      it('it returns true', () => {
        fixture.detectChanges();
        expect(component.userNotFiltered(testUserToFilter)).toBe(true);
      });
    });

    describe('when usersToFilter is not empty', () => {
      const kuzkoID = faker.random.uuid();

      beforeEach(() => {
        component.mapOfUsersToFilter = {};
        component.mapOfUsersToFilter[userAlreadyInList.id] = userAlreadyInList;
        component.mapOfUsersToFilter[kuzkoID] = {
          id: kuzkoID,
          name: 'Brock Samson',
          username: 'kuzko'
        };
      });

      describe('when user is in the list to filter', () => {
        it('it returns false', () => {
          fixture.detectChanges();
          expect(component.userNotFiltered(userAlreadyInList)).toBe(false);
        });
      });

      describe('when user is NOT in the list to filter', () => {
        let otherUser: User;
        beforeEach(() => {
          otherUser = {
            id: '119d5157-4198-478b-b75c-c2a77ac0ff11',
            name: 'Montag the Dog',
            username: 'taggerbot9000'
          };
        });

        it('it returns true', () => {
          fixture.detectChanges();
          expect(component.userNotFiltered(otherUser)).toBe(true);
        });
      });
    });
  });


  describe('noUsersAvailable', () => {
    let users: User[];
    const kuzkoID = faker.random.uuid();
    const kuzko = {
      id: kuzkoID,
      name: 'Brock Samson',
      username: 'kuzko'
    };

    describe('when both mapOfUsersToFilter and input users are empty', () => {
      beforeEach(() => {
        users = [];
        component.mapOfUsersToFilter = {};
      });

      it('it returns true', () => {
        component.users = users;
        expect(component.noUsersAvailable()).toBe(true);
      });
    });

    describe('when both mapOfUsersToFilter and input match exactly', () => {
      beforeEach(() => {
        component.mapOfUsersToFilter = {};
        component.mapOfUsersToFilter[userAlreadyInList.id] = userAlreadyInList;
        component.mapOfUsersToFilter[kuzkoID] = kuzko;
        users = [userAlreadyInList, kuzko];
      });

      it('it returns true', () => {
        component.users = users;
        expect(component.noUsersAvailable()).toBe(true);
      });
    });

    describe('when mapOfUsersToFilter is empty but input is not', () => {
      beforeEach(() => {
        component.mapOfUsersToFilter = {};
        users = [userAlreadyInList];
      });

      it('it returns false', () => {
        component.users = users;
        expect(component.noUsersAvailable()).toBe(false);
      });
    });

    describe('when mapOfUsersToFilter is not a superset of input', () => {
      beforeEach(() => {
        const otherUser = {
          id: faker.random.uuid(),
          name: 'Montag the Dog',
          username: 'taggerbot9000'
        };

        component.mapOfUsersToFilter = {};
        component.mapOfUsersToFilter[userAlreadyInList.id] = userAlreadyInList;
        component.mapOfUsersToFilter[kuzko.id] = kuzko;
        users = [userAlreadyInList, kuzko, otherUser];
      });

      it('it returns false', () => {
        component.users = users;
        expect(component.noUsersAvailable()).toBe(false);
      });
    });
  });
});
