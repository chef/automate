import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { StoreModule, Store } from '@ngrx/store';
import * as faker from 'faker';

import { NgrxStateAtom, runtimeChecks } from 'app/ngrx.reducers';
import { using } from 'app/testing/spec-helpers';
import { IndexedEntities } from 'app/entities/entities';
import { UserPermEntity } from 'app/entities/userperms/userperms.entity';
import { Status } from 'app/entities/userperms/userperms.reducer';
import { GetSomeUserPerms, GetUserParamPerms } from 'app/entities/userperms/userperms.actions';
import { AuthorizedChecker } from 'app/helpers/auth/authorized';
import { AuthorizedComponent, Check } from './authorized.component';

describe('AuthorizedComponent', () => {
  let component: AuthorizedComponent;
  let fixture: ComponentFixture<AuthorizedComponent>;
  let dispatchSpy: jasmine.Spy;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      imports: [
        StoreModule.forRoot({
          userperms: () => ({
            status: Status.loadingSuccess,
            byId: <IndexedEntities<UserPermEntity>>{
              'authed/endpoint': {
                id: 'authed/endpoint',
                get: true,
                put: true,
                post: true,
                delete: true,
                patch: true
              },
              'another/authed/endpoint': {
                id: 'another/authed/endpoint',
                get: true,
                put: true,
                post: true,
                delete: true,
                patch: true
              },
              'notauthed/endpoint': {
                id: 'notauthed/endpoint',
                get: false,
                put: false,
                post: false,
                delete: false,
                patch: false
              },
              'another/notauthed/endpoint': {
                id: 'another/notauthed/endpoint',
                get: false,
                put: false,
                post: false,
                delete: false,
                patch: false
              }
            }
          })
        }, { runtimeChecks })
      ],
      declarations: [
        AuthorizedComponent
      ]
    }).compileComponents();
    const store: Store<NgrxStateAtom> = TestBed.inject(Store);
    dispatchSpy = spyOn(store, 'dispatch').and.callThrough();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(AuthorizedComponent);
    component = fixture.componentInstance;
  });

  it('exists', () => {
    fixture.detectChanges();
    expect(component).toBeTruthy();
  });

  using([
    ['disallowed endpoint', genCheck('notauthed/endpoint'), false],
    ['allowed endpoint', genCheck('authed/endpoint'), true]
  ], function (description: string, singleEp: Check, expected: boolean) {
    it(('supports single checks to be passed without nesting for ' + description), done => {
      component.allOf = singleEp;
      fixture.detectChanges();
      checkAfterDebounce(expected, done);
    });
  });

  using([
    ['disallowed endpoint with all uppercase', [genCheck('notauthed/endpoint', 'POST')], false],
    ['disallowed endpoint with mixed case', [genCheck('notauthed/endpoint', 'GeT')], false],
    ['disallowed endpoint with all lowercase', [genCheck('notauthed/endpoint', 'patch')], false],
    ['allowed endpoint with all uppercase', [genCheck('authed/endpoint', 'POST')], true],
    ['allowed endpoint with mixed case', [genCheck('authed/endpoint', 'GeT')], true],
    ['allowed endpoint with all lowercase', [genCheck('authed/endpoint', 'patch')], true]
  ], function (description: string, epList: Check[], expected: boolean) {
    it('supports ' + description, done => {
      component.allOf = epList;
      fixture.detectChanges();
      checkAfterDebounce(expected, done);
    });
  });

  describe('visibility setting', () => {

    it('is visible when allOf are all valid', done => {
      component.allOf = [
        genCheck('authed/endpoint'),
        genCheck('another/authed/endpoint')];
      fixture.detectChanges();
      const expected = true;
      checkAfterDebounce(expected, done);
    });

    it('is NOT visible when allOf are not all valid', done => {
      component.allOf = [
        genCheck('authed/endpoint'),
        genCheck('notauthed/endpoint')];
      fixture.detectChanges();
      const expected = false;
      checkAfterDebounce(expected, done);
    });

    it('is visible when allOf is an empty list', done => {
      component.allOf = [];
      component.anyOf = [ genCheck('another/authed/endpoint') ];
      fixture.detectChanges();
      const expected = true;
      checkAfterDebounce(expected, done);
    });

    it('is visible when anyOf are all valid', done => {
      component.anyOf = [
        genCheck('authed/endpoint'),
        genCheck('another/authed/endpoint')];
      fixture.detectChanges();
      const expected = true;
      checkAfterDebounce(expected, done);
    });

    it('is visible when anyOf are NOT all valid', done => {
      component.anyOf = [
        genCheck('authed/endpoint'),
        genCheck('notauthed/endpoint')];
      fixture.detectChanges();
      const expected = true;
      checkAfterDebounce(expected, done);
    });

    it('is NOT visible when anyOf are all invalid', done => {
      component.anyOf = [
        genCheck('notauthed/endpoint'),
        genCheck('another/notauthed/endpoint')];
      fixture.detectChanges();
      const expected = false;
      checkAfterDebounce(expected, done);
    });

    it('is visible when anyOf is an empty list', done => {
      component.anyOf = [];
      component.allOf = [ genCheck('another/authed/endpoint') ];
      fixture.detectChanges();
      const expected = true;
      checkAfterDebounce(expected, done);
    });

    it('is visible when neither anyOf or allOf are specified', done => {
      fixture.detectChanges();
      const expected = true;
      checkAfterDebounce(expected, done);
    });

  });

  describe('non-parameterized endpoints', () => {

    using([
      ['some allOf and no anyOf',
        [genCheck(), genCheck()],
        []
      ],
      ['no allOf and some anyOf',
        [],
        [genCheck(), genCheck(), genCheck()]
      ],
      ['some allOf and some anyOf',
        [genCheck(), genCheck()],
        [genCheck(), genCheck(), genCheck()]
      ]
    ], function (description, allOfList: Check[], anyOfList: Check[]) {
      it('dispatches just one action when passing ' + description, () => {
        component.allOf = allOfList;
        component.anyOf = anyOfList;

        fixture.detectChanges();

        expect(dispatchSpy).toHaveBeenCalledTimes(1);
        const args = dispatchSpy.calls.argsFor(0);
        expect(args[0] instanceof GetSomeUserPerms).toBeTruthy();
      });
    });

    it('does not dispatch action when passing no endpoints', () => {
      component.allOf = [];
      component.anyOf = [];

      fixture.detectChanges();

      expect(dispatchSpy).not.toHaveBeenCalled();
    });
  });

  describe('parameterized endpoints', () => {

    using([
      ['some allOf and no anyOf',
        [genCheckParameterized('some/ep1/{a}'), genCheckParameterized('some/ep2/{a}')],
        []
      ],
      ['no allOf and some anyOf',
        [],
        [genCheckParameterized('some/ep3/{a}'),
        genCheckParameterized('some/ep4/{a}'),
        genCheckParameterized('some/ep5/{a}')]
      ],
      ['some allOf and some anyOf',
        [genCheckParameterized('some/ep1/{a}'), genCheckParameterized('some/ep2/{a}')],
        [genCheckParameterized('some/ep3/{a}'),
        genCheckParameterized('some/ep4/{a}'),
        genCheckParameterized('some/ep5/{a}')]
      ]
    ], function (description, allOfList: Check[], anyOfList: Check[]) {
      it('dispatches one action for each parameterized endpoint when passing ' + description,
      () => {
        component.allOf = allOfList;
        component.anyOf = anyOfList;

        fixture.detectChanges();

        expect(dispatchSpy).toHaveBeenCalledTimes(allOfList.length + anyOfList.length);
        for (let i = 0; i < dispatchSpy.calls.count(); i++) {
          const args = dispatchSpy.calls.argsFor(i);
          expect(args[0] instanceof GetUserParamPerms).toBeTruthy();
        }
      });
    });

    it('does not dispatch action when passing no endpoints', () => {
      component.allOf = [];
      component.anyOf = [];

      fixture.detectChanges();

      expect(dispatchSpy).not.toHaveBeenCalled();
     });

  });

  it('dispatches BOTH parameterized AND non-parameterized actions with mixed endpoints',
    () => {
      component.allOf = [genCheckParameterized('some/ep1/{username}'), genCheck()];
      component.anyOf = [genCheck(), genCheckParameterized('some/ep4/{email}'), genCheck()];
      // NB: for simplicity in testing, leave the param lists empty above
      // so we can match the literal placeholders in the `toContain` predicate below.
      const parameterizedEntries = 2;

      fixture.detectChanges();

      expect(dispatchSpy).toHaveBeenCalledTimes(parameterizedEntries + 1);

      let args = dispatchSpy.calls.argsFor(0); // first call is all non-parameterized eps
      expect(args[0] instanceof GetSomeUserPerms).toBeTruthy();

      for (let i = 0; i < parameterizedEntries; i++) {
        args = dispatchSpy.calls.argsFor(i + 1); // parameterized eps follow, one for each
        expect(args[0] instanceof GetUserParamPerms).toBeTruthy();
      }
  });

  function genCheck(path?: string, method?: string, paramList?: string | string[]): Check {
    const methods = ['get', 'put', 'post', 'delete', 'patch'];
    return [
      path || `/${faker.lorem.word()}/${faker.lorem.word()}/${faker.lorem.word()}`,
      method || faker.random.arrayElement(methods),
      paramList || []
    ];
  }

  function genCheckParameterized(path: string, paramList?: string | string[]): Check {
    return genCheck(path, null, paramList);
  }

  function checkAfterDebounce(expected: boolean, done: any) {
      // simple way to account for debounce()
      setTimeout(() => {
        expect(component.visible).toBe(expected);
        done();
      }, AuthorizedChecker.DebounceTime + 1);
  }
});
