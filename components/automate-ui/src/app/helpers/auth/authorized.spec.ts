import { waitForAsync, TestBed } from '@angular/core/testing';
import { StoreModule, Store } from '@ngrx/store';
import { concat } from 'lodash/fp';
import * as faker from 'faker';

import { NgrxStateAtom, runtimeChecks } from 'app/ngrx.reducers';
import { using } from 'app/testing/spec-helpers';
import { CheckObj, AuthorizedChecker } from 'app/helpers/auth/authorized';
import { IndexedEntities } from 'app/entities/entities';
import { Status } from 'app/entities/userperms/userperms.reducer';
import { GetSomeUserPerms, GetUserParamPerms } from 'app/entities/userperms/userperms.actions';
import { UserPermEntity } from 'app/entities/userperms/userperms.entity';

describe('AuthorizedComponent setPermissions', () => {

  let authorizedChecker: AuthorizedChecker;
  let dispatchSpy: jasmine.Spy;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      imports: [
        StoreModule.forRoot({
          userperms: () => ({})
        }, { runtimeChecks })
      ]
    }).compileComponents();
    const store: Store<NgrxStateAtom> = TestBed.inject(Store);
    authorizedChecker = new AuthorizedChecker(store);
    dispatchSpy = spyOn(store, 'dispatch');
  }));

  describe('non-parameterized endpoints', () => {

    using([
      ['one endpoint', [genCheckObj()]],
      ['multiple endpoints', [genCheckObj(), genCheckObj(), genCheckObj()]]
    ], function (description: string, epList: CheckObj[]) {
      it('maps allOf list for ' + description, () => {
        authorizedChecker.setPermissions(epList, []);

        const pathsOnly = epList.map(check => check.endpoint);
        expect(dispatchSpy).toHaveBeenCalledTimes(1);
        expect(dispatchSpy).toHaveBeenCalledWith(new GetSomeUserPerms({paths: pathsOnly}));
        // equivalent to last check, but easier for diagnosing issues:
        // const dispatchArg = <GetSomeUserPerms>dispatchSpy.calls.argsFor(0)[0];
        // expect(dispatchArg).not.toBeNull();
        // expect(dispatchArg.payload.paths).toEqual(pathsOnly);
      });
    });

    using([
      ['one endpoint', [genCheckObj()]],
      ['multiple endpoints', [genCheckObj(), genCheckObj()]]
    ], function (description: string, epList: CheckObj[]) {
      it('maps anyOf list for ' + description, () => {
        authorizedChecker.setPermissions([], epList);

        const pathsOnly = epList.map(check => check.endpoint);
        expect(dispatchSpy).toHaveBeenCalledTimes(1);
        expect(dispatchSpy).toHaveBeenCalledWith(new GetSomeUserPerms({paths: pathsOnly}));
      });
    });

    it('maps allOf AND anyOf lists', () => {
      const allOfList = [genCheckObj(), genCheckObj()];
      const anyOfList = [genCheckObj(), genCheckObj(), genCheckObj()];

      authorizedChecker.setPermissions(allOfList, anyOfList);

      const pathsOnly = concat(allOfList, anyOfList).map(check => check.endpoint);
      expect(dispatchSpy).toHaveBeenCalledTimes(1);
      expect(dispatchSpy).toHaveBeenCalledWith(new GetSomeUserPerms({paths: pathsOnly}));
    });

    it('maps allOf AND anyOf lists BUT skips parameterized paths', () => {
      const allOfList = [genCheckObj('some/ep1/{username}'), genCheckObj()];
      const anyOfList = [genCheckObj(), genCheckObj('some/ep4/{email}'), genCheckObj()];
      const parameterizedEntries = 2;
      const endpointsWithParams = /ep1|ep4/;

      authorizedChecker.setPermissions(allOfList, anyOfList);

      const pathsOnly = concat(allOfList, anyOfList)
        .filter(check => !endpointsWithParams.test(check.endpoint))
        .map(check => check.endpoint);
      expect(dispatchSpy).toHaveBeenCalledTimes(1 + parameterizedEntries);
      expect(dispatchSpy).toHaveBeenCalledWith(new GetSomeUserPerms({paths: pathsOnly}));
    });

  });

  describe('parameterized endpoints', () => {
    using([
      ['one endpoint', [genCheckParameterized('some/endpoint/{placeholder}', ['bob'])]],
      ['multiple endpoints',
        [genCheckParameterized('some/ep1/{placeholder}', ['bob']),
        genCheckParameterized('some/ep2/{placeholder}/runs', ['bob'])]
      ]
    ], function (description: string, epList: CheckObj[]) {
      it('maps allOf list for ' + description, () => {

        authorizedChecker.setPermissions(epList, []);
        checkParameterizedEndpoints(epList);
     });
    });

   using([
      ['one endpoint', [genCheckParameterized('some/endpoint/{placeholder}', ['bob'])]],
      ['multiple endpoints',
        [genCheckParameterized('some/ep1/{placeholder}', ['bob']),
        genCheckParameterized('some/ep2/{placeholder}/runs', ['bob'])]
      ]
    ], function (description: string, epList: CheckObj[]) {
      it('maps anyOf list for ' + description, () => {
        authorizedChecker.setPermissions([], epList);
        checkParameterizedEndpoints(epList);
      });
    });

    it('maps allOf AND anyOf lists', () => {
      const allOfList = <CheckObj[]>[
        genCheckParameterized('some/ep1/{placeholder}', ['bob']),
        genCheckParameterized('some/ep2/{placeholder}', ['bob'])];
      const anyOfList = <CheckObj[]>[
        genCheckParameterized('some/ep3/{placeholder}', ['bob']),
        genCheckParameterized('some/ep4/{placeholder}', ['bob']),
        genCheckParameterized('some/ep5/{placeholder}', ['bob'])];

      authorizedChecker.setPermissions(allOfList, anyOfList);
      checkParameterizedEndpoints(concat(allOfList, anyOfList));
    });

    it('maps allOf AND anyOf lists BUT skips non-parameterized paths', () => {
      const allOfList = [genCheckObj('some/ep1/{username}'), genCheckObj()];
      const anyOfList = [genCheckObj(), genCheckObj('some/ep4/{email}'), genCheckObj()];
      const endpointsWithParams = /ep1|ep4/;

      authorizedChecker.setPermissions(allOfList, anyOfList);

      checkParameterizedEndpoints(
        concat(allOfList, anyOfList)
          .filter((check: CheckObj) => endpointsWithParams.test(check.endpoint)),
        true
      );
    });

    using([
      ['with one placeholder at end',
        'some/ep/{username}', ['bob'], 'some/ep/bob'],
      ['with one placeholder in middle',
        'some/{username}/ep', ['bob'], 'some/bob/ep'],
      ['with two placeholders',
        'some/ep/{a}/run/{b}', ['one', 'two'], 'some/ep/one/run/two'],
      ['but one empty provided',
        'some/ep/{a}/run/{b}/{c}', ['one', '', 'two'], 'some/ep/one/run//two'
      ],
      ['but extra parameters provided',
        'some/ep/{a}/run/{b}', ['one', 'two', 'three'], 'some/ep/one/run/two'
      ],
      ['but not enough parameters provided',
        'some/ep/{a}/run/{b}', ['one'], 'some/ep/one/run/{b}'],
      // This last test case shows that param names are not considered
      // (i.e. it is also reasonable to expect to get 'some/ep/one/run/one').
      // That functionality is not needed at present, though.
      ['but ignores placeholder names',
        'some/ep/{a}/run/{a}', ['one', 'two'], 'some/ep/one/run/two'
      ]
    ], function (description: string, path: string, paramList: string[], expected: string) {
      it('fills in parameters ' + description, () => {

        authorizedChecker.setPermissions([genCheckParameterized(path, paramList)], []);

        expect(dispatchSpy).toHaveBeenCalledTimes(1);
        expect(dispatchSpy).toHaveBeenCalledWith(new GetUserParamPerms({
          path: expected,
          parameters: []
        }));
      });
    });

    using([
      ['in a list', [genCheckParameterized('some/endpoint/{target}', ['bob'])]],
      ['NOT in a list', [genCheckParameterized('some/endpoint/{target}', 'bob')]]
    ], function (description: string, epList: CheckObj[]) {
        it('accepts a single parameter ' + description, () => {
          authorizedChecker.setPermissions(epList, []);

          expect(dispatchSpy).toHaveBeenCalledTimes(1);
          expect(dispatchSpy).toHaveBeenCalledWith(new GetUserParamPerms({
            path: 'some/endpoint/bob',
            parameters: []
          }));
      });
    });
  });

  function genCheckObj(path?: string, method?: string, paramList?: string | string[]): CheckObj {
    const methods = ['get', 'put', 'post', 'delete', 'patch'];
    return <CheckObj>{
      endpoint: path || `/${faker.lorem.word()}/${faker.lorem.word()}/${faker.lorem.word()}`,
      verb: method || faker.random.arrayElement(methods),
      paramList: paramList || []
    };
  }

  function genCheckParameterized(path: string, paramList?: string | string[]): CheckObj {
    return genCheckObj(path, null, paramList);
  }

  function checkParameterizedEndpoints(epList: CheckObj[], addOneForNonParameterized = false) {
    expect(dispatchSpy).toHaveBeenCalledTimes(epList.length + (addOneForNonParameterized ? 1 : 0));
    epList.forEach(
      check =>
        expect(dispatchSpy).toHaveBeenCalledWith(new GetUserParamPerms({
          path: check.endpoint.replace('{placeholder}', check.paramList[0]),
          parameters: []
        }))
    );
  }

});

describe('AuthorizedComponent real round trip', () => {

  let authorizedChecker: AuthorizedChecker;
  let visible: boolean;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      imports: [
        StoreModule.forRoot({
          userperms: () => ({
            status: Status.loadingSuccess,
            byId: <IndexedEntities<UserPermEntity>>{
              '/apis/iam/v2/projects/project-1/rules': {
                id: '/apis/iam/v2/projects/project-1/rules',
                get: false, put: false, post: true, delete: false, patch: false
              },
               '/apis/iam/v2/projects/project-2/rules': {
                id: '/apis/iam/v2/projects/project-2/rules',
                get: false, put: false, post: false, delete: false, patch: false
              },
              '/apis/iam/v2/users': {
                id: '/apis/iam/v2/users',
                get: false, put: false, post: false, delete: false, patch: false
              },
              '/apis/iam/v2/teams': {
                id: '/apis/iam/v2/teams',
                get: true, put: false, post: false, delete: false, patch: false
              }
            }
          })

        }, { runtimeChecks })
      ]
    })
    .compileComponents();
    const store: Store<NgrxStateAtom> = TestBed.inject(Store);
    authorizedChecker = new AuthorizedChecker(store);
    visible = false;
  }));

  it('reflects changes from successive setPermission calls', done => {

    expect(visible).toBe(false);
    authorizedChecker.isAuthorized$.subscribe((isAuthorized: boolean) => {
      visible = isAuthorized;
    });

    authorizedChecker.setPermissions(
      [<CheckObj>{ endpoint: '/apis/iam/v2/teams', verb: 'get' }], []);
    setTimeout(() => {
      expect(visible).toBe(true);

      authorizedChecker.setPermissions(
        [<CheckObj>{ endpoint: '/apis/iam/v2/users', verb: 'get' }], []);
      setTimeout(() => {
        expect(visible).toBe(false);

        authorizedChecker.setPermissions([<CheckObj>{
          endpoint: '/apis/iam/v2/teams', verb: 'get' }], []);
        setTimeout(() => {
          expect(visible).toBe(true);

          done();
        }, AuthorizedChecker.DebounceTime + 1);
      }, AuthorizedChecker.DebounceTime + 1);
    }, AuthorizedChecker.DebounceTime + 1);
  });

  it('reflects changes from successive setPermission calls for parameterized endpoint', done => {
    expect(visible).toBe(false);
    authorizedChecker.isAuthorized$.subscribe((isAuthorized: boolean) => {
      visible = isAuthorized;
    });

    const endpoint = '/apis/iam/v2/projects/{project_id}/rules';
    const verb = 'post';

    authorizedChecker.setPermissions(
      [<CheckObj>{ endpoint, verb, paramList: ['project-1'] }], []);
    setTimeout(() => {
      expect(visible).toBe(true);

      authorizedChecker.setPermissions(
        [<CheckObj>{ endpoint, verb, paramList: ['project-2'] }], []);
      setTimeout(() => {
        expect(visible).toBe(false);

        done();
      });
    });
  });

});

describe('AuthorizedComponent evalPerms', () => {
  let ac: AuthorizedChecker;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      imports: [
        StoreModule.forRoot({
          userperms: () => ({})
        }, { runtimeChecks })
      ]
    }).compileComponents();
    const store: Store<NgrxStateAtom> = TestBed.inject(Store);
    ac = new AuthorizedChecker(store);
  }));

  describe('allOf/anyOf', () => {
    let perms: IndexedEntities<UserPermEntity>;

    beforeAll(() => {
      perms = {
          '/apis/iam/v2/users': {
            id: '/apis/iam/v2/users',
            get: true,
            put: false,
            post: false,
            delete: false,
            patch: false
          },
          '/apis/iam/v2/teams': {
            id: '/apis/iam/v2/teams',
            get: true,
            put: false,
            post: false,
            delete: false,
            patch: false
          },
          '/apis/iam/v2/users/alice': {
            id: '/apis/iam/v2/users/alice',
            get: true,
            put: false,
            post: false,
            delete: false,
            patch: false
          }
        };
    });

    using([
      { descr: 'empty allOf and anyOf evaluates to true', expected: true },
      { descr: 'requires all of allOf to be true, and returns true',
        allOf: [{ endpoint: '/apis/iam/v2/users', verb: 'get', paramList: [] },
                { endpoint: '/apis/iam/v2/teams', verb: 'get', paramList: [] }],
        expected: true },
      { descr: 'requires all of allOf to be true, and otherwise returns false',
        allOf: [{ endpoint: '/apis/iam/v2/users', verb: 'get', paramList: [] },
                { endpoint: '/apis/iam/v2/teams', verb: 'put', paramList: [] }]},
      { descr: 'requires any of anyOf to be true',
        anyOf: [{ endpoint: '/apis/iam/v2/users', verb: 'get', paramList: [] },
                { endpoint: '/apis/iam/v2/teams', verb: 'put', paramList: [] }],
        expected: true },
      { descr: 'fills parameters in allOf, when authorized, returns true',
        allOf: [{ endpoint: '/apis/iam/v2/users/{id}', verb: 'get', paramList: 'alice' }],
        expected: true },
      { descr: 'fills parameters in allOf (from array), when authorized, returns true',
        allOf: [{ endpoint: '/apis/iam/v2/users/{id}', verb: 'get', paramList: ['alice'] }],
        expected: true },
      { descr: 'fills parameters in allOf, when not authorized, returns false',
        allOf: [{ endpoint: '/apis/iam/v2/users/{id}', verb: 'get', paramList: 'bob' }]},
      { descr: 'fills parameters in anyOf, when authorized, returns true',
        anyOf: [{ endpoint: '/apis/iam/v2/users/{id}', verb: 'get', paramList: 'alice' }],
        expected: true },
      { descr: 'fills parameters in anyOf (from array), when authorized, returns true',
        anyOf: [{ endpoint: '/apis/iam/v2/users/{id}', verb: 'get', paramList: ['alice'] }],
        expected: true },
      { descr: 'fills parameters in anyOf, when not authorized, returns false',
        anyOf: [{ endpoint: '/apis/iam/v2/users/{id}', verb: 'get', paramList: 'bob' }]}
    ],
    ({descr, allOf, anyOf, expected}:
      { descr: string; allOf?, anyOf?: CheckObj[]; expected?: boolean}) => {
      it(descr, () => {
        ac.setPermissions(allOf || [], anyOf || []);
        expect(ac.evalPerms(perms)).toEqual(expected || false);
      });
    });
  });
});
