import { Router } from '@angular/router';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { StoreModule } from '@ngrx/store';

import { runtimeChecks } from 'app/ngrx.reducers';
import { IndexedEntities } from 'app/entities/entities';
import { UserPermEntity } from 'app/entities/userperms/userperms.entity';
import { AuthorizedChecker } from 'app/helpers/auth/authorized';
import { PermEntityState, Status } from 'app/entities/userperms/userperms.reducer';
import { LandingComponent, RoutePerms } from './landing.component';

describe('LandingComponent', () => {
  let component: LandingComponent;
  let fixture: ComponentFixture<LandingComponent>;
  const router = {
    navigate: jasmine.createSpy('navigate')
  };

  const targetRoute = '/settings/teams';
  const routePerms: RoutePerms[] = [
    {
      allOfCheck: [['/apis/iam/v2/teams', 'get']],
      anyOfCheck: [['/apis/iam/v2/something-else', 'get']],
      route: '/settings/teams'
    },
    { anyOfCheck: [['/apis/iam/v2/tokens', 'get']], route: '/settings/tokens' },
    { allOfCheck: [['/apis/iam/v2/users', 'get']], route: '/settings/users' }
  ];


  describe('with first page allowed and all others denied', () => {

    router.navigate.calls.reset();

    beforeEach(waitForAsync(() => {
      configureWith({
        '/apis/iam/v2/users': genPerm('/apis/iam/v2/users', false),
        '/apis/iam/v2/teams': genPerm('/apis/iam/v2/teams', true),
        '/apis/iam/v2/tokens': genPerm('/apis/iam/v2/tokens', false),
        '/apis/iam/v2/something-else': genPerm('/apis/iam/v2/something-else', true)
      });

    }));

    it('should create', () => {
      fixture = TestBed.createComponent(LandingComponent);
      component = fixture.componentInstance;
      expect(component).toBeTruthy();
    });

    it('routes to first page', done => {
      fixture = TestBed.createComponent(LandingComponent);
      component = fixture.componentInstance;
      component.routePerms = routePerms;
      fixture.detectChanges();

      setTimeout(() => {
        expect(router.navigate)
          .toHaveBeenCalledWith(component.routeToSegmentList(targetRoute), { replaceUrl: true });
        done();
      }, AuthorizedChecker.DebounceTime + 1);
    });

  });

  describe('with first page allowed and all others also allowed', () => {
    const targetIndex = 0;
    router.navigate.calls.reset();

    beforeEach(waitForAsync(() => {
      configureWith({
        '/apis/iam/v2/users': genPerm('/apis/iam/v2/users', true),
        '/apis/iam/v2/teams': genPerm('/apis/iam/v2/teams', true),
        '/apis/iam/v2/tokens': genPerm('/apis/iam/v2/tokens', true),
        '/apis/iam/v2/something-else': genPerm('/apis/iam/v2/something-else', true)
      });
    }));

    it('routes to first page', done => {
      fixture = TestBed.createComponent(LandingComponent);
      component = fixture.componentInstance;
      component.routePerms = routePerms;
      spyOn(component.authorizedChecker, 'setPermissions').and.callThrough();
      fixture.detectChanges();

      setTimeout(() => {
        expect(router.navigate)
          .toHaveBeenCalledWith(component.routeToSegmentList(targetRoute), { replaceUrl: true });
        expect(component.authorizedChecker.setPermissions).toHaveBeenCalledTimes(1);
        expect(component.authorizedChecker.setPermissions)
          .toHaveBeenCalledWith(
            component.getAllOfChecks(targetIndex),
            component.getAnyOfChecks(targetIndex));
        done();
      }, AuthorizedChecker.DebounceTime + 1);

    });
  });

  // Cannot test beyond second page with unit tests.
  // (Would require userperms.requests to return data from the backend to the reducer
  // so the observable watching the ngrx store would emit values.)
  describe('with second page allowed and all others denied', () => {
    const targetIndex = 1;
    const newRoutePerms: RoutePerms[] = [
      { allOfCheck: [['/apis/iam/v2/teams', 'get']], route: '/settings/teams' },
      { anyOfCheck: [['/apis/iam/v2/tokens', 'get']], route: '/settings/tokens' },
      { allOfCheck: [['/apis/iam/v2/users', 'get']], route: '/settings/users' }
    ];

    beforeEach(waitForAsync(() => {
      configureWith({
        '/apis/iam/v2/users': genPerm('/apis/iam/v2/users', true),
        '/apis/iam/v2/teams': genPerm('/apis/iam/v2/teams', false),
        '/apis/iam/v2/tokens': genPerm('/apis/iam/v2/tokens', false),
        '/apis/iam/v2/something-else': genPerm('/apis/iam/v2/something-else', true)
      });
    }));

    beforeEach(() => {
      fixture = TestBed.createComponent(LandingComponent);
      component = fixture.componentInstance;
      component.routePerms = newRoutePerms;
      spyOn(component.authorizedChecker, 'setPermissions').and.callThrough();
      fixture.detectChanges();
    });

    it('routes to second page', done => {
      setTimeout(() => {
        expect(component.authorizedChecker.setPermissions).toHaveBeenCalledTimes(2);
        expect(component.authorizedChecker.setPermissions)
          .toHaveBeenCalledWith(
            component.getAllOfChecks(targetIndex),
            component.getAnyOfChecks(targetIndex));
        done();
      }, 10);
    });
  });


  function configureWith(perms: IndexedEntities<UserPermEntity>): void {
    TestBed.configureTestingModule({
      imports: [
        StoreModule.forRoot({
          userperms: () => <PermEntityState>{
            status: Status.loadingSuccess,
            byId: perms
          }
        }, { runtimeChecks })
      ],
      providers: [
        { provide: Router, useValue: router }
      ],
      declarations: [
        LandingComponent
      ]
    })
      .compileComponents();
  }

  function genPerm(name: string, state: boolean): UserPermEntity {
    return new UserPermEntity(
      { get: state, put: state, post: state, delete: state, patch: state }, name);
  }
});
