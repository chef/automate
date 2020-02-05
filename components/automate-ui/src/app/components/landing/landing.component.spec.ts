import { Router } from '@angular/router';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
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
      allOfCheck: [['/iam/v2/teams', 'get', '']],
      anyOfCheck: [['/auth/something-else', 'get', '']],
      route: '/settings/teams'
    },
    { allOfCheck: [['/auth/users', 'get', '']], route: '/settings/users' },
    {
      anyOfCheck: [['/auth/tokens', 'get', ''], ['/iam/v2/tokens', 'get', '']],
      route: '/settings/tokens'
    }
  ];


  describe('with first page allowed and all others denied', () => {

    router.navigate.calls.reset();

    beforeEach(async(() => {
      configureWith({
        '/auth/users': genPerm('/auth/users', false),
        '/iam/v2/teams': genPerm('/iam/v2/teams', true),
        '/auth/tokens': genPerm('/auth/tokens', false),
        '/auth/something-else': genPerm('/auth/something-else', true)
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

    beforeEach(async(() => {
      configureWith({
        '/auth/users': genPerm('/auth/users', true),
        '/iam/v2/teams': genPerm('/iam/v2/teams', true),
        '/auth/tokens': genPerm('/auth/tokens', true),
        '/auth/something-else': genPerm('/auth/something-else', true)
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
      { allOfCheck: [['/iam/v2/teams', 'get', '']], route: '/settings/teams' },
      { allOfCheck: [['/auth/users', 'get', '']], route: '/settings/users' },
      { anyOfCheck: [['/auth/tokens', 'get', '']], route: '/settings/tokens' }
    ];

    beforeEach(async(() => {
      configureWith({
        '/auth/users': genPerm('/auth/users', true),
        '/iam/v2/teams': genPerm('/iam/v2/teams', false),
        '/auth/tokens': genPerm('/auth/tokens', false),
        '/auth/something-else': genPerm('/auth/something-else', true)
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
