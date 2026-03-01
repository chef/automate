import { HTTP_INTERCEPTORS, HttpClient } from '@angular/common/http';
import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { StoreModule } from '@ngrx/store';

import { ChefSessionService } from '../chef-session/chef-session.service';
import { runtimeChecks } from 'app/ngrx.reducers';
import {
  ProjectsFilterState, ProjectsFilterOption
} from '../projects-filter/projects-filter.reducer';
import { HttpClientAuthInterceptor } from './http-client-auth.interceptor';
import { using } from 'app/testing/spec-helpers';
import { ReplaySubject } from 'rxjs';

describe('HttpClientAuthInterceptor', () => {
  let httpClient: HttpClient;
  let httpMock: HttpTestingController;
  let chefSession: ChefSessionService;

  describe('common functionality', () => {
    beforeEach(() => {
      configure();
      httpClient = TestBed.inject(HttpClient);
      httpMock = TestBed.inject(HttpTestingController);
      chefSession = TestBed.inject(ChefSessionService);

      spyOnProperty(chefSession, 'id_token', 'get').and.returnValue('token');
      const tokenProvider = new ReplaySubject<string>(1);
      tokenProvider.next(chefSession.id_token);
      spyOnProperty(chefSession, 'token_provider', 'get').and.returnValue(tokenProvider);
    });

    it('includes auth token in all requests', done => {
      httpClient.get('/endpoint').subscribe(() => {
        try {
          expect(httpRequest.request.headers.get('Authorization'))
            .toEqual(`Bearer ${chefSession.id_token}`);
          done();
        } catch (error) {
          done.fail(error instanceof Error ? error : new Error(String(error)));
        }
      });

      const httpRequest = httpMock.expectOne('/endpoint');
      httpRequest.flush('response');
    });

    it('sets the lax API header all requests', done => {
      httpClient.get('/endpoint').subscribe(() => {
        try {
          expect(httpRequest.request.headers.get('Content-Type'))
            .toEqual('application/json+lax');
          done();
        } catch (error) {
          done.fail(error instanceof Error ? error : new Error(String(error)));
        }
      });

      const httpRequest = httpMock.expectOne('/endpoint');
      httpRequest.flush('response');
    });

    it('when a 401 response is intercepted logs out the session', done => {
      spyOn(chefSession, 'logout');
      httpClient.get('/endpoint').subscribe({
        next: () => done.fail('Should not succeed'),
        error: (error) => {
          try {
            expect(error.status).toBe(401);
            expect(chefSession.logout).toHaveBeenCalledWith();
            done();
          } catch (assertionError) {
            done.fail(assertionError instanceof Error ? assertionError : new Error(String(assertionError)));
          }
        }
      });

      const httpRequest = httpMock.expectOne('/endpoint');
      // Note 2019/06/27 (sr): When using HTTP/2, statusText will always be "OK"
      // so our logic shouldn't depend on it.
      httpRequest.flush('response', { status: 401, statusText: 'OK' });
    });
  });

  describe('project header', () => {

    using([
      ['with some projects and ALL checked',
        [
          { value: 'proj1', label: 'proj 1', type: 'CUSTOM', checked: true },
          { value: 'proj2', label: 'proj 2', type: 'CUSTOM', checked: true },
          { value: 'proj3', label: 'proj 3', type: 'CUSTOM', checked: true }
        ]],
      ['with some projects and SOME checked',
        [
          { value: 'proj1', label: 'proj 1', type: 'CUSTOM', checked: true },
          { value: 'proj2', label: 'proj 2', type: 'CUSTOM', checked: false },
          { value: 'proj3', label: 'proj 3', type: 'CUSTOM', checked: true },
          { value: 'proj4', label: 'proj 4', type: 'CUSTOM', checked: false }
        ]],
      ['with some projects and ONE checked',
        [
          { value: 'proj1', label: 'proj 1', type: 'CUSTOM', checked: false },
          { value: 'proj2', label: 'proj 2', type: 'CUSTOM', checked: false },
          { value: 'proj3', label: 'proj 3', type: 'CUSTOM', checked: true }
        ]]
    ], function (description: string, projectList: ProjectsFilterOption[]) {

      describe(description, () => {
        beforeEach(() => {
          configure(projectList);
          httpClient = TestBed.inject(HttpClient);
          httpMock = TestBed.inject(HttpTestingController);
          chefSession = TestBed.inject(ChefSessionService);
          spyOnProperty(chefSession, 'id_token', 'get').and.returnValue('token');
          const tokenProvider = new ReplaySubject<string>(1);
          tokenProvider.next(chefSession.id_token);
          spyOnProperty(chefSession, 'token_provider', 'get').and.returnValue(tokenProvider);
        });

        it('includes checked projects', done => {
          httpClient.get('/endpoint').subscribe(() => {
            try {
              expect(httpRequest.request.headers.keys()).toContain('projects');
              expect(httpRequest.request.headers.get('projects'))
                .toEqual(projectList.filter(p => p.checked).map(p => p.value).join(', '));
              done();
            } catch (error) {
              done.fail(error instanceof Error ? error : new Error(String(error)));
            }
          });

          const httpRequest = httpMock.expectOne('/endpoint');
          httpRequest.flush('response');
        });
      });
    });

    using([
      ['with some projects and NONE checked',
        [
          { value: 'proj1', label: 'proj 1', type: 'CUSTOM', checked: false },
          { value: 'proj2', label: 'proj 2', type: 'CUSTOM', checked: false },
          { value: 'proj3', label: 'proj 3', type: 'CUSTOM', checked: false }
        ]],
      ['with NO projects',
        [
        ]]
    ], function (description: string, projectList: ProjectsFilterOption[]) {

      describe(description, () => {

        beforeEach(() => {
          configure(projectList);
          httpClient = TestBed.inject(HttpClient);
          httpMock = TestBed.inject(HttpTestingController);
          chefSession = TestBed.inject(ChefSessionService);
          spyOnProperty(chefSession, 'id_token', 'get').and.returnValue('token');
          const tokenProvider = new ReplaySubject<string>(1);
          tokenProvider.next(chefSession.id_token);
          spyOnProperty(chefSession, 'token_provider', 'get').and.returnValue(tokenProvider);
        });

        it('does not include projects header', done => {
          httpClient.get('/endpoint').subscribe(() => {
            try {
              expect(httpRequest.request.headers.keys()).not.toContain('projects');
              done();
            } catch (error) {
              done.fail(error instanceof Error ? error : new Error(String(error)));
            }
          });

          const httpRequest = httpMock.expectOne('/endpoint');
          httpRequest.flush('response');
        });
      });
    });

    describe('unfiltered flag', () => {
      beforeEach(() => {
        const projectList = [
          { value: 'proj1', label: 'proj 1', type: 'CUSTOM', checked: true },
          { value: 'proj2', label: 'proj 2', type: 'CUSTOM', checked: false },
          { value: 'proj3', label: 'proj 3', type: 'CUSTOM', checked: true },
          { value: 'proj4', label: 'proj 4', type: 'CUSTOM', checked: false }
        ];
        configure(projectList);
        httpClient = TestBed.inject(HttpClient);
        httpMock = TestBed.inject(HttpTestingController);
        chefSession = TestBed.inject(ChefSessionService);
        spyOnProperty(chefSession, 'id_token', 'get').and.returnValue('token');
        const tokenProvider = new ReplaySubject<string>(1);
        tokenProvider.next(chefSession.id_token);
        spyOnProperty(chefSession, 'token_provider', 'get').and.returnValue(tokenProvider);
      });

      using([
        ['does not include project header', true],
        ['includes project header', false]
      ], function (description: string, setting: boolean) {

        it(description + 'project header when unfiltered flag set to ' + setting, done => {
          const options = { params: { unfiltered: String(setting) } };
          httpClient.get('/endpoint', options).subscribe(() => {
            try {
              const headerKeys = httpRequest.request.headers.keys();
              if (setting) {
                expect(headerKeys).not.toContain('projects');
              } else {
                expect(headerKeys).toContain('projects');
              }
              done();
            } catch (error) {
              done.fail(error instanceof Error ? error : new Error(String(error)));
            }
          });

          const httpRequest = httpMock.expectOne('/endpoint');
          httpRequest.flush('response');
        });

        it('strips unfiltered param when set to ' + setting, done => {
          const options = { params: { unfiltered: String(setting), dummy: 'foobar' } };
          httpClient.get('/endpoint', options).subscribe(() => {
            try {
              expect(httpRequest.request.params.get('unfiltered')).toBeNull();
              expect(httpRequest.request.params.get('dummy')).toEqual('foobar');
              done();
            } catch (error) {
              done.fail(error instanceof Error ? error : new Error(String(error)));
            }
          });

          const httpRequest = httpMock.expectOne('/endpoint?dummy=foobar');
          httpRequest.flush('response');
        });
      });
    });
  });
});

function configure(projects?: ProjectsFilterOption[]): void {
  TestBed.configureTestingModule({
    imports: [
      HttpClientTestingModule,
      StoreModule.forRoot({
        projectsFilter: () => <ProjectsFilterState>{
          options: projects || []
        }
      }, { runtimeChecks })
    ],
    providers: [
      ChefSessionService,
      {
        provide: HTTP_INTERCEPTORS,
        useClass: HttpClientAuthInterceptor,
        multi: true
      }
    ]
  });
}
