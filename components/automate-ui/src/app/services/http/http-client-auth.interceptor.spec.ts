import { HTTP_INTERCEPTORS, HttpClient } from '@angular/common/http';
import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { StoreModule } from '@ngrx/store';

import { ChefSessionService } from '../chef-session/chef-session.service';
import {
  ProjectsFilterState, ProjectsFilterOption
} from '../projects-filter/projects-filter.reducer';
import { HttpClientAuthInterceptor } from './http-client-auth.interceptor';
import { using } from 'app/testing/spec-helpers';

describe('HttpClientAuthInterceptor', () => {
  let httpClient: HttpClient;
  let httpMock: HttpTestingController;
  let chefSession: ChefSessionService;

  describe('common functionality', () => {
    beforeEach(() => {
      configure();
      httpClient = TestBed.get(HttpClient);
      httpMock = TestBed.get(HttpTestingController);
      chefSession = TestBed.get(ChefSessionService);
      spyOnProperty(chefSession, 'id_token', 'get').and.returnValue('token');
    });

    it('includes auth token in all requests', done => {
      httpClient.get('/endpoint').subscribe(done);

      const httpRequest = httpMock.expectOne('/endpoint');
      httpRequest.flush('response');

      expect(httpRequest.request.headers.get('Authorization'))
        .toEqual(`Bearer ${chefSession.id_token}`);
    });

    it('when a 401 response is intercepted logs out the session', done => {
      spyOn(chefSession, 'logout');
      httpClient.get('/endpoint').subscribe(null, done);

      const httpRequest = httpMock.expectOne('/endpoint');
      httpRequest.flush('response', { status: 401, statusText: 'Not Authorized' });

      expect(chefSession.logout).toHaveBeenCalled();
    });
  });

  describe('project header', () => {

    using([
      ['with some projects and ALL checked',
        [
          { value: 'proj1', label: 'proj 1', checked: true },
          { value: 'proj2', label: 'proj 2', checked: true },
          { value: 'proj3', label: 'proj 3', checked: true }
        ]],
      ['with some projects and SOME checked',
        [
          { value: 'proj1', label: 'proj 1', checked: true },
          { value: 'proj2', label: 'proj 2', checked: false },
          { value: 'proj3', label: 'proj 3', checked: true },
          { value: 'proj4', label: 'proj 4', checked: false }
        ]],
      ['with some projects and ONE checked',
        [
          { value: 'proj1', label: 'proj 1', checked: false },
          { value: 'proj2', label: 'proj 2', checked: false },
          { value: 'proj3', label: 'proj 3', checked: true }
        ]]
    ], function (description: string, projectList: ProjectsFilterOption[]) {

      describe(description, () => {
        beforeEach(() => {
          configure(projectList);
          httpClient = TestBed.get(HttpClient);
          httpMock = TestBed.get(HttpTestingController);
          chefSession = TestBed.get(ChefSessionService);
          spyOnProperty(chefSession, 'id_token', 'get').and.returnValue('token');
        });

        it('includes checked projects', done => {
          httpClient.get('/endpoint').subscribe(done);

          const httpRequest = httpMock.expectOne('/endpoint');
          httpRequest.flush('response');

          expect(httpRequest.request.headers.keys()).toContain('projects');
          expect(httpRequest.request.headers.get('projects'))
            .toEqual(projectList.filter(p => p.checked).map(p => p.value).join(', '));
        });
      });
    });

    using([
      ['with some projects and NONE checked',
        [
          { value: 'proj1', label: 'proj 1', checked: false },
          { value: 'proj2', label: 'proj 2', checked: false },
          { value: 'proj3', label: 'proj 3', checked: false }
        ]],
      ['with NO projects',
        [
        ]]
    ], function (description: string, projectList: ProjectsFilterOption[]) {

      describe(description, () => {

        beforeEach(() => {
          configure(projectList);
          httpClient = TestBed.get(HttpClient);
          httpMock = TestBed.get(HttpTestingController);
          chefSession = TestBed.get(ChefSessionService);
          spyOnProperty(chefSession, 'id_token', 'get').and.returnValue('token');
        });

        it('does not include projects header', done => {
          httpClient.get('/endpoint').subscribe(done);

          const httpRequest = httpMock.expectOne('/endpoint');
          httpRequest.flush('response');

          expect(httpRequest.request.headers.keys()).not.toContain('projects');
        });
      });
    });

    describe('unfiltered flag', () => {
      beforeEach(() => {
        const projectList = [
          { value: 'proj1', label: 'proj 1', checked: true },
          { value: 'proj2', label: 'proj 2', checked: false },
          { value: 'proj3', label: 'proj 3', checked: true },
          { value: 'proj4', label: 'proj 4', checked: false }
        ];
        configure(projectList);
        httpClient = TestBed.get(HttpClient);
        httpMock = TestBed.get(HttpTestingController);
        chefSession = TestBed.get(ChefSessionService);
        spyOnProperty(chefSession, 'id_token', 'get').and.returnValue('token');
      });

      using([
        ['does not include project header', true],
        ['includes project header', false]
      ], function (description: string, setting: boolean) {

        it(description + 'project header when unfiltered flag set to ' + setting, done => {
          const options = { params: { unfiltered: String(setting) } };
          httpClient.get('/endpoint', options).subscribe(done);

          const httpRequest = httpMock.expectOne(`/endpoint?unfiltered=${setting}`);
          httpRequest.flush('response');

          const headerKeys = httpRequest.request.headers.keys();
          if (setting) {
            expect(headerKeys).not.toContain('projects');
          } else {
            expect(headerKeys).toContain('projects');
          }
        });

        // Uncomment test after https://github.com/angular/angular/issues/18812 is fixed.
        // it('strips unfiltered param when set to ' + setting, done => {
        //   const options = { params: { unfiltered: String(setting), dummy: 'foobar' } };
        //   httpClient.get('/endpoint', options).subscribe(done);

        //   const httpRequest = httpMock.expectOne('/endpoint?dummy=foobar');
        //   httpRequest.flush('response');

        //   // This assertion is completely redundant with the expectOne above,
        //   // but having it adds to clarity.
        //   expect(httpRequest.request.params.get('unfiltered')).toBeFalsy();
        // });
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
      })
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
