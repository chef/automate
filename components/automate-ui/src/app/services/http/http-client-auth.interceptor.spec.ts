import { HTTP_INTERCEPTORS, HttpClient } from '@angular/common/http';
import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { StoreModule } from '@ngrx/store';

import { ChefSessionService } from '../chef-session/chef-session.service';
import {
  ProjectsFilterState, ProjectsFilterOption
} from '../projects-filter/projects-filter.reducer';
import { HttpClientAuthInterceptor } from './http-client-auth.interceptor';

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

    describe('with some projects', () => {
      const projectsList = ['proj1', 'proj2', 'proj3'];

      beforeEach(() => {
        configure(projectsList.map(p => <ProjectsFilterOption>{ value: p }));
        httpClient = TestBed.get(HttpClient);
        httpMock = TestBed.get(HttpTestingController);
        chefSession = TestBed.get(ChefSessionService);
        spyOnProperty(chefSession, 'id_token', 'get').and.returnValue('token');
      });

      it('includes projects header in all requests', done => {
        httpClient.get('/endpoint').subscribe(done);

        const httpRequest = httpMock.expectOne('/endpoint');
        httpRequest.flush('response');

        expect(httpRequest.request.headers.keys()).toContain('projects');
        expect(httpRequest.request.headers.get('projects')).toEqual(projectsList.join(', '));
      });
    });

    describe('with no projects', () => {
        const projectsList = [];

      beforeEach(() => {
        configure(projectsList.map(p => <ProjectsFilterOption>{ value: p }));
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
