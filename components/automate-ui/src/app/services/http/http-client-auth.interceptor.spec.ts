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
  const projects = ['proj1', 'proj2', 'proj3'];

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpClientTestingModule,
        StoreModule.forRoot({
          projectsFilter: () => <ProjectsFilterState>{
            options: projects.map(p => <ProjectsFilterOption>{ value: p })
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

  it('includes projects in all requests', done => {
    httpClient.get('/endpoint').subscribe(done);

    const httpRequest = httpMock.expectOne('/endpoint');
    httpRequest.flush('response');

    expect(httpRequest.request.headers.get('projects'))
      .toEqual(projects.join(', '));
  });

  describe('when a 401 response is intercepted', () => {
    it('logs out the session', done => {
      spyOn(chefSession, 'logout');
      httpClient.get('/endpoint').subscribe(null, done);

      const httpRequest = httpMock.expectOne('/endpoint');
      httpRequest.flush('response', { status: 401, statusText: 'Not Authorized' });

      expect(chefSession.logout).toHaveBeenCalled();
    });
  });
});
