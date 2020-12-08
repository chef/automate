import { CUSTOM_ELEMENTS_SCHEMA, Type } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { Router } from '@angular/router';
import { MockComponent } from 'ng2-mock-component';
import { environment as env } from 'environments/environment';
import { StoreModule, Store } from '@ngrx/store';
import {
  NgrxStateAtom,
  ngrxReducers,
  defaultInitialState,
  runtimeChecks,
  defaultRouterState,
  defaultRouterRouterState
} from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { GetCookbookDetailsSuccess } from 'app/entities/cookbooks/cookbook-details.actions';
import { CookbookDetails } from 'app/entities/cookbooks/cookbook-details.model';
import { GetCookbookVersionsSuccess } from 'app/entities/cookbooks/cookbook-versions.actions';
import { CookbookVersions } from 'app/entities/cookbooks/cookbook-versions.model';
import { CookbookDetailsComponent } from './cookbook-details.component';


const declarations: any[] = [
  MockComponent({ selector: 'chef-heading' }),
  MockComponent({ selector: 'chef-icon' }),
  MockComponent({ selector: 'chef-loading-spinner' }),
  MockComponent({ selector: 'mat-select' }),
  MockComponent({ selector: 'mat-option' }),
  MockComponent({ selector: 'chef-page-header' }),
  MockComponent({ selector: 'chef-subheading' }),
  MockComponent({ selector: 'chef-toolbar' }),
  MockComponent({ selector: 'a', inputs: ['routerLink'] }),
  MockComponent({ selector: 'input', inputs: ['resetOrigin'] }),
  CookbookDetailsComponent
];
const server_id = '6e98f609-586d-4816-a6de-e841e659b11d';
const org_id = '6e98f609-586d-4816-a6de';
const cookbook_name = 'aix';
const current_version = '1.1.1';
const readme_content = 'test content';
const cookbookVersion: CookbookVersions = {
  name: 'aix',
  versions: []
};
const cookbookDetails: CookbookDetails = {
  cookbook_name: 'aix',
  name_and_version: 'aix_1.1.1',
  version: '1.1.1',
  chef_type: 'any',
  frozen: 'any',
  json_class: '1',
  files: [],
  templates: [],
  attributes: [],
  recipes: [],
  definitions: [],
  libraries: [],
  providers: [],
  resources: [],
  root_files: [{
    checksum: 'bdfe7faf',
    name: 'README.md',
    path: 'README.md',
    specificity: 'default',
    url: 'https://ec2-automate-infra-view-test.com'
  }],
  metadata: [],
  access: []
};

describe('CookbookDetailsComponent', () => {
  let component: CookbookDetailsComponent;
  let fixture: ComponentFixture<CookbookDetailsComponent>;
  let router: Router;
  let store: Store<NgrxStateAtom>;
  let httpMock: HttpTestingController;

  const initialState = {
    ...defaultInitialState,
    router: {
      ...defaultRouterState,
      state: {
        ...defaultRouterRouterState,
        url: `infrastructure/chef-servers/${server_id}/org/${org_id}/cookbooks/${cookbook_name}`,
        params: { id: server_id, 'org-id': org_id, 'cookbook-name': cookbook_name }
      }
    }
  };

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: declarations,
      providers: [
        FeatureFlagsService
      ],
      imports: [
        RouterTestingModule,
        HttpClientTestingModule,
        StoreModule.forRoot(ngrxReducers, { initialState, runtimeChecks })
      ],
      schemas: [CUSTOM_ELEMENTS_SCHEMA]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    router = TestBed.inject(Router);
    spyOn(router, 'navigate').and.stub();
    store = TestBed.inject(Store);

    fixture = TestBed.createComponent(CookbookDetailsComponent);
    httpMock = fixture.debugElement.injector
      .get<HttpTestingController>(HttpTestingController as Type<HttpTestingController>);
    component = fixture.componentInstance;
    component.currentVersion = '1.1.1';
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  it('defaults to showing content section', () => {
    expect(component.tabValue).toBe('content');
  });

  it('Check cookbook version success', () => {
    store.dispatch(new GetCookbookVersionsSuccess(cookbookVersion));
    fixture.detectChanges();
    expect(component.cookbook).toEqual(cookbookVersion);
  });

  it('Check cookbook details success', () => {
    store.dispatch(new GetCookbookDetailsSuccess(cookbookDetails));
    fixture.detectChanges();
    expect(component.cookbookDetails).toEqual(cookbookDetails);
  });

  it('Remote HTTP call when README file URL exists', () => {
    store.dispatch(new GetCookbookDetailsSuccess(cookbookDetails));
    fixture.detectChanges();
    const fileUrl = encodeURIComponent(cookbookDetails.root_files[0].url);
    const req = httpMock.expectOne(`${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/cookbooks/${cookbook_name}/${current_version}/file-content?url=${fileUrl}`);
    expect(req.request.method).toBe('GET');
    httpMock.verify();
    req.flush(readme_content);
    expect(component.readFileContent).toEqual(readme_content);
  });

  it('No remote HTTP call triggered when README file URL does not exist', () => {
    store.dispatch(new GetCookbookDetailsSuccess(cookbookDetails));
    fixture.detectChanges();
    const req = httpMock.expectNone(`${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/cookbooks/${cookbook_name}/${current_version}/file-content?url=`);
    expect(req).toBeUndefined();
  });

});

