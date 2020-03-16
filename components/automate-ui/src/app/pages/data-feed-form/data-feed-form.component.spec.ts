import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';
import { Observable, of as observableOf } from 'rxjs';
import { MockComponent } from 'ng2-mock-component';
import { Store, StoreModule } from '@ngrx/store';
import { NgrxStateAtom, runtimeChecks, ngrxReducers } from 'app/ngrx.reducers';

import { Destination } from 'app/pages/data-feed/destination';
import { DatafeedFormComponent } from './data-feed-form.component';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { FeatureFlagsService } from '../../services/feature-flags/feature-flags.service';
import { DatafeedService } from '../../services/data-feed/data-feed.service';

describe('DatafeedFormComponent', () => {
  let store: Store<NgrxStateAtom>;
  let component: DatafeedFormComponent;
  let fixture: ComponentFixture<DatafeedFormComponent>;
  let datafeedService: DatafeedService;
  const snapshot = { params: { name: 'name' }};

  class MockDatafeedService {
    fetchDestinations(): Observable<Destination[]> { return observableOf([]); }
    fetchDestination(destination) { return observableOf(destination); }
    deleteDestination(destination) { return observableOf(destination); }
    createDestination(destination) { return observableOf(destination); }
    editDestination(destination) { return observableOf(destination); }
    testDestinationWithUsernamePassword(_url,
      _username, _password): Observable<Object> { return observableOf({}); }
    testDestinationWithSecretId(_url, _secretId): Observable<Object> { return observableOf({}); }
    testDestinationWithNoCreds(_url): Observable<Object> { return observableOf({}); }
  }

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [
        DatafeedFormComponent,
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-icon' }),
        MockComponent({ selector: 'chef-success-alert' }),
        MockComponent({ selector: 'chef-error-alert' }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        MockComponent({ selector: 'chef-modal', inputs: ['visible'] })
      ],
      providers: [
        { provide: DatafeedService, useClass: MockDatafeedService },
        { provide: ActivatedRoute, useValue: { snapshot: snapshot } },
        FeatureFlagsService
      ],
      imports: [
        FormsModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    }).compileComponents();
      store = TestBed.get(Store);
      spyOn(store, 'dispatch').and.callThrough();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(DatafeedFormComponent);
    component = fixture.componentInstance;
    datafeedService = TestBed.get(DatafeedService);
    fixture.detectChanges();
  });



  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  it('editing a destination should not save username and password', () => {
    const expectedUsername = 'bob';
    const expectedPassword = 'pass';

    spyOn(datafeedService, 'editDestination').and.callThrough();
    component.model.targetUsername = expectedUsername;
    component.model.targetPassword = expectedPassword;
    component.isEditDestination = true;
    component.onSubmit();
    expect(datafeedService.editDestination).
      toHaveBeenCalledWith(jasmine.anything());
  });

  it('testHook with username and password set', () => {
    const expectedUsername = 'bob';
    const expectedPassword = 'pass';
    const expectedUrl = 'url';
    const event: Event = <Event>{
      preventDefault() { }
    };

    spyOn(datafeedService, 'testDestinationWithUsernamePassword').and.callThrough();
    component.model.targetUsername = expectedUsername;
    component.model.targetPassword = expectedPassword;
    component.model.destination.targetUrl = expectedUrl;
    component.testHook(event);
    expect(datafeedService.testDestinationWithUsernamePassword).
      toHaveBeenCalledWith(expectedUrl, expectedUsername, expectedPassword);
  });

  it('testHook with secretsID set', () => {
    const expectedSecretId = 'super_secret';
    const expectedUrl = 'url';
    const event: Event = <Event>{
      preventDefault() { }
    };

    spyOn(datafeedService, 'testDestinationWithSecretId').and.callThrough();
    component.model.destination.targetSecretId = expectedSecretId;
    component.model.destination.targetUrl = expectedUrl;
    component.testHook(event);
    expect(datafeedService.testDestinationWithSecretId).
      toHaveBeenCalledWith(expectedUrl, expectedSecretId);
  });

  it('testHook with no creds set', () => {
    const expectedUrl = 'url';
    const event: Event = <Event>{
      preventDefault() { }
    };

    spyOn(datafeedService, 'testDestinationWithNoCreds').and.callThrough();
    component.model.destination.targetUrl = expectedUrl;
    component.testHook(event);
    expect(datafeedService.testDestinationWithNoCreds).
      toHaveBeenCalledWith(expectedUrl);
  });


});
