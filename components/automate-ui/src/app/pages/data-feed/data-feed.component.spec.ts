import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpErrorResponse } from '@angular/common/http';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { StoreModule, Store } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { DataFeedComponent } from './data-feed.component';
import { DestinationRequests } from 'app/entities/destinations/destination.requests';
import { HttpClient, HttpHandler } from '@angular/common/http';
import { Destination } from 'app/entities/destinations/destination.model';
import { CreateDestinationSuccess, CreateDestinationFailure } from 'app/entities/destinations/destination.actions';
import { HttpStatus } from 'app/types/types';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { DataFeedCreateComponent } from '../data-feed-create/data-feed-create.component';

describe('DataFeedComponent', () => {
  let component: DataFeedComponent;
  let fixture: ComponentFixture<DataFeedComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        DataFeedCreateComponent,
        DataFeedComponent,
        MockComponent({
        selector: 'app-create-data-feed-modal',
        inputs: ['visible', 'creating', 'conflictErrorEvent', 'createForm'],
        outputs: ['close', 'createClicked']
        }),
        MockComponent({ selector: 'app-delete-object-modal',
        inputs: ['default', 'visible', 'objectNoun', 'objectName'],
        outputs: ['close', 'deleteClicked'] }),
        MockComponent({ selector: 'chef-button',
                inputs: ['disabled', 'routerLink'] }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-icon' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'mat-select' }),
        MockComponent({ selector: 'mat-option' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-toolbar' }),
        MockComponent({ selector: 'chef-table' }),
        MockComponent({ selector: 'chef-thead' }),
        MockComponent({ selector: 'chef-tbody' }),
        MockComponent({ selector: 'chef-tr' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'a', inputs: ['routerLink'] })
      ],
      providers: [
        FeatureFlagsService,
        DestinationRequests,
        HttpClient,
        HttpHandler
      ],
      imports: [
        FormsModule,
        ReactiveFormsModule,
        RouterTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(DataFeedComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('create data feed', () => {
    let store: Store<NgrxStateAtom>;
    const username = 'test';
    const password = 'test123';
    const token = 'test123';
    const destination = <Destination> {
      id: '1',
      name: 'new data feed',
      secret: 'testSecret',
      url: 'http://foo.com'
    };

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('slidePanel opens slider', () => {
      spyOn(component.createChild, 'slidePanel');
      expect(component.createModalVisible).toBe(false);
      component.slidePanel();
      expect(component.createModalVisible).toBe(true);
    });

    it('on success, closes slider and adds new data feed', () => {
      spyOnProperty(component.createChild, 'saveDone', 'set');

      component.createDataFeedForm.controls['name'].setValue(destination.name);
      component.createDataFeedForm.controls['url'].setValue(destination.url);
      component.createDataFeedForm.controls['username'].setValue(username);
      component.createDataFeedForm.controls['password'].setValue(password);
      component.saveDestination({auth: 'Username and Password', name: 'Service Now'});

      store.dispatch(new CreateDestinationSuccess(destination));
      component.sortedDestinations$.subscribe(destinations => {
        expect(destinations).toContain(destination);
      });
    });

    it('on success, closes slider and adds new data feed access token', () => {
      spyOnProperty(component.createChild, 'saveDone', 'set');

      component.createDataFeedForm.controls['name'].setValue(destination.name);
      component.createDataFeedForm.controls['url'].setValue(destination.url);
      component.createDataFeedForm.controls['tokenType'].setValue('Bearer');
      component.createDataFeedForm.controls['token'].setValue(token);

      component.saveDestination({auth: 'Access Token', name: 'Service Now'});

      store.dispatch(new CreateDestinationSuccess(destination));
      component.sortedDestinations$.subscribe(destinations => {
        expect(destinations).toContain(destination);
      });
    });

    it('on conflict error, slider is open with conflict error', () => {
      spyOnProperty(component.createChild, 'saveDone', 'set');
      spyOn(component.conflictErrorEvent, 'emit');
      spyOn(component.createChild, 'slidePanel');

      component.slidePanel();
      component.createDataFeedForm.controls['name'].setValue(destination.name);
      component.createDataFeedForm.controls['url'].setValue(destination.url);
      component.createDataFeedForm.controls['username'].setValue(username);
      component.createDataFeedForm.controls['password'].setValue(password);
      component.saveDestination({auth: 'Username and Password', name: 'Service Now'});

      const conflict = <HttpErrorResponse>{
        status: HttpStatus.CONFLICT,
        ok: false
      };
      store.dispatch(new CreateDestinationFailure(conflict));

      expect(component.createModalVisible).toBe(true);
      expect(component.conflictErrorEvent.emit).toHaveBeenCalled();
    });

    it('on create error, slider is closed with failure banner', () => {
      spyOnProperty(component.createChild, 'saveDone', 'set');
      spyOn(component.conflictErrorEvent, 'emit');
      spyOn(component.createChild, 'slidePanel');

      component.slidePanel();
      component.createDataFeedForm.controls['name'].setValue(destination.name);
      component.createDataFeedForm.controls['url'].setValue(destination.url);
      component.createDataFeedForm.controls['username'].setValue(username);
      component.createDataFeedForm.controls['password'].setValue(password);
      component.saveDestination({auth: 'Username and Password', name: 'Service Now'});

      const error = <HttpErrorResponse>{
        status: HttpStatus.INTERNAL_SERVER_ERROR,
        ok: false
      };
      store.dispatch(new CreateDestinationFailure(error));

      expect(component.createModalVisible).toBe(true);
      expect(component.conflictErrorEvent.emit).toHaveBeenCalledWith(false);
    });

  });

});
