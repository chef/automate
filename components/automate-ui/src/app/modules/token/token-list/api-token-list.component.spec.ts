import { HttpErrorResponse } from '@angular/common/http';
import { ReactiveFormsModule } from '@angular/forms';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { StoreModule, Store } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { runtimeChecks, ngrxReducers, NgrxStateAtom } from 'app/ngrx.reducers';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { CreateTokenSuccess, CreateTokenFailure } from 'app/entities/api-tokens/api-token.actions';
import { ApiToken } from 'app/entities/api-tokens/api-token.model';
import { ApiTokenListComponent } from './api-token-list.component';

describe('ApiTokenListComponent', () => {
  let component: ApiTokenListComponent;
  let fixture: ComponentFixture<ApiTokenListComponent>;
  let element: HTMLElement;
  let store: Store<NgrxStateAtom>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      imports: [
        ReactiveFormsModule,
        RouterTestingModule,
        ChefPipesModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      providers: [
        FeatureFlagsService
      ],
      declarations: [
        ApiTokenListComponent,
        MockComponent({
          selector: 'chef-toolbar',
          template: '<ng-content></ng-content>'
        }),
        MockComponent({ selector: 'app-authorized',
                        inputs: ['allOf', 'anyOf'],
                        template: '<ng-content></ng-content>' }),
        MockComponent({ selector: 'app-delete-object-modal',
                        inputs: ['default', 'visible', 'objectNoun', 'objectName'],
                        outputs: ['close', 'deleteClicked'] }),
        MockComponent({ selector: 'app-create-object-modal',
                        inputs: ['creating', 'createForm',
                                 'visible', 'objectNoun', 'conflictErrorEvent',
                                 'assignableProjects'],
                        outputs: ['close', 'createClicked'] }),
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-clipboard', inputs: ['value'] }),
        MockComponent({ selector: 'mat-select' }),
        MockComponent({ selector: 'mat-option' }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-table' }),
        MockComponent({ selector: 'chef-thead' }),
        MockComponent({ selector: 'chef-tbody' }),
        MockComponent({ selector: 'chef-tr' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-td' })
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ApiTokenListComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement.nativeElement;
    store = TestBed.inject(Store);
    fixture.detectChanges();
  });

  it('exists', () => {
    expect(component).toBeTruthy();
  });

  describe('create modal', () => {
    it('create modal opens upon clicking create button', () => {
      expect(component.createModalVisible).toBe(false);
      (<HTMLButtonElement>(element.querySelector('#create-button'))).click();
      expect(component.createModalVisible).toBe(true);
    });

    it('opening create modal resets name, id, and projects to empty', () => {
      component.createTokenForm.controls['name'].setValue('any');
      (<HTMLButtonElement>(element.querySelector('#create-button'))).click();
      expect(component.createTokenForm.controls.name.value).toBe(null);
      expect(component.createTokenForm.controls.id.value).toBe(null);
      expect(component.createTokenForm.controls.projects.value).toBe(null);
    });

    it('create token with no policies dispatches action just to create token', () => {
      component.createModalVisible = true;
      spyOn(store, 'dispatch').and.callThrough();
      component.createTokenForm.controls.policies.setValue(null);
      component.createToken();
      expect(store.dispatch).toHaveBeenCalledTimes(1);

      store.dispatch(new CreateTokenSuccess({ 'id': 't1', 'projects': [] } as ApiToken));
      fixture.detectChanges();

      const setupCallsToDispatch = 2;
      expect(store.dispatch).toHaveBeenCalledTimes(setupCallsToDispatch);

    });

    it('successful create token with policies dispatches create plus each add-to-policy', () => {
      const policies = ['p1', 'p2', 'p3', 'p4'];
      component.createModalVisible = true;
      spyOn(store, 'dispatch').and.callThrough();
      component.createTokenForm.controls.policies.setValue(policies);
      component.createToken();
      expect(store.dispatch).toHaveBeenCalledTimes(1);

      store.dispatch(new CreateTokenSuccess({ 'id': 't1', 'projects': [] } as ApiToken));
      fixture.detectChanges();

      const setupCallsToDispatch = 2;
      expect(store.dispatch).toHaveBeenCalledTimes(setupCallsToDispatch + policies.length);
    });

    it('failed create token with policies dispatches create but no add-to-policy', () => {
      const policies = ['p1', 'p2', 'p3', 'p4'];
      component.createModalVisible = true;
      spyOn(store, 'dispatch').and.callThrough();
      component.createTokenForm.controls.policies.setValue(policies);
      component.createToken();
      expect(store.dispatch).toHaveBeenCalledTimes(1);

      store.dispatch(new CreateTokenFailure({} as HttpErrorResponse));
      fixture.detectChanges();

      const setupCallsToDispatch = 2;
      expect(store.dispatch).toHaveBeenCalledTimes(setupCallsToDispatch);
    });
  });
});
