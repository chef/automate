import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { RouterTestingModule } from '@angular/router/testing';
import { StoreModule, Store } from '@ngrx/store';

import { runtimeChecks, ngrxReducers, NgrxStateAtom } from 'app/ngrx.reducers';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { IntegrationsListComponent } from './integrations-list.component';
import { MatOptionSelectionChange } from '@angular/material/core/option';
import { DeleteManager } from 'app/entities/managers/manager.actions';

describe('IntegrationsListComponent', () => {
  let component: IntegrationsListComponent;
  let fixture: ComponentFixture<IntegrationsListComponent>;
  let store: Store<NgrxStateAtom>;
  const MockSelectionEvent: MatOptionSelectionChange = { isUserInput: true, source: null };

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        ChefPipesModule,
        StoreModule.forRoot({
          ...ngrxReducers,
          router: () => ({
            state: {
              url: '/',
              queryParams: {},
              params: {},
              fragment: '',
              path: ['/']
            },
            previousRoute: {},
            navigationId: 0
          })
        }, { runtimeChecks })
      ],
      declarations: [
        IntegrationsListComponent
      ],
      providers: [
        FeatureFlagsService
      ],
      schemas: [
        CUSTOM_ELEMENTS_SCHEMA
      ]
    })
    .compileComponents();
    store = TestBed.inject(Store);
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(IntegrationsListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('exists', () => {
    expect(component).toBeTruthy();
  });

  describe('deleting a node manager', () => {
    it('displays a modal prompt when user selects Delete Integration from the dropdown', () => {
      expect(component.deletePromptVisible).toBe(false);
      component.beginDelete(MockSelectionEvent, '12345');

      expect(component.deletePromptVisible).toBe(true);
    });

    it('stores the id of the node manager to be deleted', () => {
      expect(component.managerIdForDeletion).toBe('');

      component.beginDelete(MockSelectionEvent, '12ebd34');
      expect(component.managerIdForDeletion).toBe('12ebd34');
    });

    it('deletes the selected node manaager', () => {
      spyOn(store, 'dispatch');
      component.managerIdForDeletion = 'abcde';
      component.handleDelete();

      expect(store.dispatch).toHaveBeenCalledWith(new DeleteManager({ id: 'abcde' }));
    });
  });
});
