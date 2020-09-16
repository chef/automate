import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { RouterTestingModule } from '@angular/router/testing';
import { StoreModule, Store } from '@ngrx/store';

import { runtimeChecks, ngrxReducers, NgrxStateAtom } from 'app/ngrx.reducers';
import { ChefPipesModule } from '../../../pipes/chef-pipes.module';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { IntegrationsListComponent } from './integrations-list.component';
import { MatOptionSelectionChange } from '@angular/material/core/option';
import { DeleteManager } from 'app/entities/managers/manager.actions';

const mockManager = {
  account_id: '',
  credential_data: [],
  credential_id: 'e1899b64-bc00-41d1-ae18-bdfd36460b11',
  date_added: '2020-09-15T22:34:51Z',
  id: '61f53683-11f8-4a06-99bf-e32afa373eca',
  instance_credentials: [],
  name: 'Duvall',
  status: 'unreachable',
  type: 'aws-ec2'
};

describe('IntegrationsListComponent', () => {
  let component: IntegrationsListComponent;
  let fixture: ComponentFixture<IntegrationsListComponent>;
  let store: Store<NgrxStateAtom>;
  const MockSelectionEvent: MatOptionSelectionChange = { isUserInput: true, source: null };

  beforeEach(async(() => {
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
      component.beginDelete(MockSelectionEvent, mockManager);

      expect(component.deletePromptVisible).toBe(true);
    });

    it('stores the id of the node manager to be deleted', () => {
      expect(component.managerForDeletion).toBe(null);

      component.beginDelete(MockSelectionEvent, mockManager);
      expect(component.managerForDeletion.id).toBe('61f53683-11f8-4a06-99bf-e32afa373eca');
    });

    it('deletes the selected node manaager', () => {
      spyOn(store, 'dispatch');
      component.beginDelete(MockSelectionEvent, mockManager);
      component.handleDelete();

      expect(store.dispatch).toHaveBeenCalledWith(new DeleteManager({ id: '61f53683-11f8-4a06-99bf-e32afa373eca' }));
    });
  });
});
