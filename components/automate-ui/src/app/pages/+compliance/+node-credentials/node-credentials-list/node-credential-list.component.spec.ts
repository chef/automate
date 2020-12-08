import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { Store, StoreModule } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, defaultInitialState, runtimeChecks } from 'app/ngrx.reducers';
import { MockComponent } from 'ng2-mock-component';

import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { customMatchers } from 'app/testing/custom-matchers';
import { NodeCredentialListComponent } from './node-credential-list.component';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { FormBuilder } from '@angular/forms';
import { NodeCredential, NodeCredentialTypes } from 'app/entities/node-credentials/node-credential.model';
import { MatOptionSelectionChange } from '@angular/material/core';

describe('NodeCredentialListComponent', () => {
  let store: Store<NgrxStateAtom>;
  let component: NodeCredentialListComponent;
  let fixture: ComponentFixture<NodeCredentialListComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'app-authorized', inputs: ['allOf'] }),
        MockComponent({
          selector: 'app-delete-object-modal',
          inputs: ['visible', 'objectNoun', 'objectName', 'moreDetails'],
          outputs: ['close', 'deleteClicked']
        }),
        MockComponent({
          selector: 'app-create-node-credential-modal',
          inputs: ['openEvent', 'sortParams'],
          outputs: []
        }),
        MockComponent({ selector: 'mat-select' }),
        MockComponent({ selector: 'mat-option' }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-table' }),
        MockComponent({
          selector: 'chef-sort-toggle',
          inputs: ['order']
        }),
        MockComponent({ selector: 'chef-button' }),
        MockComponent({ selector: 'chef-toolbar' }),
        MockComponent({ selector: 'chef-tbody' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-thead' }),
        MockComponent({ selector: 'chef-tr' }),
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        NodeCredentialListComponent
      ],
      providers: [
        FormBuilder,
        FeatureFlagsService
      ],
      imports: [
        ChefPipesModule,
        StoreModule.forRoot(ngrxReducers, { initialState: defaultInitialState, runtimeChecks })
      ]
    }).compileComponents();
    store = TestBed.inject(Store);
    spyOn(store, 'dispatch').and.callThrough();
  }));

  beforeEach(() => {
    jasmine.addMatchers(customMatchers);
    fixture = TestBed.createComponent(NodeCredentialListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  describe('when there are no nodeCredentials', () => {
    it('displays no projects', () => {
      component.instanceNodeCredentials$.subscribe(results => {
        expect(results.length).toEqual(0);
      });
    });
  });

  describe('delete modal', () => {
    const mockEvent = { isUserInput: true } as MatOptionSelectionChange;

    it('selecting delete from control menu opens the delete modal', () => {
      expect(component.deleteModalVisible).toBe(false);
      component.startNodeCredentialDelete(mockEvent, genNodeCred('uuid-111'));
      expect(component.deleteModalVisible).toBe(true);
    });

    it('closes upon sending request to back-end', () => {
      component.startNodeCredentialDelete(mockEvent, genNodeCred('uuid-111'));
      expect(component.deleteModalVisible).toBe(true);
      component.deleteNodeCredential();
      expect(component.deleteModalVisible).toBe(false);
    });

    function genNodeCred(id: string): NodeCredential {
      return {
        id,
        name: 'Node Credential',
        type: NodeCredentialTypes.SSH,
        last_modified: '2020-08-24T05:23:24Z',
        tags: [],
        data: [{
          key: 'username',
          value: 'admin'
        }]
      };
    }

  });
});
