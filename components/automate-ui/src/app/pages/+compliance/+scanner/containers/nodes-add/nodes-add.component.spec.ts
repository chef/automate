import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { Store, StoreModule } from '@ngrx/store';
import { NgrxStateAtom, runtimeChecks, ngrxReducers } from 'app/ngrx.reducers';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { CookieModule } from 'ngx-cookie';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { MockChefSessionService } from 'app/testing/mock-chef-session.service';
import { NodesAddComponent } from './nodes-add.component';
import { MockComponent } from 'ng2-mock-component';
import { AccordionComponent } from 'app/page-components/accordion/accordion.component';
import { AccordionItemComponent } from 'app/page-components/accordion/accordion-item/accordion-item.component';
import { SearchCredentials } from 'app/entities/credentials/credential.actions';

describe('NodesAddComponent', () => {
  let store: Store<NgrxStateAtom>;
  let fixture: ComponentFixture<NodesAddComponent>;
  let component: NodesAddComponent;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        FormsModule,
        ReactiveFormsModule,
        CookieModule.forRoot(),
        HttpClientTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      declarations: [
        NodesAddComponent,
        AccordionComponent,
        AccordionItemComponent
      ],
      providers: [
        { provide: ChefSessionService, useClass: MockChefSessionService },
        FeatureFlagsService,
        MockComponent({ selector: 'app-accordion'}),
        MockComponent({ selector: 'app-accordion-item', inputs: ['title']}),
        MockComponent({ selector: 'app-selectbox', inputs: [
          'data', 'searchFlag', 'combination', 'scrollLoadingRightSide',
          'uniqueFiledName', 'typeValue', 'typeFieldName'
        ], outputs: ['searchData', 'selectData', 'onScrollListData']}),
        MockComponent({ selector: 'chef-select' }),
        MockComponent({ selector: 'chef-option' })
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });
    store = TestBed.inject(Store);
    spyOn(store, 'dispatch').and.callThrough();
    fixture = TestBed.createComponent(NodesAddComponent);
    component = fixture.componentInstance;
  });

  beforeEach(() => {
    component.ngOnInit();
  });

  describe('navToStep()', () => {
    it('sets activeStep to provided number', () => {
      component.activeStep = 1;
      component.navToStep(2);
      expect(component.activeStep).toEqual(2);
    });
  });

  describe('stepIsActive()', () => {
    it('returns boolean for active state of provided step', () => {
      component.activeStep = 1;
      expect(component.stepIsActive(1)).toEqual(true);
      expect(component.stepIsActive(2)).toEqual(false);
    });
  });

  describe('stepIsValid()', () => {
    it('returns boolean for form group validity of provided step', () => {
      expect(component.stepIsValid(1))
        .toEqual(component.form.controls['wizardStep1'].valid);
    });
  });

  describe('Add nodes', () => {
    it('onTypeSelect', () => {
      component.onTypeSelect('ssh');
      expect(component.secretType).toEqual('ssh');
    });

    it('getCredList', () => {
      component.getCredList(true);
      component.searchData = '';
      component.secretType = 'ssh';
      component.pageNumber = 1;
      const data = {
        filters:  [
          {
              key: 'name',
              values: ['']
          },
          {
              key: 'type',
              values: ['ssh']
          }
      ],
      page: 1,
      per_page: 100
      };
      expect(store.dispatch).toHaveBeenCalledWith(new SearchCredentials(data));
      expect(component.isScroll).toEqual(true);
    });

    it('search', () => {
      component.search('data');
      expect(component.searchData).toEqual('data');
      expect(component.pageNumber).toEqual(1);
    });

    it('search', () => {
      component.search('data');
      expect(component.searchData).toEqual('data');
    });

  });
});
