import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { StoreModule, Store } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { PagePickerComponent } from './page-picker.component';

describe('PagePickerComponent', () => {
  let component: PagePickerComponent;
  let fixture: ComponentFixture<PagePickerComponent>;
  let store: Store<NgrxStateAtom>;

  beforeEach(async() => {
    TestBed.configureTestingModule({
      declarations: [ PagePickerComponent ],
      imports: [ StoreModule.forRoot(ngrxReducers, { runtimeChecks })],
      providers: [],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    }).compileComponents();
  });

  beforeEach(() => {
    store = TestBed.inject(Store);
    spyOn(store, 'dispatch').and.callThrough();
    fixture = TestBed.createComponent(PagePickerComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('desktop page picker', () => {
    // let desktopStateMock = {
    //   getDesktopsFilter: {
    //     currentPage: 1,
    //     pageSize: 10
    //   }
    // }

    beforeEach(() => {
      component.forDesktop = true;
    });

    describe('initial load', () => {

      xit('should be on the first page', () => {
        expect(component.page).toEqual(1);
      });

      xit('should have an item start count of 1', () => {
        expect(component.itemStartCount).toEqual(1);
      });

      xit('should display 10 items per page', () => {
        expect(component.perPage).toEqual(10);
      });
    });
  });

});
