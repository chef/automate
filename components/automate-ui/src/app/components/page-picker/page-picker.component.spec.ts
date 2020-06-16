import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ComponentFixture, TestBed, tick } from '@angular/core/testing';
import { StoreModule, Store } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { PagePickerComponent } from './page-picker.component';
import { using } from 'app/testing/spec-helpers';

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

    beforeEach(() => {
      component.forDesktop = true;
      component.page = 1;
      component.total = 153;
      component.perPage = 10;
    });

    describe('initial load', () => {

      it('should be on the first page', () => {
        expect(component.page).toEqual(1);
      });

      it('should have an item start count of 1', () => {
        component.getItemStartCount();
        expect(component.itemStartCount).toEqual(1);
      });

      xit('should have an item end count of 10', () => {
        component.getItemEndCount();
        // needs to use the pipe or will get total
        expect(component.itemEndCount).toEqual(10);
      });

      it('should display 10 items per page', () => {
        expect(component.perPage).toEqual(10);
      });
    });

    describe('selecting a new page', () => {

      it('emits the newly selected page value', () => {
        const mockEvent = { isUserInput: true };
        spyOn(component.pageChanged, 'emit');

        component.handleSelectItem(mockEvent, 3);

        expect(component.pageChanged.emit).toHaveBeenCalledWith(3);
      });
    });

    describe('selecting number of items to display per page', () => {

      using([
        [1, 10, 1, 50],
        [3, 50, 6, 20]
      ], function (currentPageNumber: number, currentPageSize: number, newPageNumber: number, newPageSize: number) {
          it(`when current page is ${currentPageNumber}, emits the calculated page as ${newPageNumber} and page Selection of ${newPageSize}`, () => {
          const mockEvent = { isUserInput: true };
          spyOn(component.pageSizeChanged, 'emit');

          component.page = currentPageNumber;
          component.perPage = currentPageSize;

          component.handleSelectMaxPageItems(mockEvent, newPageSize);

          expect(component.pageSizeChanged.emit).toHaveBeenCalledWith({
            pageSize: newPageSize,
            updatedPageNumber: newPageNumber
          })
        });
      });
    });
  });

});
