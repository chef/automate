import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
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
      beforeEach(() => {
        component.ngOnChanges();
      });

      it('should be on the first page', () => {
        expect(component.page).toEqual(1);
      });

      it('should have an item start count of 1', () => {
        expect(component.itemStartCount).toEqual(1);
      });

      it('should have an item end count of 10', () => {
        expect(component.itemEndCount).toEqual(10);
      });

      it('should display 16 pages to select', () => {
        expect(component.totalPages.length).toEqual(16);
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
        [21, 85, 3, 10, 2, 20],
        [1, 153, 1, 10, 1, 50],
        [151, 153, 16, 10, 4, 50],
        [41, 85, 3, 20, 5, 10],
        [157, 186, 7, 20, 4, 50],
        [61, 85, 3, 20, 2, 50]
      ], function (itemStartCount: number,
                   total: number,
                   currentPageNumber: number,
                   currentPageSize: number,
                   newPageNumber: number,
                   newPageSize: number) {
          it(`when current page is ${currentPageNumber}, and page size is changed emits the calculated page as ${newPageNumber} and page Selection of ${newPageSize}`, () => {
          const mockEvent = { isUserInput: true };
          spyOn(component.pageSizeChanged, 'emit');

          component.itemStartCount = itemStartCount;
          component.total = total;
          component.page = currentPageNumber;
          component.perPage = currentPageSize;

          component.handleSelectPerPageItems(mockEvent, newPageSize);

          expect(component.pageSizeChanged.emit).toHaveBeenCalledWith({
            pageSize: newPageSize,
            updatedPageNumber: newPageNumber
          });
        });
      });
    });
  });

});
