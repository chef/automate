import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { StoreModule, Store } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { PagePickerComponent } from './page-picker.component';
import { using } from 'app/testing/spec-helpers';
import { MatOptionSelectionChange } from '@angular/material/core/option';

describe('PagePickerComponent', () => {
  let component: PagePickerComponent;
  let fixture: ComponentFixture<PagePickerComponent>;
  let store: Store<NgrxStateAtom>;
  const MockSelectionEvent: MatOptionSelectionChange = { isUserInput: true, source: null };

  beforeEach(waitForAsync() => {
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
        expect(component.allPages.length).toEqual(16);
      });
    });

    describe('selecting a new page', () => {

      it('emits the newly selected page value', () => {
        spyOn(component.pageChanged, 'emit');
        component.handleSelectItem(MockSelectionEvent, 3);

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
                   totalItems: number,
                   currentPageNumber: number,
                   currentPageSize: number,
                   newPageNumber: number,
                   newPageSize: number) {
          it(`when current page is ${currentPageNumber}, and page size is changed emits the calculated page as ${newPageNumber} and page Selection of ${newPageSize}`, () => {

          spyOn(component.pageSizeChanged, 'emit');

          component.itemStartCount = itemStartCount;
          component.total = totalItems;
          component.page = currentPageNumber;
          component.perPage = currentPageSize;

          component.handleSelectPerPageItems(MockSelectionEvent, newPageSize);

          expect(component.pageSizeChanged.emit).toHaveBeenCalledWith({
            pageSize: newPageSize,
            updatedPageNumber: newPageNumber
          });
        });
      });
    });

    describe('page set calculations', () => {

      using([
        [ 1, 1, 1, [1], 'just one item and one allowed page'],
        [ 1, 3, 1, [1], 'just one item with multiple allowed pages'],
        [10, 3, 1, [1], 'one full page of items'],
        [25, 5, 1, [1, 2, 3], 'multiple pages less than one full view'],
        [46, 5, 1, [1, 2, 3, 4, 5],
          'multiple pages of exactly one full view but last page not full'],
        [50, 5, 1, [1, 2, 3, 4, 5],
          'multiple pages of exactly one full view and last page full'],
        [46, 5, 3, [1, 2, 3, 4, 5],
          'multiple pages of exactly one full view, on middle page'],
        [46, 5, 5, [1, 2, 3, 4, 5],
          'multiple pages of exactly one full view, on last page'],
        [105, 5, 6, [6, 7, 8, 9, 10],
          'visit first page beyond the first full view'],
        [105, 5, 11, [11],
          'visit last view'],
        [105, 3, 1, [1, 2, 3],
          'different pages per view, first view'],
        [105, 3, 4, [4, 5, 6],
          'different pages per view, second view'],
        [105, 3, 10, [10, 11],
          'different pages per view, last view']
      ], function (
        totalItems: number,
        pagesPerView: number,
        currentPage: number,
        expected: number[],
        description: string
        ) {
        it(`with ${description}, expect ${expected}`, () => {
          component.perPage = 10; // just easier for mentally munging the data above
          component.total = totalItems;
          component.maxSelectablePages = pagesPerView;
          component.page = currentPage;

          component.ngOnChanges();

          expect(component.selectablePages).toEqual(expected);
        });
      });
    });
  });

});
