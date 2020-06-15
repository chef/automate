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

});
