import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { CookbooksListComponent } from './cookbooks-list.component';

xdescribe('CookbooksListComponent', () => {
  let component: CookbooksListComponent;
  let fixture: ComponentFixture<CookbooksListComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ CookbooksListComponent ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CookbooksListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
