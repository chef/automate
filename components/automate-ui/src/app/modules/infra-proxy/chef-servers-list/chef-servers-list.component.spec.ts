import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ChefServersListComponent } from './chef-servers-list.component';

xdescribe('ChefServersListComponent', () => {
  let component: ChefServersListComponent;
  let fixture: ComponentFixture<ChefServersListComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ ChefServersListComponent ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ChefServersListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
