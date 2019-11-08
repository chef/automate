import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ChefDropdownComponent } from './chef-dropdown.component';

describe('ChefDropdownComponent', () => {
  let component: ChefDropdownComponent;
  let fixture: ComponentFixture<ChefDropdownComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ ChefDropdownComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ChefDropdownComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
