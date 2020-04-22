import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ResourceDropdownComponent } from './resource-dropdown.component';

describe('ResourceDropdownComponent', () => {
  let component: ResourceDropdownComponent;
  let fixture: ComponentFixture<ResourceDropdownComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ ResourceDropdownComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ResourceDropdownComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
