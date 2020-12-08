import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';

import { SelectListItemComponent } from './select-list-item.component';

describe('SelectListItemComponent', () => {
  let component: SelectListItemComponent;
  let fixture: ComponentFixture<SelectListItemComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [ SelectListItemComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(SelectListItemComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
