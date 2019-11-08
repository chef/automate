import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ChefOptionComponent } from './chef-option.component';

describe('ChefOptionComponent', () => {
  let component: ChefOptionComponent;
  let fixture: ComponentFixture<ChefOptionComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ ChefOptionComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ChefOptionComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
