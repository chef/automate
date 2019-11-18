import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ControlMenuComponent } from './control-menu.component';

describe('ControlMenuComponent', () => {
  let component: ControlMenuComponent;
  let fixture: ComponentFixture<ControlMenuComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ ControlMenuComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ControlMenuComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
