import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ChefControlMenuComponent } from './chef-control-menu.component';

describe('ChefControlMenuComponent', () => {
  let component: ChefControlMenuComponent;
  let fixture: ComponentFixture<ChefControlMenuComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ ChefControlMenuComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ChefControlMenuComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
