import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';

import { ChefControlMenuComponent } from './chef-control-menu.component';

describe('ChefControlMenuComponent', () => {
  let component: ChefControlMenuComponent;
  let fixture: ComponentFixture<ChefControlMenuComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({'selector': 'chef-dropdown'}),
        MockComponent({'selector': 'chef-icon'}),
        ChefControlMenuComponent
      ]
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
