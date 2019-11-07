import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ChefServersListComponent } from './chef-servers-list.component';

describe('ChefServersListComponent', () => {
  let component: ChefServersListComponent;
  let fixture: ComponentFixture<ChefServersListComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ ChefServersListComponent ]
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
