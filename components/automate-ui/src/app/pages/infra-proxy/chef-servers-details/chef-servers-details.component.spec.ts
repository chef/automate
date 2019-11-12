import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ChefServersDetailsComponent } from './chef-servers-details.component';

describe('ChefServersDetailsComponent', () => {
  let component: ChefServersDetailsComponent;
  let fixture: ComponentFixture<ChefServersDetailsComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ ChefServersDetailsComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ChefServersDetailsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
