import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ChefServersComponent } from './chef-servers.component';

describe('ChefServersComponent', () => {
  let component: ChefServersComponent;
  let fixture: ComponentFixture<ChefServersComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ ChefServersComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ChefServersComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
