import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';

import { TopNavLandingComponent } from './top-nav-landing.component';

describe('TopNavLandingComponent', () => {
  let component: TopNavLandingComponent;
  let fixture: ComponentFixture<TopNavLandingComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [
        TopNavLandingComponent,
        MockComponent({ selector: 'app-landing', inputs: ['routePerms'] })
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TopNavLandingComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
