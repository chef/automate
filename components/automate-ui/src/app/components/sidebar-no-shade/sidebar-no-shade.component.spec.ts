import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { SidebarNoShadeComponent } from './sidebar-no-shade.component';

describe('SidebarNoShadeComponent', () => {
  let component: SidebarNoShadeComponent;
  let fixture: ComponentFixture<SidebarNoShadeComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ SidebarNoShadeComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(SidebarNoShadeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
