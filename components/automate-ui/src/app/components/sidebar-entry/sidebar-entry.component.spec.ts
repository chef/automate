import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';
import { MockChefIcon } from 'app/testing/mock-components';
import { SidebarEntryComponent } from './sidebar-entry.component';

describe('SidebarEntryComponent', () => {
  let component: SidebarEntryComponent;
  let fixture: ComponentFixture<SidebarEntryComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        SidebarEntryComponent
      ],
      imports: [
        MockChefIcon,
        MockComponent({
          selector: 'a',
          inputs: [ 'routerLink', 'routerLinkActiveOptions' ]
        })
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(SidebarEntryComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
