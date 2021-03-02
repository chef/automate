import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { InfraTabComponent } from './infra-tab.component';

describe('InfraTabComponent', () => {
  let component: InfraTabComponent;
  let fixture: ComponentFixture<InfraTabComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [ InfraTabComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(InfraTabComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
