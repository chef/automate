import { Component, CUSTOM_ELEMENTS_SCHEMA, DebugElement } from '@angular/core';
import { By } from '@angular/platform-browser';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';
import { MessageModalComponent } from './message-modal.component';

@Component({
  template: `
<app-message-modal [visible]="visible" [title]="title" (close)="closeModal()">
</app-message-modal>
`
})
class TestHostComponent {

  visible: boolean;
  title: string;

  closeModal() {
    this.visible = false;
  }
}

describe('HostedMessageModalComponent', () => {
  let hostComponent: TestHostComponent;
  let fixture: ComponentFixture<TestHostComponent>;
  let modalComponent: MessageModalComponent;
  let modalElement: DebugElement;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [ TestHostComponent, MessageModalComponent ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TestHostComponent);
    hostComponent = fixture.componentInstance;
    modalComponent = fixture.debugElement.children[0].componentInstance;
    modalElement = fixture.debugElement.query(By.css('app-message-modal'));
  });

  it('should be created', () => {
    expect(hostComponent).toBeTruthy();
    expect(modalComponent).toBeTruthy();
  });

  it('should reflect title when host sets title', () => {
    hostComponent.title = 'my title';
    fixture.detectChanges();
    expect(modalComponent.title).toBe('my title');
    expect(modalElement.attributes['ng-reflect-title']).toBe('my title');
  });

  it('should be visible when host sets visible true', () => {
    hostComponent.visible = true;
    fixture.detectChanges();
    expect(modalComponent.visible).toBe(true);
    expect(modalElement.attributes['ng-reflect-visible']).toBe('true');
  });

  it('should be hidden when host sets visible false', () => {
    hostComponent.visible = false;
    fixture.detectChanges();
    expect(modalComponent.visible).toBe(false);
    expect(modalElement.attributes['ng-reflect-visible']).toBe('false');
  });

  it('should be hidden when selecting close (by method)', () => {
    modalComponent.visible = true;
    modalComponent.closeEvent();
    fixture.detectChanges();
    expect(modalComponent.visible).toBe(false);
  });

  it('should be hidden when selecting close (by DOM)', waitForAsync () => {
    modalComponent.visible = true;
    await fixture.nativeElement.querySelector('chef-button').click();
    fixture.detectChanges();
    expect(modalComponent.visible).toBe(false);
  });

});

describe('MessageModalComponent', () => {
  let component: MessageModalComponent;
  let fixture: ComponentFixture<MessageModalComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MessageModalComponent,
        MockComponent({ selector: 'chef-button' }),
        MockComponent({ selector: 'chef-modal', inputs: ['visible'] })
      ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(MessageModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
