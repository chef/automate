import { ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { WarningBannerComponent } from './warning-banner.component';
import { AppConfigService } from 'app/services/app-config/app-config.service';


describe('WarningBannerComponent', () => {
  let component: WarningBannerComponent;
  let fixture: ComponentFixture<WarningBannerComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [WarningBannerComponent],
      imports: [HttpClientTestingModule],
      providers: [AppConfigService]
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(WarningBannerComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeDefined();
  });
});
