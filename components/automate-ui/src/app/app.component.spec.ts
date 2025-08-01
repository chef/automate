import { TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { AppComponent } from './app.component';
import { LicenseUsageService } from './services/license-usage/license-usage.service';
import { ComplianceStatsService } from './services/telemetry/compliance-stats/compliance-stats.service';
import { ClientRunsStatsService } from './services/telemetry/client-runs-stats/client-runs-stats.service';
import { ApplicationStatsService } from './services/telemetry/application-stats/application-stats.service';
import { ConfigService } from './services/config/config.service';

class MockComplianceStatsService {
  getCount() { return { subscribe: () => {} }; }
}

class MockClientRunsStatsService {
  getCount() { return { subscribe: () => {} }; }
}

class MockApplicationStatsService {
  getCount() { return { subscribe: () => {} }; }
}

class MockConfigService {
  getConfig() { return { subscribe: () => {} }; }
}

class MockLicenseUsageService {
  postData() {}
}

describe('AppComponent', () => {
  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        HttpClientTestingModule
      ],
      declarations: [
        AppComponent
      ],
      providers: [
        { provide: LicenseUsageService, useClass: MockLicenseUsageService },
        { provide: ComplianceStatsService, useClass: MockComplianceStatsService },
        { provide: ClientRunsStatsService, useClass: MockClientRunsStatsService },
        { provide: ApplicationStatsService, useClass: MockApplicationStatsService },
        { provide: ConfigService, useClass: MockConfigService }
      ],
      schemas: [CUSTOM_ELEMENTS_SCHEMA]
    }).compileComponents();
  });

  it('should create the app', () => {
    const fixture = TestBed.createComponent(AppComponent);
    const app = fixture.componentInstance;
    expect(app).toBeTruthy();
  });

  it(`should have as title 'automate-ui'`, () => {
    const fixture = TestBed.createComponent(AppComponent);
    const app = fixture.componentInstance;
    expect(app.title).toEqual('automate-ui');
  });

  it('should render router outlet', () => {
    const fixture = TestBed.createComponent(AppComponent);
    fixture.detectChanges();
    const compiled = fixture.nativeElement as HTMLElement;
    expect(compiled.querySelector('router-outlet')).toBeTruthy();
  });
});
