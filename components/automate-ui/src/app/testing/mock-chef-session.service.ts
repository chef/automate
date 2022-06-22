import { IDToken } from 'app/helpers/jwt/jwt';

export class MockChefSessionService {
  fullname = 'Test Mock';
  username = 'testmock';
  email = 'testmock@domain.com';
  uuid = 'test_subject';
  id_token = 'test_id_token';
  telemetry_enabled = true;
  isLocalUser = true;

  public logout(): void {}
  public setSession(): void {}
  public deleteSession(): void {}
  public userWelcomeModalSeenKey(): string {
    return 'test_subject-welcome-modal-seen';
  }
  public fetchTelemetryPreference(): boolean {
    return true;
  }

  public isLocalUserFromId(_id: IDToken): boolean {
    return true;
  }
}
export class MockChefSessionServiceEnabledUndefined {
  fullname = 'Test Mock';
  email = 'testmock@domain.com';
  uuid = 'test_subject';

  get id_token(): string {
    return 'test_id_token';
  }

  get telemetry_enabled(): boolean {
    return undefined;
  }

  public logout(): void {}
  public setSession(): void {}
  public deleteSession(): void {}
}
