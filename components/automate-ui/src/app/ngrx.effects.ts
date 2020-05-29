import { NgModule } from '@angular/core';
import { EffectsModule } from '@ngrx/effects';

import { ApiTokenEffects } from './entities/api-tokens/api-token.effects';
import { AutomateSettingsEffects } from './entities/automate-settings/automate-settings.effects';
import { ClientRunsEffects } from './entities/client-runs/client-runs.effects';
import { CookbookEffects } from './entities/cookbooks/cookbook.effects';
import { CookbookDetailsEffects } from './entities/cookbooks/cookbook-details.effects';
import { CookbookVersionsEffects } from './entities/cookbooks/cookbook-versions.effects';
import { ClientEffects } from './entities/clients/client.effects';
import { CredentialsEffects } from './pages/+compliance/+credentials/credentials.state';
// CredentialEffect is for the credential entities. Don't confuse it with CredentialsEffects.
// CredentialsEffects will be removed when the credentials page is refactored.
import { CredentialEffects } from './entities/credentials/credential.effects';
import { DataBagsEffects } from './entities/data-bags/data-bags.effects';
import { DesktopEffects } from './entities/desktop/desktop.effects';
import { DestinationEffects } from './entities/destinations/destination.effects';
import { EnvironmentEffects } from './entities/environments/environment.effects';
import { EventFeedEffects } from './services/event-feed/event-feed.effects';
import { InfraRoleEffects } from './entities/infra-roles/infra-role.effects';
import { JobEffects } from './entities/jobs/job.effects';
import { LicenseStatusEffects } from './entities/license/license.effects';
import { ManagerEffects } from './entities/managers/manager.effects';
import { NodesEffects } from './entities/nodes/nodes.effects';
import { OrgEffects } from './entities/orgs/org.effects';
import { PolicyEffects } from './entities/policies/policy.effects';
import { ProfileEffects } from './entities/profiles/profile.effects';
import { ProjectEffects } from './entities/projects/project.effects';
import { ProjectsFilterEffects } from './services/projects-filter/projects-filter.effects';
import { RoleEffects } from './entities/roles/role.effects';
import { RuleEffects } from './entities/rules/rule.effects';
import { ServerEffects } from './entities/servers/server.effects';
import { ServiceGroupsEffects } from './entities/service-groups/service-groups.effects';
import { ScannerEffects } from './pages/+compliance/+scanner/state/scanner.effects';
import { TeamEffects } from './entities/teams/team.effects';
import { UserEffects } from './entities/users/user.effects';
import { UserSelfEffects } from './entities/users/userself.effects';
import { UserPermEffects } from './entities/userperms/userperms.effects';

@NgModule({
  imports: [
    EffectsModule.forRoot([
      ApiTokenEffects,
      AutomateSettingsEffects,
      ClientRunsEffects,
      CookbookEffects,
      CookbookDetailsEffects,
      CookbookVersionsEffects,
      ClientEffects,
      CredentialsEffects,
      CredentialEffects,
      DataBagsEffects,
      DesktopEffects,
      DestinationEffects,
      EnvironmentEffects,
      EventFeedEffects,
      InfraRoleEffects,
      JobEffects,
      LicenseStatusEffects,
      ManagerEffects,
      NodesEffects,
      OrgEffects,
      PolicyEffects,
      ProfileEffects,
      ProjectEffects,
      ProjectsFilterEffects,
      RoleEffects,
      RuleEffects,
      ServerEffects,
      ServiceGroupsEffects,
      ScannerEffects,
      TeamEffects,
      UserEffects,
      UserSelfEffects,
      UserPermEffects
    ])
  ],
  exports: [
    EffectsModule
  ]
})
export class NgrxEffectsModule {}
