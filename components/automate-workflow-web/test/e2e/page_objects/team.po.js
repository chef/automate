export default class TeamPage {

  get teamTitle()  {
    return element(by.css('.sidebar-content-title'));
  }

  get teamDescription() {
    return element(by.css('.team-description'));
  }

  get teamCreator() {
    return element(by.css('.team-creator'));
  }

  get teamModified() {
    return element(by.css('.team-modified'));
  }

  get addMemberForm() {
    return element(by.css('.new-member-form'));
  }

  get addMemberButton() {
    return element(by.buttonText('Add to Team'));
  }

  get addMemberInput() {
    return element(by.css('.team-members-input'));
  }

  get addMemberResultsDropdown() {
    return element(by.css('.new-member-form')).element(by.css('.dropdown-menu'));
  }

  get members() {
    return element.all(by.css('.team-members table tbody tr'));
  }

  get usersInDropdown() {
    return element.all(by.css('.new-member-form .dropdown-menu li'));
  }

  get noTeamMembersMessage() {
    return element(by.css('.team-members .no-members'));
  }
}
