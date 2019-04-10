import { Component, Prop, Watch } from '@stencil/core';

@Component({
  tag: 'chef-logo',
  styleUrl: 'chef-logo.scss'
})
export class ChefLogo {
  @Prop() company = 'Unknown';

  companyData = new Map([
    [ 'Chef Software', 'chef-software.svg' ],
    [ 'Slack', 'slack.svg' ],
    [ 'Unknown', 'Unknown.svg' ],
  ]);

  @Watch('company')
  validateCompany(newValue: string, _oldValue: string) {
    const company_entry = this.companyData.get(newValue);
    if (! company_entry) { throw new Error('The supplied name "' + newValue + '" is invalid!'); }
  }

  render() {
    let image;
    let company_name;
    if (this.companyData.has(this.company)) {
      company_name = this.company;
      image = this.companyData.get(this.company);
    } else {
      company_name = 'Unknown';
      image = this.companyData.get('Unknown');
    }
    return (
      <img alt={company_name + ' logo'} src={'/assets/logos/' + image} />
    );
  }
}
