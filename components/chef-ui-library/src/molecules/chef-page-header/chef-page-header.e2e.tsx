import { newE2EPage } from '@stencil/core/testing';

describe('chef-page-header', () => {
  const html = `
    <chef-page-header>
      <chef-heading>Header text</chef-heading>
      <chef-subheading>Subheader text.</chef-subheading>
    </chef-page-header>
    `;

  it('displays header text', async () => {
    const page = await newE2EPage();
    await page.setContent(html);

    const element = await page.find('h1');
    expect(element.textContent).toEqual('Header text');
  });

  it('displays subheader text', async () => {
    const page = await newE2EPage();
    await page.setContent(html);

    const element = await page.find('p');
    expect(element.textContent).toEqual('Subheader text.');

  });
});
