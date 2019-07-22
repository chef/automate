import { newE2EPage } from '@stencil/core/testing';

describe('chef-snippet', () => {
  const code = '<h1>Example markup</h1>';
  const lang = 'html';
  const html = `<chef-snippet code='${code}' lang='${lang}'></chef-snippet>`;

  it('renders', async () => {
    const page = await newE2EPage();

    await page.setContent(html);
    const element = await page.find('chef-snippet');
    expect(element).toHaveClass('hydrated');
  });

  it('displays highlighted code', async () => {
    const page = await newE2EPage();

    await page.setContent(html);
    const element = await page.find('chef-snippet');
    expect(element.textContent).toEqual('<h1>Example markup</h1>');
  });

  describe('when `code` prop is empty', () => {
    it('displays nothing', async () => {
      const page = await newE2EPage();
      const snippet = `<chef-snippet code='' lang='${lang}'></chef-snippet>`;

      await page.setContent(snippet);

      const content = await page.find('pre');
      expect(content).toBeFalsy();
    });
  });
});
