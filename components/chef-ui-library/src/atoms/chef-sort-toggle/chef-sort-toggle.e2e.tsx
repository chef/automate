import { newE2EPage } from '@stencil/core/testing';

describe('chef-sort-toggle', () => {

  const sort = 'key';
  const order = 'asc';
  const html = `<chef-sort-toggle sort='${sort}' order='${order}'></chef-sort-toggle>`;

  it('renders', async () => {
    const page = await newE2EPage();

    await page.setContent(html);
    const element = await page.find('chef-sort-toggle');
    expect(element).toHaveClass('hydrated');
  });

  describe('toggle()', () => {
    describe('when `order` is `none`', () => {
      it('sets `order` to `asc`', async () => {
        const page = await newE2EPage();

        await page.setContent(html);
        const element = await page.find('chef-sort-toggle');
        element.setProperty('order', 'none');

        await element.callMethod('toggle');

        await page.waitForChanges();

        expect(element.getAttribute('order')).toEqual('asc');
      });
    });

    describe('when `order` is `asc`', () => {
      it('sets `order` to `desc`', async () => {
        const page = await newE2EPage();

        await page.setContent(html);
        const element = await page.find('chef-sort-toggle');
        element.setProperty('order', 'asc');

        await element.callMethod('toggle');

        await page.waitForChanges();

        expect(element.getAttribute('order')).toEqual('desc');
      });
    });

    describe('when `order` is `desc`', () => {
      it('sets `order` to `none`', async () => {
        const page = await newE2EPage();

        await page.setContent(html);
        const element = await page.find('chef-sort-toggle');
        element.setProperty('order', 'desc');

        await element.callMethod('toggle');

        await page.waitForChanges();

        expect(element.getAttribute('order')).toEqual('none');
      });
    });
  });
});
