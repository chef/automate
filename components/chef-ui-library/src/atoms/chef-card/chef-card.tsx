import { Component, h } from '@stencil/core';

/**
 * @description
 * Simple atom to wrap content in chef styled "card". Provides a slot named 'label' to provide
 * a label for the card. Labels can be one or multiple elements and can be styled as necessary.
 *
 * @example
 * <chef-card>
 *   <span style='display:block; padding: 1em 0;' slot='label'>This is the card's label</span>
 *   <p>Once upon a midnight dreary, while I pondered, weak and weary. Over many a
 *      quaint and curious volume of forgotten lore. While I nodded, nearly napping,
 *      suddenly there came a tapping. As of some one gently rapping, rapping at my
 *      chamber door. 'Tis some visiter, I muttered, tapping at my chamber door. Only
 *      this, and nothing more. Ah, distinctly I remember it was in the bleak December.
 *      And each separate dying ember wrought its ghost upon the floor. Eagerly I wished
 *      the morrow;—vainly I had sought to borrow. From my books surcease of
 *      sorrow—sorrow for the lost Lenore. For the rare and radiant maiden whom the
 *      angels name Lenore. Nameless here for evermore. And the silken sad uncertain
 *      rustling of each purple curtain Thrilled me, filled me with fantastic terrors</p>
 * </chef-card>
 */
@Component({
  tag: 'chef-card',
  styleUrl: './chef-card.scss'
})
export class ChefCard {

  render() {
    return [
      <slot name="label" />,
      <div class="content">
        <slot />
      </div>
    ];
  }

}
