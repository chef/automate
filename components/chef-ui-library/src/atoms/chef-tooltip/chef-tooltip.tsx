import { Component, Element, Host, Prop, State, h } from '@stencil/core';
import debounce from 'lodash/fp/debounce';

/**
 * @description
 * chef-tooltip provides a way to create simple tooltips for elements.
 * You can change the background color of the tooltip with the --tooltip-color css variable.
 *
 * @example
 * <chef-tooltip for='fancy-button'>Fancy Button Tooltip</chef-tooltip>
 * <chef-button primary id='fancy-button'>A Button</chef-button>
 *
 * @example
 * <chef-tooltip for='another-button' delay=1000>A Tooltip with a longer delay</chef-tooltip>
 * <chef-button primary id='another-button'>Another Button</chef-button>
 *
 * @example
 * <chef-tooltip for='not-a-button'>A tooltip for something other than a button</chef-tooltip>
 * <div style='padding: 1em;' id='not-a-button'>This is not a button. But it has a tooltip.</div>
 *
 * @example
 * <chef-tooltip for='complex-tooltip'>
 *   <div style='padding: 1em;'>
 *     <span style='color: red;'>Tooltips can even contain markup and styling</span>
 *   </div>
 * </chef-tooltip>
 * <chef-button primary id='complex-tooltip'>Yet another button</chef-button>
 *
 * @example
 * <chef-tooltip style='--tooltip-color: lightblue;' for='fancy-background'>A Tooltip Of A Different Color</chef-tooltip>
 * <chef-button primary id='fancy-background'>Colored Tooltips!</chef-button>
 *
 * @example
 * <chef-tooltip follow for='following-tooltip'>Tooltip that follows</chef-tooltip>
 * <p id='following-tooltip'>
 *    once upon a midnight dreary, while i pondered, weak and weary. over many a
 *    quaint and curious volume of forgotten lore. while i nodded, nearly napping,
 *    suddenly there came a tapping. as of some one gently rapping, rapping at my
 *    chamber door. 'tis some visiter, i muttered, tapping at my chamber door. only
 *    this, and nothing more. ah, distinctly i remember it was in the bleak december.
 *    and each separate dying ember wrought its ghost upon the floor. eagerly i wished
 *    the morrow;—vainly i had sought to borrow. from my books surcease of
 *    sorrow—sorrow for the lost lenore. for the rare and radiant maiden whom the
 *    angels name lenore. nameless here for evermore. and the silken sad uncertain
 *    rustling of each purple curtain thrilled me, filled me with fantastic terrors
 * </p>
 *
 * @example
 * <chef-tooltip offset='-50 0' for='offset-tooltip'>offset tooltip</chef-tooltip>
 * <chef-button primary id='offset-tooltip'>Button</chef-button>
 *
 * @example
 * <chef-tooltip position='left' for='left-tooltip'>Left</chef-tooltip>
 * <chef-button primary id='left-tooltip'>Left</chef-button>
 *
 * @example
 * <chef-tooltip position='right' for='right-tooltip'>Right</chef-tooltip>
 * <chef-button primary id='right-tooltip'>Right</chef-button>
 *
 * @example
 * <chef-tooltip position='bottom' for='bottom-tooltip'>Bottom</chef-tooltip>
 * <chef-button primary id='bottom-tooltip'>Bottom</chef-button>
 *
 * @example
 * <chef-tooltip offset='0 -50' position='left' for='offset-position'>Left offset</chef-tooltip>
 * <chef-button primary id='offset-position'>Left offset</chef-button>
 *
 * @example
 * <chef-tooltip follow for='following-left' position='left'>Tooltip that follows</chef-tooltip>
 * <p id='following-left'>
 *    once upon a midnight dreary, while i pondered, weak and weary. over many a
 *    quaint and curious volume of forgotten lore. while i nodded, nearly napping,
 *    suddenly there came a tapping. as of some one gently rapping, rapping at my
 *    chamber door. 'tis some visiter, i muttered, tapping at my chamber door. only
 *    this, and nothing more. ah, distinctly i remember it was in the bleak december.
 *    and each separate dying ember wrought its ghost upon the floor. eagerly i wished
 *    the morrow;—vainly i had sought to borrow. from my books surcease of
 *    sorrow—sorrow for the lost lenore. for the rare and radiant maiden whom the
 *    angels name lenore. nameless here for evermore. and the silken sad uncertain
 *    rustling of each purple curtain thrilled me, filled me with fantastic terrors
 * </p>
 */
@Component({
  tag: 'chef-tooltip',
  styleUrl: './chef-tooltip.scss'
})
export class ChefTooltip {

  /**
   * The ID of the element to attach the tooltip
   */
  @Prop({ reflectToAttr: true }) for: string;

  /**
   * The delay before the tooltip is displayed in milliseconds.
   */
  @Prop() delay = 100;

  /**
   * The position of the tooltip in relation to the mouse/element.
   * Can be one of `top`, `left`, `right` or `bottom`.
   */
  @Prop() position: 'top' | 'left' | 'right' | 'bottom' = 'top';

  /**
   * The x and y offset of the tooltip in pixels.
   */
  @Prop() offset = '0 0';

  /**
   * If set the tooltip will follow the mouse when it is over the targetted element.
   */
  @Prop() follow = false;

  @State() visible = false;
  @State() _screenPosition = [0, 0];

  set screenPosition([x, y]) {
    const [offsetX, offsetY] = this.offset.split(' ').map((i) => parseInt(i, 10));
    this._screenPosition = [x + (offsetX || 0), y + (offsetY || 0)];
  }

  get screenPosition() {
    return this._screenPosition;
  }

  @Element() el: HTMLElement;

  componentDidLoad() {
    const ref: HTMLElement = document.querySelector(`#${this.for}`);
    if (ref) {
      if (this.follow) {
        this.setupDynamicPositioning(ref);
      } else {
        this.setupStaticPositioning(ref);
      }
    }
  }

  render() {
    const [x, y] = this.screenPosition;
    const styles = { left: `${x}px`, top: `${y}px` };
    const classNames = [
      this.visible ? 'visible' : '',
      this.position,
      this.follow ? 'follow' : ''
    ].join(' ');

    return (
      <Host class={classNames} style={styles}>
        <slot />
      </Host>
    );
  }

  setupDynamicPositioning(ref: HTMLElement) {
    const delayedShow = debounce(this.delay, () => this.visible = true);

    const handleMouseEnter = (event) => {
      this.screenPosition = [event.clientX, event.clientY];
    };

    const handleMouseMove = (event) => {
      this.screenPosition = [event.clientX, event.clientY];
      delayedShow();
    };

    const handleMouseLeave = () => {
      delayedShow.cancel();
      this.visible = false;
    };

    ref.addEventListener('mouseenter', handleMouseEnter);
    ref.addEventListener('mousemove', handleMouseMove);
    ref.addEventListener('mouseleave', handleMouseLeave);
  }

  setupStaticPositioning(ref: HTMLElement) {
    let transitionTimeoutID;
    const transitionDuration = parseFloat(getComputedStyle(this.el).transitionDuration) * 1000;

    const handleMouseEnter = () => {
      clearTimeout(transitionTimeoutID);
      addEventListener('scroll', handleScroll);
    };

    const handleMouseMove = debounce(this.delay, () => {
      this.setStaticPosition(ref);
      this.visible = true;
    });

    const handleScroll = () => this.setStaticPosition(ref);

    const handleMouseLeave = () => {
      handleMouseMove.cancel();
      this.visible = false;
      transitionTimeoutID = setTimeout(handleTransitionComplete, transitionDuration);
    };

    const handleTransitionComplete = () => removeEventListener('scroll', handleScroll);

    ref.addEventListener('mouseenter', handleMouseEnter);
    ref.addEventListener('mousemove', handleMouseMove);
    ref.addEventListener('mouseleave', handleMouseLeave);
  }

  setStaticPosition(ref: HTMLElement) {
    const boundingRect = ref.getBoundingClientRect();
    const top = boundingRect.top;
    const bottom = top + boundingRect.height;
    const left = boundingRect.left;
    const right = left + boundingRect.width;
    const centerXOffset = boundingRect.width / 2;
    const centerYOffset = boundingRect.height / 2;

    switch (this.position) {
      case 'left':
        this.screenPosition = [left, top + centerYOffset];
        break;
      case 'right':
        this.screenPosition = [right, top + centerYOffset];
        break;
      case 'bottom':
        this.screenPosition = [left + centerXOffset, bottom];
        break;
      case 'top':
      default:
        this.screenPosition = [left + centerXOffset, top];
        break;
    }
  }

}
