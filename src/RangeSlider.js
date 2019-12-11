class RangeSlider extends HTMLElement {
  connectedCallback() {
    const node = this;
    const input = document.createElement("input");
    node.appendChild(input);
    const jsr = new JSR(input, {
      max: this.max,
      values: [this.val],
      sliders: 1,
      grid: false,
    });
    
    jsr.addEventListener("update", (elem, value) => {
      const e = new CustomEvent("slide", {
        detail: { slideTo: value }
      });

      node.dispatchEvent(e);
    });
  }
}

globalThis.customElements.define("range-slider", RangeSlider);