class RangeSlider extends HTMLElement {
  connectedCallback() {
    const input = document.createElement("input");
    this.appendChild(input);
  }
}

globalThis.customElements.define("range-slider", RangeSlider);